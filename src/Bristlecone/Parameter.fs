namespace Bristlecone

open System

[<RequireQualifiedAccess>]
module Parameter =

    type ConstraintMode =
        | OptimSpace
        | Detached

    type Constraint =
        | Unconstrained
        | PositiveOnly

    type Estimation<[<Measure>] 'u> =
        | NotEstimated of lowStartingBound: float<'u> * highStartingBound: float<'u>
        | Estimated    of estimate: float<'u>

    type Parameter<[<Measure>] 'u> =
        private Parameter of Constraint * ConstraintMode * Estimation<'u>


    [<RequireQualifiedAccess>]
    module ParameterTransforms =

        open DiffSharp
        open Tensors

        type OptimSpaceTransform =
            { Forward : TypedTensor<Scalar,``optim-space``> -> TypedTensor<Scalar,``parameter``>
              Inverse : TypedTensor<Scalar,``parameter``>   -> TypedTensor<Scalar,``optim-space``> }

        /// AD‑safe scalar transforms for a given constraint
        let scalarTransform (constraint_: Constraint) : OptimSpaceTransform =
            match constraint_ with
            | Constraint.Unconstrained ->
                { Forward = fun (z: TypedTensor<Scalar,``optim-space``>) -> z.Value |> tryAsScalar<``parameter``> |> Option.get
                  Inverse = fun (x: TypedTensor<Scalar,``parameter``>) -> x.Value |> tryAsScalar<``optim-space``> |> Option.get }
            | Constraint.PositiveOnly  ->
                { Forward = fun (z: TypedTensor<Scalar,``optim-space``>) -> dsharp.exp z.Value |> tryAsScalar<``parameter``> |> Option.get
                  Inverse = fun (x: TypedTensor<Scalar,``parameter``>) -> dsharp.log x.Value |> tryAsScalar<``optim-space``> |> Option.get }

        let toOptimFloat (constraint_: Constraint) (x: float<'u>) : float<``optim-space``> =
            match constraint_ with
            | Constraint.Unconstrained -> LanguagePrimitives.FloatWithMeasure<``optim-space``> (float x)
            | Constraint.PositiveOnly  -> Units.floatMap log x |> float |> LanguagePrimitives.FloatWithMeasure<``optim-space``>


    let private unwrap (Parameter(c, m, e)) = c, m, e

    let internal isValid (con: Constraint) (x: float<'u>) =
        let v = float x
        not (Double.IsNaN v) && not (Double.IsInfinity v) && (con <> PositiveOnly || v > 0.0)

    let create con (bound1: float<'u>) (bound2: float<'u>) =
        if isValid con bound1 && isValid con bound2 then
            let lo, hi = min bound1 bound2, max bound1 bound2
            Parameter(con, OptimSpace, NotEstimated(lo, hi)) |> Some
        else None

    let isEstimated p =
        match unwrap p with
        | _, _, Estimated _ -> true
        | _                 -> false

    let tryGetEstimate p =
        match unwrap p with
        | _, _, Estimated v -> Some v
        | _                 -> None

    let getEstimate p = tryGetEstimate p |> Option.get

    let bounds p =
        let c, m, est = unwrap p
        match est with
        | Estimated _ -> None
        | NotEstimated (s, e) ->
            match m with
            | Detached   -> Some (s |> Units.removeUnitFromFloat |> LanguagePrimitives.FloatWithMeasure<``optim-space``>, e |> Units.removeUnitFromFloat |> LanguagePrimitives.FloatWithMeasure<``optim-space``>)
            | OptimSpace -> Some (ParameterTransforms.toOptimFloat c s,
                                  ParameterTransforms.toOptimFloat c e)

    let internal setRealValue (p: Parameter<'u>) (x: float<'u>) =
        let c, m, _ = unwrap p
        if isValid c x then Ok (Parameter(c, m, Estimated x))
        else Error (sprintf "Invalid parameter value %f" (float x))

    let detachConstraint (p: Parameter<'u>) : Parameter<'u> * Constraint =
        let c, _, est = unwrap p
        match est with
        | NotEstimated (lo, hi) -> Parameter(c, Detached, NotEstimated(lo, hi)), c
        | Estimated v           -> Parameter(c, Detached, Estimated v), c

    [<RequireQualifiedAccess>]
    module Pool =

        open DiffSharp
        open Tensors

        type AnyParameter =
            private {
                Name             : string
                ToTensorRealIO   : unit -> Tensor
                FromTensorRealIO : Tensor -> AnyParameter
                Transform        : ParameterTransforms.OptimSpaceTransform
                TryGetReal       : unit -> float option
                TryGetBounds     : unit -> (float * float) option
                TryDetach        : unit -> (AnyParameter * Constraint)
            }

        let inline boxParam<[<Measure>] 'u> (name: string) (p: Parameter<'u>) : AnyParameter =
            let rec make param =
                {
                    Name = name
                    ToTensorRealIO   = fun () ->
                        match tryGetEstimate param with
                        | Some r -> dsharp.scalar (float r)
                        | None   -> invalidOp $"Parameter '{name}' has no real estimate"
                    FromTensorRealIO = fun t ->
                        let x = LanguagePrimitives.FloatWithMeasure<'u> (float t)
                        match setRealValue param x with
                        | Ok p'   -> make p'
                        | Error m -> invalidOp m
                    Transform        = ParameterTransforms.scalarTransform (let (Parameter(c,_,_)) = param in c)
                    TryGetReal       = fun () -> tryGetEstimate param |> Option.map float
                    TryGetBounds     = fun () -> bounds param |> Option.map (fun (lo, hi) -> float lo, float hi)
                    TryDetach        = fun () ->
                        let p', extra = detachConstraint param
                        make p', extra
                }
            make p

        type ParameterPool = Pool of CodedMap<AnyParameter>

        let toList  (Pool p) = Map.toList p
        let count   (Pool p) = p.Count
        let fromList xs      = xs |> Map.ofList |> Pool

        /// For reporting, serialisation, or initialisation — not the AD hot path.
        let toTensorWithKeysReal (Pool p)
            : ShortCode.ShortCode[] * TypedTensor<Vector,``parameter``> =
            let keys, scalars =
                p |> Map.toList |> List.map (fun (sc, ap) -> sc, ap.ToTensorRealIO()) |> List.unzip
            let vec =
                scalars |> dsharp.stack
                |> tryAsVector<``parameter``>
                |> Option.defaultWith (fun () -> invalidOp "Pool was not a vector tensor")
            keys |> List.toArray, vec

        /// Pre‑computes everything needed to map whole vectors between spaces efficiently.
        type CompiledTransforms =
            { Keys        : ShortCode.ShortCode[]
              IndexByName : Map<string,int>
              Forward     : TypedTensor<Vector,``optim-space``> -> TypedTensor<Vector,``parameter``>
              Inverse     : TypedTensor<Vector,``parameter``>   -> TypedTensor<Vector,``optim-space``> }

        let compileTransforms (Pool p) : CompiledTransforms =
            let entries    = p |> Map.toList
            let keys       = entries |> List.map fst |> List.toArray
            let index      = keys |> Array.mapi (fun i k -> k.Value, i) |> Map.ofArray
            let forwards   = entries |> List.map (fun (_, ap) -> ap.Transform.Forward)
            let inverses   = entries |> List.map (fun (_, ap) -> ap.Transform.Inverse)

            let forwardVec (thetaOpt: TypedTensor<Vector,``optim-space``>) =
                // For each parameter i, take scalar z_i (typed), apply Forward, then unwrap to Tensor
                let comps =
                    forwards
                    |> List.mapi (fun i f ->
                        let zi = asScalar<``optim-space``> thetaOpt.Value.[i]
                        let xi = f zi
                        xi.Value)
                comps
                |> dsharp.stack
                |> tryAsVector<``parameter``>
                |> Option.defaultWith (fun () -> invalidOp "Forward produced non-vector")

            let inverseVec (thetaReal: TypedTensor<Vector,``parameter``>) =
                let comps =
                    inverses
                    |> List.mapi (fun i g ->
                        let xi = asScalar<``parameter``> thetaReal.Value.[i]
                        let zi = g xi
                        zi.Value)
                comps
                |> dsharp.stack
                |> tryAsVector<``optim-space``>
                |> Option.defaultWith (fun () -> invalidOp "Inverse produced non-vector")

            { Keys = keys; IndexByName = index; Forward = forwardVec; Inverse = inverseVec }
