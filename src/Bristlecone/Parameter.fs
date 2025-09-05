namespace Bristlecone

open System

[<RequireQualifiedAccess>]
module Parameter =

    type Constraint =
        | Unconstrained
        | PositiveOnly

    type Estimation<[<Measure>] 'u> =
        | NotEstimated of lowStartingBound: float<'u> * highStartingBound: float<'u>
        | Estimated    of estimate: float<'u>

    type Parameter<[<Measure>] 'u> =
        private Parameter of Constraint * Estimation<'u>

    let private unwrap (Parameter(c, e)) = c, e

    let internal isValid (con: Constraint) (x: float<'u>) =
        let v = float x
        not (Double.IsNaN v) && not (Double.IsInfinity v) && (con <> PositiveOnly || v > 0.0)

    let create con (bound1: float<'u>) (bound2: float<'u>) =
        if isValid con bound1 && isValid con bound2 then
            let lo, hi = min bound1 bound2, max bound1 bound2
            Parameter(con, NotEstimated(lo, hi)) |> Some
        else None

    let isEstimated p =
        match unwrap p with
        | _, Estimated _ -> true
        | _              -> false

    let tryGetEstimate p =
        match unwrap p with
        | _, Estimated v -> Some v
        | _              -> None

    let getEstimate p = tryGetEstimate p |> Option.get

    let bounds (p: Parameter<'u>) =
        let c, est = unwrap p
        match est with
        | Estimated _ -> None
        | NotEstimated (lo, hi) -> Some (lo, hi)

    let internal setRealValue (p: Parameter<'u>) (x: float<'u>) =
        let c, _ = unwrap p
        if isValid c x then Ok (Parameter(c, Estimated x))
        else Error (sprintf "Invalid parameter value %f" (float x))


    [<RequireQualifiedAccess>]
    module ParameterTransforms =

        open DiffSharp
        open Tensors

        type OptimSpaceTransform<[<Measure>] 'space> =
            { Forward : TypedTensor<Scalar,'space>        -> TypedTensor<Scalar,``parameter``>
              Inverse : TypedTensor<Scalar,``parameter``> -> TypedTensor<Scalar,'space> }

        // Detached/bounded mode: identity mapping regardless of constraint
        let scalarTransformOptimSpace (constraint_: Constraint) : OptimSpaceTransform<``optim-space``> =
            { Forward = fun (z: TypedTensor<Scalar,``optim-space``>)        -> z.Value |> tryAsScalar<``parameter``> |> Option.get
              Inverse = fun (x: TypedTensor<Scalar,``parameter``>) -> x.Value |> tryAsScalar<``optim-space``>        |> Option.get }

        // Transformed mode: apply constraint transforms
        let scalarTransformOptimSpaceTransformed (constraint_: Constraint) : OptimSpaceTransform<``optim-space-transformed``> =
            match constraint_ with
            | Constraint.Unconstrained ->
                { Forward = fun (z: TypedTensor<Scalar,``optim-space-transformed``>)        -> z.Value |> tryAsScalar<``parameter``> |> Option.get
                  Inverse = fun (x: TypedTensor<Scalar,``parameter``>) -> x.Value |> tryAsScalar<``optim-space-transformed``>        |> Option.get }
            | Constraint.PositiveOnly ->
                { Forward = fun (z: TypedTensor<Scalar,``optim-space-transformed``>)        -> dsharp.exp z.Value |> tryAsScalar<``parameter``> |> Option.get
                  Inverse = fun (x: TypedTensor<Scalar,``parameter``>) -> dsharp.log x.Value |> tryAsScalar<``optim-space-transformed``>        |> Option.get }


    [<RequireQualifiedAccess>]
    module Pool =

        open DiffSharp
        open Tensors

        type AnyParameter =
            private {
                Name             : string
                ToTensorRealIO   : unit -> Tensor
                FromTensorRealIO : Tensor -> AnyParameter
                GetConstraint      : unit -> Constraint
                TryGetReal       : unit -> float option
                TryGetBounds     : unit -> (float<parameter> * float<parameter>) option
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
                    GetConstraint    = fun () -> let (Parameter(c,_)) = param in c
                    TryGetReal       = fun () -> tryGetEstimate param |> Option.map float
                    TryGetBounds     = fun () -> bounds param |> Option.map (fun (lo, hi) -> float lo |> LanguagePrimitives.FloatWithMeasure<parameter>, float hi |> LanguagePrimitives.FloatWithMeasure<parameter>)
                }
            make p

        type ParameterPool = Pool of CodedMap<AnyParameter>

        let toList  (Pool p) = Map.toList p
        let count   (Pool p) = p.Count
        let fromList xs      = xs |> Map.ofList |> Pool

        /// Given a real-space parameter vector and an existing pool,
        /// return a new pool with each parameter's estimate set to the corresponding value.
        let fromRealVector (realVec: TypedTensor<Vector,``parameter``>) (Pool p: ParameterPool) : ParameterPool =
            let updated =
                p
                |> Map.toList
                |> List.mapi (fun i (sc, ap) ->
                    let value = realVec.Value.[i] |> float |> LanguagePrimitives.FloatWithMeasure<parameter>
                    // Use the AnyParameter's FromTensorRealIO to set the value
                    let newAp = ap.FromTensorRealIO (dsharp.scalar (float value))
                    sc, newAp)
                |> Map.ofList
            Pool updated

        let toTensorWithKeysReal (Pool p)
            : ShortCode.ShortCode[] * TypedTensor<Vector,``parameter``> =
            let keys, scalars =
                p |> Map.toList |> List.map (fun (sc, ap) -> sc, ap.ToTensorRealIO()) |> List.unzip
            let vec =
                scalars |> dsharp.stack
                |> tryAsVector<``parameter``>
                |> Option.defaultWith (fun () -> invalidOp "Pool was not a vector tensor")
            keys |> List.toArray, vec

        type CompiledTransforms<[<Measure>] 'space> =
            { Keys             : ShortCode.ShortCode[]
              IndexByName      : Map<string,int>
              Forward          : TypedTensor<Vector,'space>        -> TypedTensor<Vector,``parameter``>
              Inverse          : TypedTensor<Vector,``parameter``> -> TypedTensor<Vector,'space>
              ScalarTransforms : ParameterTransforms.OptimSpaceTransform<'space>[] }

        /// Compiles forward and inverse transformations between parameter-space (real units)
        /// and optimisation space.
        let internal compileTransformsWith<[<Measure>] 'space>
            (mkScalar: Constraint -> ParameterTransforms.OptimSpaceTransform<'space>)
            (Pool p)
            : CompiledTransforms<'space> =

            let entries = p |> Map.toList
            let keys    = entries |> List.map fst |> List.toArray
            let index   = keys |> Array.mapi (fun i k -> k.Value, i) |> Map.ofArray

            let trans   = entries |> List.map (fun (_, ap) -> mkScalar (ap.GetConstraint())) |> List.toArray

            let forwardVec (thetaOpt: TypedTensor<Vector,'space>) =
                trans
                |> Array.mapi (fun i t ->
                    let zi = asScalar<'space> thetaOpt.Value.[i]
                    let xi = t.Forward zi
                    xi.Value)
                |> dsharp.stack
                |> tryAsVector<``parameter``>
                |> Option.defaultWith (fun () -> invalidOp "Forward produced non-vector")

            let inverseVec (thetaReal: TypedTensor<Vector,``parameter``>) =
                trans
                |> Array.mapi (fun i t ->
                    let xi = asScalar<``parameter``> thetaReal.Value.[i]
                    let zi = t.Inverse xi
                    zi.Value)
                |> dsharp.stack
                |> tryAsVector<'space>
                |> Option.defaultWith (fun () -> invalidOp "Inverse produced non-vector")

            { Keys = keys
              IndexByName = index
              Forward = forwardVec
              Inverse = inverseVec
              ScalarTransforms = trans }

        let internal compileTransformsBounded (pool: ParameterPool) =
            compileTransformsWith<``optim-space``> ParameterTransforms.scalarTransformOptimSpace pool

        let internal compileTransformsTransformed (pool: ParameterPool) =
            compileTransformsWith<``optim-space-transformed``> ParameterTransforms.scalarTransformOptimSpaceTransformed pool


        type OptimiserConfig<[<Measure>] 'space> =
            { Domain      : (float<'space> * float<'space> * Constraint)[]
              Constraints : Constraint list
              Compiled    : CompiledTransforms<'space> }

        and AnyOptimiserConfig =
            | DetachedConfig    of OptimiserConfig<``optim-space``>
            | TransformedConfig of OptimiserConfig<``optim-space-transformed``>

        /// Builds a Domain array from the starting bounds in the pool,
        /// mapping them into optimiser space using the per-parameter scalar transforms.
        let internal buildDomainFromBounds<[<Measure>] 'space>
            (compiled: CompiledTransforms<'space>)
            (pool: ParameterPool)
            : (float<'space> * float<'space> * Constraint)[] =

            pool
            |> toList
            |> List.mapi (fun i (_, ap) ->
                match ap.TryGetBounds() with
                | Some (loReal, hiReal) ->
                    // Convert real-space bounds to optimiser space using scalar transforms
                    let inv = compiled.ScalarTransforms.[i].Inverse
                    let loOpt = inv (Tensors.Typed.ofScalar loReal) |> Tensors.Typed.toFloatScalar
                    let hiOpt = inv (Tensors.Typed.ofScalar hiReal) |> Tensors.Typed.toFloatScalar
                    (loOpt, hiOpt, ap.GetConstraint())

                | None ->
                    // No bounds: use +/- infinity in optimiser space
                    let inf = LanguagePrimitives.FloatWithMeasure<'space> infinity
                    (-inf, inf, ap.GetConstraint()) )
            |> List.toArray

        /// Make a configuration for an optimiser that handles
        /// unit transforms to bounded optimisation space.
        let toOptimiserConfigBounded (pool: ParameterPool) : OptimiserConfig<``optim-space``> =
            let constraints = pool |> toList |> List.map (fun (_, ap) -> ap.GetConstraint())
            let compiled    = compileTransformsBounded pool
            let domainArray = buildDomainFromBounds compiled pool
            { Domain = domainArray; Constraints = constraints; Compiled = compiled }

        /// Make a configuration for an optimiser that handles
        /// unit transforms to unbounded optimisation space.
        /// Transforms are applied where applicable.
        let toOptimiserConfigTransformed (pool: ParameterPool) : OptimiserConfig<``optim-space-transformed``> =
            let compiled    = compileTransformsTransformed pool
            let domainArray = buildDomainFromBounds compiled pool
            { Domain = domainArray; Constraints = []; Compiled = compiled }
