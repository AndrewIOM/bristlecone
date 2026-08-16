namespace Bristlecone

open System

[<RequireQualifiedAccess>]
module Parameter =

    type Constraint<[<Measure>] 'u> =
        | Unconstrained
        | PositiveOnly
        | Bounded of lower: float<'u> * upper: float<'u>

    type Estimation<[<Measure>] 'u> =
        | NotEstimated of lowStartingBound: float<'u> * highStartingBound: float<'u>
        | Estimated of estimate: float<'u>

    type Parameter<[<Measure>] 'u> = private Parameter of Constraint<'u> * Estimation<'u>

    let private unwrap (Parameter(c, e)) = c, e

    let internal isValid (con: Constraint<'u>) (x: float<'u>) =
        if Units.isNotFinite x then
            false
        else
            match con with
            | Unconstrained -> true
            | PositiveOnly -> x > Units.tagUnit 0.
            | Bounded(lo, hi) -> x > lo && x < hi

    let create con (bound1: float<'u>) (bound2: float<'u>) =
        if isValid con bound1 && isValid con bound2 then
            let lo, hi = min bound1 bound2, max bound1 bound2
            Parameter(con, NotEstimated(lo, hi)) |> Some
        else
            None

    let isEstimated p =
        match unwrap p with
        | _, Estimated _ -> true
        | _ -> false

    let tryGetEstimate p =
        match unwrap p with
        | _, Estimated v -> Some v
        | _ -> None

    let getEstimate p = tryGetEstimate p |> Option.get

    let getConstraint p = unwrap p |> fst

    let bounds (p: Parameter<'u>) =
        let c, est = unwrap p

        match est with
        | Estimated _ -> None
        | NotEstimated(lo, hi) -> Some(lo, hi)

    let internal setRealValue (p: Parameter<'u>) (x: float<'u>) =
        let c, _ = unwrap p

        if isValid c x then
            Ok(Parameter(c, Estimated x))
        else
            Error(sprintf "Invalid parameter value %f" (float x))


    [<RequireQualifiedAccess>]
    module ParameterTransforms =

        open Bristlecone.Numerics
        open Bristlecone.Numerics.Typed

        type OptimSpaceTransform<'S,[<Measure>] 'space> =
            { Forward: TypedScalar<'S,'space> -> TypedScalar<'S,``parameter``>
              Inverse: TypedScalar<'S,``parameter``> -> TypedScalar<'S,'space> }

        /// Detached/bounded mode: identity mapping regardless of constraint.
        let scalarTransformOptimSpace: OptimSpaceTransform<'S,``optim-space``> =
            { Forward =
                fun (z: TypedScalar<'S,``optim-space``>) -> z |> Scalar.retype
              Inverse =
                fun (x: TypedScalar<'S,``parameter``>) -> x |> Scalar.retype }

        // Transformed mode: apply constraint transforms
        let scalarTransformOptimSpaceTransformed
            backend
            (cons: Constraint<parameter>)
            : OptimSpaceTransform<'S,``optim-space-transformed``> =
            match cons with
            | Constraint.Unconstrained ->
                { Forward =
                    fun (z: TypedScalar<'S,``optim-space-transformed``>) -> z |> Scalar.retype
                  Inverse =
                    fun (x: TypedScalar<'S,``parameter``>) -> x |> Scalar.retype }
            | Constraint.PositiveOnly ->
                { Forward =
                    fun (z: TypedScalar<'S,``optim-space-transformed``>) ->
                        z |> Scalar.retype |> Scalar.exp |> Scalar.retype
                  Inverse =
                    fun (x: TypedScalar<'S,``parameter``>) ->
                        x |> Scalar.log |> Scalar.retype }
            | Constraint.Bounded(low, hi) ->
                let lowT = Typed.ofScalar backend low
                let hiT = Typed.ofScalar backend hi
                let one = Typed.ofScalar backend 1.

                { Forward =
                    fun (z: TypedScalar<'S,``optim-space-transformed``>) ->
                        let sigma = Scalar.sigmoid (Scalar.retype z)
                        lowT + (hiT - lowT) * sigma
                  Inverse =
                    fun (x: TypedScalar<'S,``parameter``>) ->
                        let sigma = (x - lowT) / (hiT - lowT)
                        let z = Scalar.log (sigma / (one - sigma))
                        z |> Scalar.retype }


    [<RequireQualifiedAccess>]
    module Pool =

        open Bristlecone.Numerics
        open Bristlecone.Numerics.Typed

        type ParameterNoUnit =
            private
                { Name: string
                  ToReal: unit -> float<parameter>
                  FromReal: float<parameter> -> ParameterNoUnit
                  GetConstraint: unit -> Constraint<parameter>
                  TryGetReal: unit -> float option
                  TryGetBounds: unit -> (float<parameter> * float<parameter>) option }

        let private retypeConstraint =
            function
            | Unconstrained -> Unconstrained
            | PositiveOnly -> PositiveOnly
            | Bounded(low, hi) -> Bounded(Units.retype low, Units.retype hi)

        let stripUnit<[<Measure>] 'u> (name: string) (p: Parameter<'u>) : ParameterNoUnit =
            let rec make param =
                { Name = name
                  ToReal =
                    fun () ->
                        match tryGetEstimate param with
                        | Some r -> r |> Units.retype
                        | None -> invalidOp $"Parameter '{name}' has no real estimate"
                  FromReal =
                    fun t ->
                        match setRealValue param (Units.retype t) with
                        | Ok p' -> make p'
                        | Error m -> invalidOp m
                  GetConstraint = fun () -> let (Parameter(c, _)) = param in retypeConstraint c
                  TryGetReal = fun () -> tryGetEstimate param |> Option.map float
                  TryGetBounds =
                    fun () ->
                        bounds param
                        |> Option.map (fun (lo, hi) ->
                            float lo |> LanguagePrimitives.FloatWithMeasure<parameter>,
                            float hi |> LanguagePrimitives.FloatWithMeasure<parameter>) }

            make p

        type ParameterPool = Pool of CodedMap<ParameterNoUnit>

        let toList (Pool p) = Map.toList p
        let count (Pool p) = p.Count

        let keys (Pool p) =
            p |> Map.toList |> List.map (fun (sc, _) -> sc)

        let fromList xs = xs |> Map.ofList |> Pool

        /// Try to get the real value of a parameter by its ShortCode key.
        let tryGetRealValue<[<Measure>] 'u> (name: string) (Pool p: ParameterPool) : float<'u> option =
            p
            |> Map.toSeq
            |> Seq.tryPick (fun (_, ap) ->
                if ap.Name = name then
                    ap.TryGetReal()
                    |> Option.map (fun f -> LanguagePrimitives.FloatWithMeasure<'u> f)
                else
                    None)

        /// Given a real-space parameter vector and an existing pool,
        /// return a new pool with each parameter's estimate set to the corresponding value.
        let fromRealVector (realVec: float<``parameter``>[]) (Pool p: ParameterPool) : ParameterPool =
            let updated =
                p
                |> Map.toList
                |> List.mapi (fun i (sc, ap) ->
                    let value = realVec.[i]
                    let newAp = ap.FromReal value
                    sc, newAp)
                |> Map.ofList

            Pool updated

        let toVectorWithKeysReal backend (Pool p) : ShortCode.ShortCode[] * TypedVector<'S,'V,``parameter``> =
            let keys, scalars =
                p
                |> Map.toList
                |> List.map (fun (sc, ap) -> sc, ap.ToReal())
                |> List.unzip

            let vec =
                scalars
                |> Array.ofList
                |> Array.map (ofScalar backend)
                |> Numerics.Stats.stack1D

            keys |> List.toArray, vec

        let toArrayReal (Pool p) =
            p
            |> Map.toList
            |> List.map (fun (sc, ap) -> sc, ap.ToReal())


        type CompiledTransforms<'S,'V,[<Measure>] 'space> =
            { Keys: ShortCode.ShortCode[]
              IndexByName: Map<string, int>
              Forward: TypedVector<'S,'V,'space> -> TypedVector<'S,'V,``parameter``>
              Inverse: TypedVector<'S,'V,``parameter``> -> TypedVector<'S,'V,'space>
              ScalarTransforms: ParameterTransforms.OptimSpaceTransform<'S,'space>[]
              IsBounded: bool }

        /// Compiles forward and inverse transformations between parameter-space (real units)
        /// and optimisation space.
        let internal compileTransformsWith<'S,'V,[<Measure>] 'space>
            (mkScalar: Constraint<parameter> -> ParameterTransforms.OptimSpaceTransform<'S,'space>)
            isBounded
            (Pool p)
            : CompiledTransforms<'S,'V,'space> =

            let entries = p |> Map.toList
            let keys = entries |> List.map fst |> List.toArray
            let index = keys |> Array.mapi (fun i k -> k.Value, i) |> Map.ofArray

            let trans =
                entries
                |> List.map (fun (_, ap) -> mkScalar (ap.GetConstraint()))
                |> List.toArray

            let forwardVec (thetaOpt: TypedVector<'S,'V,'space>) =
                trans
                |> Array.mapi (fun i t ->
                    let zi = Vector.itemAt i thetaOpt
                    t.Forward zi)
                |> Stats.stack1D

            let inverseVec (thetaReal: TypedVector<'S,'V,``parameter``>) =
                trans
                |> Array.mapi (fun i t ->
                    let xi = Vector.itemAt i thetaReal
                    t.Inverse xi)
                |> Stats.stack1D

            { Keys = keys
              IndexByName = index
              Forward = forwardVec
              Inverse = inverseVec
              ScalarTransforms = trans
              IsBounded = isBounded }

        let internal compileTransformsBounded (pool: ParameterPool) =
            compileTransformsWith<'S,'V,``optim-space``> (fun _ -> ParameterTransforms.scalarTransformOptimSpace) true pool

        let internal compileTransformsTransformed backend (pool: ParameterPool) =
            compileTransformsWith<'S,'V,``optim-space-transformed``>
                (ParameterTransforms.scalarTransformOptimSpaceTransformed backend)
                false
                pool


        type OptimiserConfig<'S,'V,[<Measure>] 'space> =
            { Domain: (float<'space> * float<'space> * Constraint<'space>)[]
              Compiled: CompiledTransforms<'S,'V,'space> }

        and AnyOptimiserConfig<'S,'V> =
            | DetachedConfig of OptimiserConfig<'S,'V,``optim-space``>
            | TransformedConfig of OptimiserConfig<'S,'V,``optim-space-transformed``>

        /// Transform the units of a constraint from parameter (real) units into
        /// optimisation space units.
        let internal transformConstraint<'S,[<Measure>] 'space>
            backend
            (inv: TypedScalar<'S,parameter> -> TypedScalar<'S,'space>)
            (cons: Constraint<parameter>)
            =
            let invF f =
                Typed.ofScalar backend f |> inv |> Scalar.toFloat

            match cons with
            | Unconstrained -> Unconstrained
            | PositiveOnly -> PositiveOnly
            | Bounded(low, hi) -> Bounded(invF low, invF hi)

        /// Builds a Domain array from the starting bounds in the pool,
        /// mapping them into optimiser space using the per-parameter scalar transforms.
        let internal buildDomainFromBounds<'S,'V,[<Measure>] 'space>
            backend
            (compiled: CompiledTransforms<'S,'V,'space>)
            (pool: ParameterPool)
            : (float<'space> * float<'space> * Constraint<'space>)[] =

            pool
            |> toList
            |> List.mapi (fun i (_, ap) ->
                match ap.TryGetBounds() with
                | Some(loReal, hiReal) ->
                    // Convert real-space bounds to optimiser space using scalar transforms
                    let inv = compiled.ScalarTransforms.[i].Inverse
                    let loOpt = inv (Typed.ofScalar backend loReal) |> Scalar.toFloat
                    let hiOpt = inv (Typed.ofScalar backend hiReal) |> Scalar.toFloat

                    let con =
                        if compiled.IsBounded then
                            ap.GetConstraint() |> transformConstraint backend inv
                        else
                            Unconstrained

                    loOpt, hiOpt, con

                | None ->
                    failwith
                        "Unable to generate domain from parameter pool. It may have already been used for estimation.")
            |> List.toArray

        /// Make a configuration for an optimiser that handles
        /// unit transforms to bounded optimisation space.
        let toOptimiserConfigBounded backend (pool: ParameterPool) : OptimiserConfig<'S,'V,``optim-space``> =
            let compiled = compileTransformsBounded pool
            let domainArray = buildDomainFromBounds backend compiled pool

            { Domain = domainArray
              Compiled = compiled }

        /// Make a configuration for an optimiser that handles
        /// unit transforms to unbounded optimisation space.
        /// Transforms are applied where applicable.
        let toOptimiserConfigTransformed backend (pool: ParameterPool) : OptimiserConfig<'S,'V,``optim-space-transformed``> =
            let compiled = compileTransformsTransformed backend pool
            let domainArray = buildDomainFromBounds backend compiled pool

            { Domain = domainArray
              Compiled = compiled }

        /// Draw a random set of parameters in real space within their bounds.
        /// Assumes a uniform distribution for each draw across all parameters.
        let drawRandom (rnd: Random) (Pool p: ParameterPool) : ParameterPool =
            p
            |> Map.map (fun _ ap ->
                match ap.TryGetBounds() with
                | Some(lo, hi) ->
                    let draw = Statistics.Distributions.ContinuousUniform.draw rnd lo hi ()
                    ap.FromReal draw
                | None -> failwithf "Parameter '%s' has no bounds to draw from." ap.Name)
            |> Pool

        /// Create a Pool where all parameters are fixed at their current estimate.
        /// Lower and upper bounds are both set to the estimate.
        let fromEstimated (Pool p: ParameterPool) : ParameterPool =
            let fixd =
                p
                |> Map.map (fun _ ap ->
                    match ap.TryGetReal() with
                    | Some est ->
                        let newParam =
                            create
                                (ap.GetConstraint())
                                (LanguagePrimitives.FloatWithMeasure<parameter> est)
                                (LanguagePrimitives.FloatWithMeasure<parameter> est)
                            |> Option.get
                            |> stripUnit<parameter> ap.Name

                        newParam
                    | None -> failwithf "Could not get estimate for parameter '%s'" ap.Name)

            Pool fixd
