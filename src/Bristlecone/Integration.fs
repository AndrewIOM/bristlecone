namespace Bristlecone.Integration

/// Module provides functions that 'wrap' a raw integration
/// routine into a form that can be used within Bristlecone.
module Base =

    open Bristlecone
    open Bristlecone.Time
    open Bristlecone.Numerics.Typed
    open Bristlecone.EstimationEngine
    open Bristlecone.ModelSystem

    /// Generates a coded map of time-series where all values are NaN.
    let nanResult tInitial tEnd (tStep: float<``time index``>) modelMap =
        let variableCodes = modelMap |> Map.toArray |> Array.unzip |> fst

        let fakeSeries =
            let count = (tEnd - tInitial + 1.<``time index``>) / tStep |> int
            [ 1..count ] |> List.map (fun _ -> nan) |> List.toArray

        variableCodes |> Array.map (fun k -> k, fakeSeries) |> Map.ofArray

    // Merge helpers (typed, AD-safe)
    let inline injectStatesIntoContext
        (newValues: TypedVector<'S,'V,ModelSystem.state>)
        (newValueKeys: ShortCode.ShortCode[])
        (environment: CodedMap<TypedScalar<'S,ModelSystem.``environment``>>)
        : Map<ShortCode.ShortCode,TypedScalar<'S,environment>>
        =

        // Extract typed scalars from the vector without converting to float
        let scalars = newValues |> Vector.retype |> Vector.toArray
        let merged = Array.zip newValueKeys scalars |> Map.ofArray

        environment
        |> Map.map (fun k v -> Map.tryFind k merged |> Option.defaultValue v)

    let inline overlayExogenousAtTime
        (timeIdx: int)
        (externalEnv: CodedMap<TypedVector<'S,'V,``environment``>>)
        (currentEnv: CodedMap<TypedScalar<'S,``environment``>>)
        =

        currentEnv
        |> Map.map (fun k v ->
            match Map.tryFind k externalEnv with
            | Some vec -> Vector.itemAt timeIdx vec
            | None -> v)

    /// Scaffolds an AD-aware function that bakes in the current parameter
    /// set, and only requires the current time and state on any calculation.
    /// Because of the way in which external environment variables are inserted,
    /// the function should only be run at times as specified in tInitial, tEnd,
    /// and tStep.
    let makeCompiledFunctionForIntegration
        (numBackend: Numerics.NumericEngine<'S,'V,'M>)
        (tInitial: float<'modelTime ``time index``>)
        (tEnd: float<'modelTime ``time index``>)
        (tStep: float<'modelTime ``time index``>)
        (externalEnv: CodedMap<TimeIndex.TimeIndex<float<``environment``>, 'date, 'timeunit, 'timespan, 'modelTime>>)
        (initialStateFn: TypedVector<'S,'V,``parameter``> -> CodedMap<TypedScalar<'S,state>>)
        (modelMap: ModelEquations<'S,'V>)
        : EstimationEngine.UnparameterisedRHS<'S,'V> =

        // STAGE 1. Static scaffolding.

        // Keys & equations
        let modelKeys, modelEqs = modelMap |> Map.toArray |> Array.unzip

        // Precompute external env timeline (constant, non-diff)
        let timeline = [| tInitial..tStep..tEnd |]

        let externalEnvTensors: CodedMap<TypedVector<'S,'V,``environment``>> =
            externalEnv
            |> Map.map (fun _ ti -> timeline |> Array.map (fun t -> ti.[t]) |> ofVector numBackend.vectorBackend numBackend.vectorAccess)

        let tInitial = tInitial |> ofScalar numBackend.scalarBackend
        let tStep = tStep |> ofScalar numBackend.scalarBackend

        // Read in environment values from external environment series.
        let baselineEnv =
            Map.keys externalEnvTensors
            |> Seq.map (fun k ->
                match Map.tryFind k externalEnvTensors with
                | Some vec -> k, Vector.itemAt 0 vec
                | None -> failwithf "Could not assign initial value to state / environment %s" k.Value)
            |> Map.ofSeq

        // STAGE 2. Make a parameter-specific concrete RHS.
        fun (parameters: TypedVector<'S,'V,``parameter``>) ->

            // Initial state may be parameter-dependent
            let initialState =
                initialStateFn parameters
                |> Map.map (fun _ v -> Scalar.retype v)

            // The bound RHS now closes over `parameters` but reuses all static prep
            fun (t: TypedScalar<'S,``time index``>) (x: TypedVector<'S,'V,ModelSystem.state>) ->

                let idx = ((t - tInitial) / tStep) |> Scalar.toFloat |> Units.floatToInt

                let env =
                    baselineEnv
                    |> overlayExogenousAtTime idx externalEnvTensors
                    |> Map.fold (fun acc k v -> Map.add k v acc) initialState
                    |> injectStatesIntoContext x modelKeys

                // Compute derivatives for all variables
                modelEqs
                |> Array.mapi (fun i m ->
                    let xi = Vector.itemAt i x
                    modelKeys.[i], m parameters env t xi)
                |> Map.ofArray


module RungeKutta =

    open Bristlecone
    open Bristlecone.Time
    open Bristlecone.Numerics.Typed
    open Bristlecone.Numerics

    // TODO allocates 2 and 6 on each rk4 run.
    let private rk4Core
        numEngine
        (tInitial: TypedScalar<'S,``time index``>)
        (steps: int)
        (dt: TypedScalar<'S,``time index``>)
        (y0: TypedVector<'S,'V,'su>)
        (f: TypedScalar<'S,``time index``> -> TypedVector<'S,'V,'su> -> TypedVector<'S,'V,'su / ``time index``>)
        : TypedMatrix<'S,'M, 'su> =

        let two = tInitial.Backend.atomic.constants.two |> Typed.Scalar.ofBackend tInitial.Backend
        let six = tInitial.Backend.atomic.constants.six |> Typed.Scalar.ofBackend tInitial.Backend

        let mutable t = tInitial
        let mutable y = y0
        let outputs = ResizeArray<TypedVector<'S,'V,'su>>()
        outputs.Add y
        let halfDt = dt / two
        let sixthDt = dt / six

        for _ in 1..steps do
            let k1 = f t y
            let k2 = f (t + halfDt) (y + k1 * halfDt)
            let k3 = f (t + halfDt) (y + k2 * halfDt)
            let k4 = f (t + dt) (y + k3 * dt)
            y <- y + (k4 + k1 + k2 * two + k3 * two) * sixthDt
            t <- t + dt
            outputs.Add y

        Numerics.Stats.stack2D numEngine outputs

    let rk4WithStepCount
        numEngine
        (tInitial: TypedScalar<'S,``time index``>)
        (tFinal: TypedScalar<'S,``time index``>)
        (steps: int)
        (y0: TypedVector<'S,'V,'s>)
        (f: TypedScalar<'S,``time index``> -> TypedVector<'S,'V,'s> -> TypedVector<'S,'V,'s / ``time index``>)
        =
        let dt = (tFinal - tInitial) / ofScalar tInitial.Backend (float steps)
        rk4Core numEngine tInitial steps dt y0 f

    let rk4WithStepWidth
        numEngine
        (tInitial: TypedScalar<'S,``time index``>)
        (tFinal: TypedScalar<'S,``time index``>)
        (dt: TypedScalar<'S,``time index``>)
        (y0: TypedVector<'S,'V,'s>)
        (f: TypedScalar<'S,``time index``> -> TypedVector<'S,'V,'s> -> TypedVector<'S,'V,'s / ``time index``>)
        =
        let steps = Units.floatToInt (((tFinal - tInitial) / dt) |> Scalar.toFloat)
        rk4Core numEngine tInitial steps dt y0 f

    // Flatten a CodedMap<Scalar> into (keys, Tensor vector)
    let flattenState backend (stateMap: CodedMap<TypedScalar<'S,ModelSystem.state>>) =
        let keys, vals = stateMap |> Map.toList |> List.unzip
        let vec = vals |> List.toArray |> Stats.stack1D backend
        keys, vec

    /// Wrap a ParameterisedRHS so it works on a vector rather than
    /// a map of scalars.
    let wrapRhs numEngine (keys: ShortCode.ShortCode list) (rhs: EstimationEngine.ParameterisedRHS<'S,'V>) =
        let keysArr = keys |> List.toArray
        fun (t: TypedScalar<'S,``time index``>) (y: TypedVector<'S,'V,ModelSystem.state>) ->
            if Vector.length y <> keysArr.Length then
                failwithf "wrapRhs: state length %d does not match keys length %d" (Vector.length y) keysArr.Length
            let resultMap = rhs t y
            keysArr |> Array.map (fun k -> resultMap.[k]) |> Stats.stack1D numEngine

    /// Unflatten trajectory Tensor back into ``CodedMap<Vector,state>``.
    /// traj has shape [timeSteps; stateCount].
    let unflattenTrajectory (keys: ShortCode.ShortCode list) (traj: TypedMatrix<'S,'M,'s>) =
        let comps = Stats.unstack2D traj
        (keys, comps)
        ||> Seq.map2 (fun k comp -> k, comp)
        |> Map.ofSeq

    let rk4: EstimationEngine.Integration.IntegrationRoutine<'S,'V,'M> =
        fun numEngine tInitial tEnd tStep t0 rhs ->
            let keys, y0 = flattenState numEngine t0
            let fWrapped = wrapRhs numEngine keys rhs
            let traj = rk4WithStepWidth numEngine tInitial tEnd tStep y0 fWrapped
            unflattenTrajectory keys traj
