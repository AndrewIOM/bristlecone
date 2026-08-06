namespace Bristlecone.Integration

/// Module provides functions that 'wrap' a raw integration
/// routine into a form that can be used within Bristlecone.
module Base =

    open Bristlecone
    open Bristlecone.Time
    open Bristlecone.Tensors
    open Bristlecone.EstimationEngine
    open Bristlecone.ModelSystem

    /// Generates a coded map of time-series where all values are NaN.
    let nanResult tInitial tEnd (tStep: float<``time index``>) modelMap =
        let variableCodes = modelMap |> Map.toArray |> Array.unzip |> fst

        let fakeSeries =
            let count = (tEnd - tInitial + 1.<``time index``>) / tStep |> int
            [ 1..count ] |> List.map (fun _ -> nan) |> List.toArray

        variableCodes |> Array.map (fun k -> k, fakeSeries) |> Map.ofArray

    /// Scaffolds an AD-aware function that bakes in the current parameter
    /// set, and only requires the current time and state on any calculation.
    /// Because of the way in which external environment variables are inserted,
    /// the function should only be run at times as specified in tInitial, tEnd,
    /// and tStep.
    let makeCompiledFunctionForIntegration
        (tInitial: float<'modelTime ``time index``>)
        (tEnd: float<'modelTime ``time index``>)
        (tStep: float<'modelTime ``time index``>)
        (externalEnv: CodedMap<TimeIndex.TimeIndex<float<``environment``>, 'date, 'timeunit, 'timespan, 'modelTime>>)
        (initialStateFn: TypedVector<``parameter``> -> CodedMap<TypedScalar<state>>)
        (modelMap: CodedMap<TensorODE>)
        : EstimationEngine.UnparameterisedRHS =

        // STAGE 1. Static scaffolding.

        // Keys & equations
        let modelKeys, modelEqs = modelMap |> Map.toArray |> Array.unzip

        // Precompute external env timeline (constant, non-diff)
        let timeline = [| tInitial..tStep..tEnd |]

        let externalEnvTensors: CodedMap<TypedVector<``environment``>> =
            externalEnv
            |> Map.map (fun _ ti -> timeline |> Array.map (fun t -> ti.[t]) |> Typed.ofVector)

        // Merge helpers (typed, AD-safe)
        let inline injectStatesIntoContext
            (newValues: TypedVector<ModelSystem.state>)
            (newValueKeys: ShortCode.ShortCode[])
            (environment: CodedMap<TypedScalar<ModelSystem.``environment``>>)
            =

            // Extract typed scalars from the vector without converting to float
            let scalars = newValues |> Typed.retypeVector |> Typed.toArray
            let merged = Array.zip newValueKeys scalars |> Map.ofArray

            environment
            |> Map.map (fun k v -> Map.tryFind k merged |> Option.defaultValue v)

        let inline overlayExogenousAtTime
            (timeIdx: int)
            (externalEnv: CodedMap<TypedVector<``environment``>>)
            (currentEnv: CodedMap<TypedScalar<``environment``>>)
            =

            currentEnv
            |> Map.map (fun k v ->
                match Map.tryFind k externalEnv with
                | Some vec -> Typed.itemAt timeIdx vec
                | None -> v)

        let tInitial = tInitial |> Typed.ofScalar
        let tStep = tStep |> Typed.ofScalar

        // Read in environment values from external environment series.
        let baselineEnv =
            Map.keys externalEnvTensors
            |> Seq.map (fun k ->
                match Map.tryFind k externalEnvTensors with
                | Some vec -> k, Typed.itemAt 0 vec
                | None -> failwithf "Could not assign initial value to state / environment %s" k.Value)
            |> Map.ofSeq

        // STAGE 2. Make a parameter-specific concrete RHS.
        fun (parameters: TypedVector<``parameter``>) ->

            // Initial state may be parameter-dependent
            let initialState =
                initialStateFn parameters
                |> Map.map (fun _ v -> Typed.retypeScalar v)

            // The bound RHS now closes over `parameters` but reuses all static prep
            fun (t: TypedScalar<``time index``>) (x: TypedVector<ModelSystem.state>) ->

                let idx = ((t - tInitial) / tStep) |> Typed.toFloatScalar |> Units.floatToInt

                let env =
                    baselineEnv
                    |> overlayExogenousAtTime idx externalEnvTensors
                    |> Map.fold (fun acc k v -> Map.add k v acc) initialState
                    |> injectStatesIntoContext x modelKeys

                // Compute derivatives for all variables
                modelEqs
                |> Array.mapi (fun i m ->
                    let xi = Tensors.Typed.itemAt i x
                    modelKeys.[i], m parameters env t xi)
                |> Map.ofArray


module RungeKutta =

    open Bristlecone
    open Bristlecone.Time
    open Bristlecone.Tensors

    let private two = Typed.ofScalar 2.0
    let private six = Typed.ofScalar 6.0

    let private rk4Core
        (tInitial: TypedScalar<``time index``>)
        (steps: int)
        (dt: TypedScalar<``time index``>)
        (y0: TypedVector<ModelSystem.state>)
        (f: TypedScalar<``time index``> -> TypedVector<ModelSystem.state> -> TypedVector<ModelSystem.state / ``time index``>)
        : TypedMatrix<ModelSystem.state> =

        let mutable t = tInitial
        let mutable y = y0
        let outputs = ResizeArray<TypedVector<'s/``time index``>>()
        outputs.Add y
        let halfDt = dt / two
        let sixthDt = dt / six

        for _ in 1..steps do
            let k1 = f t y
            let k2 = f (t + halfDt) (y + k1 * halfDt)
            let k3 = f (t + halfDt) (y + k2 * halfDt)
            let k4 = f (t + dt) (y + k3 * dt)
            y <- y + sixthDt * (k4 + k1 + k2 * two + k3 * two)
            t <- t + dt
            outputs.Add y

        Typed.stack2D outputs

    let rk4WithStepCount
        (tInitial: TypedScalar<``time index``>)
        (tFinal: TypedScalar<``time index``>)
        (steps: int)
        (y0: TypedVector<'s>)
        (f: TypedScalar<``time index``> -> TypedVector<'s> -> TypedVector<'s / ``time index``>)
        =
        let dt = (tFinal - tInitial) / Typed.ofScalar (float steps)
        rk4Core tInitial steps dt y0 f

    let rk4WithStepWidth
        (tInitial: TypedScalar<``time index``>)
        (tFinal: TypedScalar<``time index``>)
        (dt: TypedScalar<``time index``>)
        (y0: TypedVector<'s>)
        (f: TypedScalar<``time index``> -> TypedVector<'s> -> TypedVector<'s / ``time index``>)
        =
        let steps = Units.floatToInt (((tFinal - tInitial) / dt) |> Typed.toFloatScalar)
        rk4Core tInitial steps dt y0 f

    // Flatten a CodedMap<Scalar> into (keys, Tensor vector)
    let flattenState (stateMap: CodedMap<TypedScalar<ModelSystem.state>>) =
        let keys, vals = stateMap |> Map.toList |> List.unzip
        let vec = vals |> List.toArray |> Typed.stack1D
        keys, vec

    /// Wrap a ParameterisedRHS so it works on a vector rather than
    /// a map of scalars.
    let wrapRhs (keys: ShortCode.ShortCode list) (rhs: EstimationEngine.ParameterisedRHS) =
        let keysArr = keys |> List.toArray
        fun (t: TypedScalar<``time index``>) (y: TypedVector<ModelSystem.state>) ->
            if Typed.length y <> keysArr.Length then
                failwithf "wrapRhs: state length %d does not match keys length %d" (Typed.length y) keysArr.Length
            let resultMap = rhs t y
            keysArr |> Array.map (fun k -> resultMap.[k]) |> Typed.stack1D

    /// Unflatten trajectory Tensor back into ``CodedMap<Vector,state>``.
    /// traj has shape [timeSteps; stateCount].
    let unflattenTrajectory (keys: ShortCode.ShortCode list) (traj: TypedMatrix<'s>) =
        let comps = Typed.unstack2D traj
        (keys, comps)
        ||> Seq.map2 (fun k comp -> k, comp)
        |> Map.ofSeq

    let rk4: EstimationEngine.Integration.IntegrationRoutine =
        fun tInitial tEnd tStep t0 rhs ->
            let keys, y0 = flattenState t0
            let fWrapped = wrapRhs keys rhs
            let traj = rk4WithStepWidth tInitial tEnd tStep y0 fWrapped
            unflattenTrajectory keys traj
