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
        (tInitial       : float<``time index``>)
        (tEnd           : float<``time index``>)
        (tStep          : float<``time index``>)
        (initialEnv     : CodedMap<TypedTensor<Scalar,``environment``>>)
        (externalEnv    : CodedMap<TimeIndex.TimeIndex<float<``environment``>,'date,'timeunit,'timespan>>)
        (modelMap : CodedMap<TensorODE>)
        : EstimationEngine.UnparameterisedRHS =

        // STAGE 1. Static scaffolding.

        // Keys & equations
        let modelKeys, modelEqs = modelMap |> Map.toArray |> Array.unzip

        // Precompute external env timeline (constant, non-diff)
        let timeline = [| tInitial .. tStep .. tEnd |]
        let externalEnvTensors : CodedMap<TypedTensor<Vector,``environment``>> =
            externalEnv
            |> Map.map (fun _ ti ->
                timeline
                |> Array.map (fun t -> ti.[t])
                |> Typed.ofVector)

        // Merge helpers (typed, AD-safe)
        let inline applyDynamicVariablesT
            (newValues: TypedTensor<Vector,ModelSystem.state>)
            (newValueKeys: ShortCode.ShortCode[])
            (environment: CodedMap<TypedTensor<Scalar,ModelSystem.``environment``>>) =

            // Extract typed scalars from the vector without converting to float
            let scalars =
                [| for i in 0 .. newValues.Value.shape.[0] - 1 ->
                    match Tensors.tryAsScalar<``environment``> newValues.Value.[i] with
                    | Some s -> s
                    | None   -> invalidOp "Expected scalar when injecting dynamic variables" |]

            let merged = Array.zip newValueKeys scalars |> Map.ofArray
            environment |> Map.map (fun k v -> Map.tryFind k merged |> Option.defaultValue v)

        let inline applyExternalEnvironmentTensor
            (timeIdx: int)
            (externalEnv: CodedMap<TypedTensor<Vector,``environment``>>)
            (currentEnv: CodedMap<TypedTensor<Scalar,``environment``>>) =

            currentEnv
            |> Map.map (fun k v ->
                match Map.tryFind k externalEnv with
                | Some vec ->
                    // Grab scalar tensor and wrap via tryAsScalar (no float conversion)
                    match Tensors.tryAsScalar<``environment``> vec.Value.[timeIdx] with
                    | Some s -> s
                    | None   -> invalidOp "Expected scalar from external environment vector"
                | None -> v)

        let tInitial = tInitial |> Tensors.Typed.ofScalar
        let tStep = tStep |> Tensors.Typed.ofScalar
        let tEnd = tEnd |> Tensors.Typed.ofScalar

        // STAGE 2. Make a parameter-specific concrete RHS.
        fun (parameters: TypedTensor<Vector,``parameter``>) ->

            // The bound RHS now closes over `parameters` but reuses all static prep
            fun (t: TypedTensor<Scalar,``time index``>)
                (x: TypedTensor<Vector,ModelSystem.state>) ->

                let idx = int ((t.Value - tInitial.Value) / tStep.Value)

                let env =
                    if idx = 0 then
                        applyDynamicVariablesT x modelKeys initialEnv
                    else
                        initialEnv
                        |> applyExternalEnvironmentTensor idx externalEnvTensors
                        |> applyDynamicVariablesT x modelKeys

                // Compute derivatives for all variables
                let result =
                    modelEqs
                    |> Array.mapi (fun i m ->
                        let xi =
                            Tensors.tryAsScalar x.Value.[i]
                            |> Option.defaultWith (fun () -> invalidOp "Expected scalar state component")
                        (m parameters env t xi).Value)
                    |> Array.zip modelKeys
                    |> Array.map(fun (k,v) -> k, v |> Tensors.tryAsVector)
                
                if result |> Array.exists(fun (_,v) -> Option.isNone v)
                then invalidOp "RHS did not produce vector"
                else result |> Map.ofArray |> Map.map(fun _ v -> Option.get v)

    // let mkIntegrator : EstimationEngine.Integrate<'date, 'timeunit, 'timespan> =
    //     fun log tInitial tEnd tStep initialConditions externalEnvironment modelMap ->
    //         match modelMap with
    //         | EstimationEngine.FloatODEs odes ->
    //             failwith "not implemented"
    //             // odes |> Base.makeIntegrator log rk4float tInitial tEnd tStep initialConditions externalEnvironment
    //         | EstimationEngine.TensorODEs odes ->
                
    //             let initialConditionsT : CodedMap<Tensors.TypedTensor<Tensors.Scalar,ModelSystem.environment>> =
    //                 initialConditions
    //                 |> Map.map(fun _ v -> v |> (*) 1.<ModelSystem.environment> |> Tensors.Typed.ofScalar)                
                
    //             makeCompiledFunctionForIntegration tInitial tEnd tStep initialConditionsT externalEnvironment odes                


module RungeKutta =

    open Bristlecone
    open Bristlecone.Time
    open DiffSharp
    open Tensors
    
    let private rk4Core
        (tInitial: TypedTensor<Scalar,``time index``>)
        (steps   : int)
        (dt      : Tensor)
        (y0      : Tensor)
        (f       : Tensor -> Tensor -> Tensor) =

        let mutable t = tInitial
        let mutable y = y0
        let outputs = ResizeArray<Tensor>()
        outputs.Add y
        for _ in 1 .. steps do
            let k1 = f t.Value           y
            let k2 = f (t.Value + dt/2.0) (y + (dt/2.0) * k1)
            let k3 = f (t.Value + dt/2.0) (y + (dt/2.0) * k2)
            let k4 = f (t.Value + dt)     (y + dt * k3)
            y <- y + (dt/6.0) * (k1 + dsharp.tensor 2.0 * k2 + dsharp.tensor 2.0 * k3 + k4)
            t <- Tensors.Typed.addScalar t (dt |> Tensors.asScalar)
            outputs.Add y
        dsharp.stack outputs

    let rk4WithStepCount
        (tInitial: TypedTensor<Scalar,``time index``>)
        (tFinal  : TypedTensor<Scalar,``time index``>)
        (steps   : int)
        (y0      : Tensor)
        (f       : Tensor -> Tensor -> Tensor) =
        let dt = (tFinal.Value - tInitial.Value) / dsharp.tensor (float steps)
        rk4Core tInitial steps dt y0 f

    let rk4WithStepWidth
        (tInitial: TypedTensor<Scalar,``time index``>)
        (tFinal  : TypedTensor<Scalar,``time index``>)
        (dt      : TypedTensor<Scalar,``time index``>)
        (y0      : Tensor)
        (f       : Tensor -> Tensor -> Tensor) =
        let steps = int ((tFinal.Value - tInitial.Value) / dt.Value)
        rk4Core tInitial steps dt.Value y0 f

    let rk4float 
        (tInitial: float<``time index``>) 
        (tFinal: float<``time index``>)
        (steps: float<``time index``>) 
        (initialVector: float[]) 
        (f: float -> float[] -> float)
        : float[][] =
        
        let tInit  = tInitial |> Tensors.Typed.ofScalar
        let tFinal = tFinal |> Tensors.Typed.ofScalar
        let steps = steps |> Tensors.Typed.ofScalar
        let y0    = dsharp.tensor(initialVector, dtype = Dtype.Float64)

        let tensorTrajectory =
            rk4WithStepWidth
                tInit
                tFinal
                steps
                y0
                (fun t x ->
                    let arrDoubles =
                        match x.toArray() with
                        | :? array<float>  as doubles -> doubles
                        | :? array<single> as singles -> singles |> Array.map float
                        | other -> failwithf "Unexpected tensor storage: %A" (other.GetType())
                    dsharp.tensor(f (t.toDouble()) arrDoubles, dtype = Dtype.Float64)
                )

        let arr2d : float[,] = tensorTrajectory.toArray2D()
        Array.init (Array2D.length1 arr2d) (fun i ->
            Array.init (Array2D.length2 arr2d) (fun j -> arr2d.[i, j])
        )

    // Flatten a CodedMap<Scalar> into (keys, Tensor vector)
    let flattenState (stateMap: CodedMap<TypedTensor<Scalar,ModelSystem.state>>) =
        let keys, vals =
            stateMap
            |> Map.toList
            |> List.unzip
        let vec =
            vals
            |> List.map (fun s -> s.Value) // Tensor scalar
            |> dsharp.stack
        keys, vec

    // Wrap a ParameterisedRHS so it works on flat Tensors
    let wrapRhs (keys: ShortCode.ShortCode list) (rhs: EstimationEngine.ParameterisedRHS) =
        fun (t: Tensor) (y: Tensor) ->
            // Rebuild state map from flat vector
            let stateMap =
                (keys, y.unstack())
                ||> Seq.map2 (fun k comp ->
                    // comp is a Tensor vector — wrap without converting to float
                    match tryAsVector<ModelSystem.state> comp with
                    | Some v -> k, v
                    | None   -> failwith "Expected vector state component")
                |> Map.ofSeq
            // Call original RHS
            let resultMap = rhs (asScalar<``time index``> t) ( // wrap time as scalar
                                match tryAsVector<ModelSystem.state> y with
                                | Some v -> v
                                | None   -> failwith "Expected vector state input")
            // Flatten result back to Tensor
            keys
            |> List.map (fun k -> resultMap.[k].Value)
            |> dsharp.stack

    // Unflatten trajectory Tensor back into CodedMap<Vector,state>
    let unflattenTrajectory (keys: ShortCode.ShortCode list) (traj: Tensor) =
        let comps = traj.unstack(1) // list of Tensor vectors
        (keys, comps)
        ||> Seq.map2 (fun k comp ->
            match tryAsVector<ModelSystem.state> comp with
            | Some v -> k, v
            | None   -> failwith "Expected vector trajectory component")
        |> Map.ofSeq

    let rk4 : EstimationEngine.Integration.IntegrationRoutine =
        fun tInitial tEnd tStep t0 rhs ->
            let keys, y0 = flattenState t0
            let fWrapped = wrapRhs keys rhs
            let traj = rk4WithStepWidth tInitial tEnd tStep y0 fWrapped
            unflattenTrajectory keys traj
            