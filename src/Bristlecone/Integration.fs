namespace Bristlecone.Integration

/// Module provides functions that 'wrap' a raw integration
/// routine into a form that can be used within Bristlecone.
module Base =

    open Bristlecone
    open Bristlecone.Logging
    open Bristlecone.Time

    /// Generates a coded map of time-series where all values are NaN.
    let nanResult tInitial tEnd (tStep: float<``time index``>) modelMap =
        let variableCodes = modelMap |> Map.toArray |> Array.unzip |> fst

        let fakeSeries =
            let count = (tEnd - tInitial + 1.<``time index``>) / tStep |> int
            [ 1..count ] |> List.map (fun _ -> nan) |> List.toArray

        variableCodes |> Array.map (fun k -> (k, fakeSeries)) |> Map.ofArray

    let internal applyDynamicVariables newValues newValueKeys environment =
        let newEnv = newValues |> Array.zip newValueKeys

        environment
        |> Map.map (fun key value ->
            let updated = newEnv |> Array.tryFind (fun (k2, _) -> k2 = key)

            match updated with
            | Some u -> snd u
            | None -> value)

    let internal applyExternalEnvironment
        (time: float<``time index``>)
        (externalEnv: Map<'a, TimeIndex.TimeIndex<'T, 'date, 'timeunit, 'timespan>>)
        (currentEnv: Map<'a, 'T>)
        =
        currentEnv
        |> Map.map (fun k v ->
            let updated = externalEnv |> Map.tryFind k

            match updated with
            | Some index -> index.[time]
            | None -> v)

    let solve
        log
        integrate
        (tInitial: float<``time index``>)
        tEnd
        tStep
        initialConditions
        externalEnvironment
        (modelMap: CodedMap<EstimationEngine.ODE>)
        : CodedMap<float[]> =

        // A. Setup initial vector
        let modelKeys, modelEqs = modelMap |> Map.toArray |> Array.unzip

        let vectorKeys, initialVector =
            modelMap
            |> Map.toArray
            |> Array.map (fun (k, _) -> (k, initialConditions |> Map.find k |> Units.removeUnitFromFloat))
            |> Array.unzip

        // B. Setup composite function to integrate
        let mutable iteration = 1<iteration>

        let rp (t: float) x =
            let t = t * 1.<``time index``>

            if iteration % 5000<iteration> = 0<iteration> then
                log <| GeneralEvent(sprintf "[Integration] Slow for %f - %A" t x)

            iteration <- iteration + 1<iteration>

            let environment =
                if t < tInitial + tStep then
                    initialConditions |> applyDynamicVariables x vectorKeys
                else
                    initialConditions
                    |> applyExternalEnvironment t externalEnvironment
                    |> applyDynamicVariables x vectorKeys

            modelEqs |> Array.mapi (fun i m -> m t x.[i] environment)

        // C. Integrate
        let result: float[][] = integrate tInitial tEnd tStep initialVector rp

        // D. Match produced data back to keys
        modelKeys
        |> Seq.mapi (fun i k -> (k, result |> Array.map (fun x -> x.[i])))
        |> Map.ofSeq


module RungeKutta =

    open Bristlecone
    open Bristlecone.Time
    open DiffSharp
    
    let rk4'
            (tInitial: Tensor) 
            (tFinal: Tensor) 
            (steps: int) 
            (y0: Tensor) 
            (f: Tensor -> Tensor -> Tensor) 
            : Tensor
            =
        let dt = (tFinal - tInitial) / dsharp.tensor(float steps)
        let mutable t = tInitial
        let mutable y = y0
        for _ in 1 .. steps do
            let k1 = f t           y                     
            let k2 = f (t + dt/2.0) (y + (dt/2.0) * k1)  
            let k3 = f (t + dt/2.0) (y + (dt/2.0) * k2) 
            let k4 = f (t + dt)     (y + dt * k3)        
            y <- y + (dt/6.0) * (k1 + dsharp.tensor(2.0)*k2 + dsharp.tensor(2.0)*k3 + k4)
            t <- t + dt
        y

    let rk4float (tInitial:float<``time index``>) (tFinal:float<``time index``>)
        (steps:float<``time index``>) initialVector
        (f:(float -> array<float> -> array<EstimationEngine.State>)) : float array array =
        let answer =
            rk4'
                (dsharp.tensor tInitial)
                (dsharp.tensor tFinal)
                1 // TODO
                (dsharp.tensor initialVector)
                (fun t x -> f (t.toDouble()) (x.toArray() :?> float[]) |> dsharp.tensor)
        let arr2d = answer.toArray2D()
        Array.init (Array2D.length1 arr2d) (fun i -> Array.init (Array2D.length2 arr2d) (fun j -> arr2d.[i, j]))

    let rk4: EstimationEngine.Integrate<float, 'date, 'timeunit, 'timespan> =
        fun log tInitial tEnd tStep initialConditions externalEnvironment (modelMap: CodedMap<EstimationEngine.ODE>) ->
            Base.solve log rk4float tInitial tEnd tStep initialConditions externalEnvironment modelMap
