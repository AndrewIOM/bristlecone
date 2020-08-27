namespace Bristlecone.Integration

/// Module provides functions that 'wrap' a raw integration
/// routine into a form that can be used within Bristlecone.
module Base =

    open Bristlecone.Logging
    open Bristlecone.Time

    /// Generates a coded map of time-series where all values are NaN.
    let nanResult tInitial tEnd tStep modelMap =
        let variableCodes = modelMap |> Map.toArray |> Array.unzip |> fst
        let fakeSeries =
            let count = (tEnd - tInitial + 1.) / tStep |> int
            [ 1 .. count ] |> List.map (fun _ -> nan ) |> List.toArray
        variableCodes
        |> Array.map (fun k -> (k, fakeSeries) )
        |> Map.ofArray 

    let applyDynamicVariables newValues newValueKeys environment =
        let newEnv = newValues |> Array.zip newValueKeys
        environment
        |> Map.map(fun key value ->
            let updated = newEnv |> Array.tryFind(fun (k2,_) -> k2 = key)
            match updated with
            | Some u -> snd u
            | None -> value )

    let applyExternalEnvironment (time:float) (externalEnv:Map<'a,TimeIndex.TimeIndex<'b>>) (currentEnv:Map<'a,'b>) =
        currentEnv |> Map.map(fun k v ->
            let updated = externalEnv |> Map.tryFind k
            match updated with
            | Some index -> index.[time]
            | None -> v )

    let solve log integrate tInitial tEnd tStep initialConditions externalEnvironment modelMap : Map<'a, float[]> =

        // A. Setup initial vector
        let modelKeys,modelEqs = modelMap |> Map.toArray |> Array.unzip
        let vectorKeys, initialVector =
            modelMap
            |> Map.toArray
            |> Array.map(fun (k,_) -> (k, initialConditions |> Map.find k) )
            |> Array.unzip

        // B. Setup composite function to integrate
        let mutable iteration = 1
        let rp (t:float) x = 
            if iteration % 5000 = 0 then log <| GeneralEvent (sprintf "[Integration] Slow for %f - %A" t x)
            iteration <- iteration + 1
            let environment = 
                if t < tInitial + tStep then 
                    initialConditions
                    |> applyDynamicVariables x vectorKeys
                else
                    initialConditions
                    |> applyExternalEnvironment t externalEnvironment
                    |> applyDynamicVariables x vectorKeys
            modelEqs
            |> Array.mapi (fun i m -> m t x.[i] environment)

        // C. Integrate
        let result : float[][] = integrate tInitial tEnd tStep initialVector rp

        // D. Match produced data back to keys
        modelKeys
        |> Seq.mapi(fun i k -> (k, result |> Array.map(fun x -> x.[i])))
        |> Map.ofSeq


/// Oslo is an integration library provided by Microsoft Research Cambridge.
module Oslo =

    open Microsoft.Research.Oslo

    module Options =

        let defaultOptions = 
            Options(
                AbsoluteTolerance = 1e-6, 
                RelativeTolerance = 1e-3, 
                MinStep = 0.1, MaxStep = 1., 
                MinScale = 0.9, 
                MaxScale = 1.1, OutputStep = 1.)

        let custom absTol relTol = 
            Options(
                AbsoluteTolerance = absTol, 
                RelativeTolerance = relTol, 
                MinStep = 0.1, MaxStep = 1., 
                MinScale = 0.9, 
                MaxScale = 1.1, OutputStep = 1.)


    let integrate' options tInitial tEnd tStep initialVector rp = 
        let rk = Ode.RK547M(tInitial, (initialVector |> Vector), System.Func<double,Vector,Vector> (fun x y -> rp x (y.ToArray()) |> Vector), options)
        rk.SolveFromToStep(tInitial, tEnd, tStep) 
        |> Seq.map (fun x -> x.X.ToArray())
        |> Seq.toArray

    let integrate options log tInitial tEnd tStep initialConditions modelMap =
        Base.solve log (integrate' options) tInitial tEnd tStep initialConditions modelMap

    /// On integration errors, assigns the maximum float value to every data point.
    let integrateWithErrorHandling options log tInitial tEnd tStep initialConditions externalEnvironment modelMap =
        try integrate options log tInitial tEnd tStep initialConditions externalEnvironment modelMap with
        | _ -> Base.nanResult tInitial tEnd tStep modelMap


module MathNet =

    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.OdeSolvers

    let integrate' tInitial tEnd tStep initialVector rp =
        let n = (tEnd - tInitial + 1.) / tStep |> int
        let f = System.Func<float, Vector<float>, Vector<float>> (fun i e -> rp i (e.ToArray()) |> vector)
        RungeKutta.FourthOrder(initialVector |> vector, tInitial, tEnd, n, f)
        |> Array.map Vector.toArray

    let integrate log tInitial tEnd tStep initialConditions externalEnvironment modelMap =
        Base.solve log integrate' tInitial tEnd tStep initialConditions externalEnvironment modelMap


module Simple =

    /// Fourth order runge-kutta algorithm calculations
    let rungekutta4' x y h f = 
        let k1 = h * f x y
        let k2 = h * f (x + 0.5 * h) (y + k1 * 0.5)
        let k3 = h * f (x + 0.5 * h) (y + k2 * 0.5)
        let k4 = h * f (x + h) (y + k3)
        y + 1.0/6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

    /// Fourth order runge-kutta solver
    let rec rungekutta4 x y h n f xx yy = 
        let y' = rungekutta4' x y h f
        if (x >= n) then (xx, yy)
        else rungekutta4 (x+h) y' h n f (List.append xx [x+h]) (List.append yy [y'])

    /// Fourth order runge-kutta algorithm for a 2 equation, 1st order system. 
    /// Adapted from: https://www.calvin.edu/~scofield/courses/m231/materials/rungeKuttaFormulas.pdf
    let rungekutta4dual' t x y h f g =
        let k1 = h * f t x y
        let l1 = h * g t x y
        let k2 = h * f (t + 0.5 * h) (x + k1 * 0.5) (y + l1 * 0.5)
        let l2 = h * g (t + 0.5 * h) (x + k1 * 0.5) (y + l1 * 0.5)
        let k3 = h * f (t + 0.5 * h) (x + k2 * 0.5) (y + l2 * 0.5)
        let l3 = h * g (t + 0.5 * h) (x + k2 * 0.5) (y + l2 * 0.5)
        let k4 = h * f (t + h) (x + k3) (y + l3)
        let l4 = h * g (t + h) (x + k3) (y + l3)
        let xinc = x + 1./6. * (k1 + 2. * k2 + 2. * k3 + k4)
        let yinc = y + 1./6. * (l1 + 2. * l2 + 2. * l3 + l4)
        (xinc, yinc)

    /// Fourth order runge-kutta solver for a 2 equation, 1st order system
    let rec rungekutta4dual t x y h n f g tt xx yy =
        let y' = rungekutta4dual' t x y h f g
        if (t >= n) then (tt, xx, yy)
        else rungekutta4dual (t+h) (fst y') (snd y') h n f g (List.append tt [t+h]) (List.append xx [(fst y')]) (List.append yy [(snd y')])

    /// 4th order runge-kutta solver that can handle (in very basic form) variable time, for example when using sedimentary age-depth models.
    /// It is for a single-equation, 1st order system.
    let rec rungeKutta4Variable currentStep (steps:float list) t x f tt xx =
        let x' = rungekutta4' t x currentStep f
        if (steps.Length = 0) then ((List.append tt [t+currentStep]), (List.append xx [x']))
        else rungeKutta4Variable steps.Head steps.Tail (t+currentStep) x' f (List.append tt [t+currentStep]) (List.append xx [x'])
