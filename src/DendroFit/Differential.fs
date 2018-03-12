module ODE

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
    if (x >= n) then (xx,yy)
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
    if (t >= n) then (tt,xx,yy)
    else rungekutta4dual (t+h) (fst y') (snd y') h n f g (List.append tt [t+h]) (List.append xx [(fst y')]) (List.append yy [(snd y')])

/// 4th order runge-kutta solver that can handle (in very basic form) variable time, for example when using sedimentary age-depth models.
/// It is for a single-equation, 1st order system.
let rec rungeKutta4Variable currentStep (steps:float list) t x f tt xx =
    let x' = rungekutta4' t x currentStep f
    if (steps.Length = 0) then ((List.append tt [t+currentStep]),(List.append xx [x']))
    else rungeKutta4Variable steps.Head steps.Tail (t+currentStep) x' f (List.append tt [t+currentStep]) (List.append xx [x'])


module Oslo =

    open Microsoft.Research.Oslo
    open Types

    let solve2D initialTime initialX1 initialX2 (rightPart:float->float->float->float*float) endTime step  = 

        let rp t (v:Vector) =
            let (x1, x2) = rightPart t v.[0] v.[1]
            Vector(x1,x2)

        let rk = Ode.RK547M (initialTime,
                            Vector(initialX1, initialX2),
                            System.Func<double,Vector,Vector>(rp))

        rk.SolveFromToStep(initialTime, endTime, step) |> Seq.toArray

    let private updateEnvironment (newValues:Vector) newValueKeys environment =
        let newEnv = newValues.ToArray() |> Array.zip newValueKeys
        environment
        |> Map.map(fun key value ->
            let updated = newEnv |> Array.tryFind(fun (k2,_) -> k2 = key)
            match updated with
            | Some u -> snd u
            | None -> value )

    let solve t tEnd tStep initialConditions modelMap =
    
        let keys,values = initialConditions |> Map.toArray |> Array.unzip
        let modelKeys,modelEqs = modelMap |> Map.toArray |> Array.unzip

        let rp t (x:Vector) = 
            modelEqs
            |> Array.map (fun m -> m t x.[0] (updateEnvironment x keys initialConditions))
            |> Vector

        let options = Options(AbsoluteTolerance = 1e-6, RelativeTolerance = 1e-6)
        let rk = Ode.RK547M(t, (values |> Vector), System.Func<double,Vector,Vector> rp, options)

        let result =
            rk.SolveFromToStep(t, tEnd, tStep) 
            |> Seq.map (fun x -> x.X.ToArray())
            |> Seq.toArray

        modelKeys
        |> Seq.mapi(fun i k -> k, result |> Array.map(fun x -> x.[i]))
        |> Map.ofSeq