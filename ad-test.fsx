#r "nuget: DiffSharp-cpu"

open DiffSharp

// =========================
// 1. RK4 Integrator
// =========================
module Integration =
    let rk4 (f: Tensor -> Tensor -> Tensor -> Tensor) 
            (t0: Tensor) 
            (y0: Tensor) 
            (tFinal: Tensor) 
            (steps: int) 
            (p: Tensor) : Tensor =
        let dt = (tFinal - t0) / dsharp.tensor(float steps)
        let mutable t = t0
        let mutable y = y0
        for _ in 1 .. steps do
            let k1 = f t           y                     p
            let k2 = f (t + dt/2.0) (y + (dt/2.0) * k1)  p
            let k3 = f (t + dt/2.0) (y + (dt/2.0) * k2)  p
            let k4 = f (t + dt)     (y + dt * k3)        p
            y <- y + (dt/6.0) * (k1 + dsharp.tensor(2.0)*k2 + dsharp.tensor(2.0)*k3 + k4)
            t <- t + dt
        y

// =========================
// 2. Lotka–Volterra ODE
// =========================
let lotkaVolterra (t: Tensor) (y: Tensor) (p: Tensor) : Tensor =
    let prey, predator = y.[0], y.[1]
    let alpha, beta, delta, gamma = p.[0], p.[1], p.[2], p.[3]
    let dPrey     = alpha * prey - beta * prey * predator
    let dPredator = delta * prey * predator - gamma * predator
    dsharp.stack [dPrey; dPredator]

// =========================
// 3. Synthetic Observations
// =========================
let obsTimes = [| 1. .. 30. |] |> Array.map dsharp.tensor
let trueParams = dsharp.tensor [| 1.5; 1.0; 1.0; 3.0 |]
let y0 = dsharp.tensor [| 1.; 1. |]
let yObsVecs =
    obsTimes
    |> Array.map (fun t ->
        let y = Integration.rk4 lotkaVolterra (dsharp.tensor 0.0) y0 t 200 trueParams
        y + 0.05 * dsharp.randn([2])
    )

// =========================
// 4. Improvement-based schedule
// =========================
let rk4StepScheduleByError stepsMin stepsMax startN currentBestN =
    let improvement = max 0.0 (float startN - float currentBestN)
    let totalNeeded = max 1e-9 (float startN)
    let frac = improvement / totalNeeded
    let fracClamped = min 1.0 frac
    int (float stepsMin + fracClamped * float (stepsMax - stepsMin))

// =========================
// 5. Adaptive NLL / Grad
// =========================
let makeAdaptiveNll stepsMin stepsMax startN getBestN =
    fun (p: Tensor) ->
        let steps = rk4StepScheduleByError stepsMin stepsMax startN (getBestN())
        let sigma2 = dsharp.tensor(0.05 ** 2)
        let mutable tPrev = dsharp.tensor 0.0
        let mutable yPrev = y0
        let mutable totalNegLogLik = dsharp.tensor 0.0
        for i in 0 .. obsTimes.Length - 1 do
            let tNext = obsTimes.[i]
            let yObs = yObsVecs.[i]
            let yPred = Integration.rk4 lotkaVolterra tPrev yPrev tNext steps p
            let diff = yObs - yPred
            totalNegLogLik <- totalNegLogLik + 0.5 * (diff * diff / sigma2).sum()
            tPrev <- tNext
            yPrev <- yPred
        totalNegLogLik

let makeAdaptiveGrad adaptiveNll =
    fun (p: Tensor) -> dsharp.grad adaptiveNll p

// =========================
// 6. HMC with adaptive RK4 steps
// =========================
module Hmc =

    open DiffSharp

    type State = { Q: Tensor; P: Tensor }
    type Params =
        { Epsilon: float
          LeapfrogSteps: int
          Mass: float
          ClipThreshold: float
          TargetAccept: float
          AdaptRate: float
          Lmin: int
          Lmax: int }

    type IterResult =
        { Iter: int
          Params: Params
          CurrentQ: Tensor
          BestQ: Tensor
          BestNll: Tensor
          Alpha: float
          AvgGradNorm: float }

    let kineticEnergy mass (p: Tensor) =
        0.5 * (p * p).sum() / mass

    let hamiltonian nll mass state =
        nll state.Q + kineticEnergy mass state.P

    let clipGrad clipThreshold (g: Tensor) =
        let nrm = g.norm().toDouble()
        if nrm > clipThreshold then g * (clipThreshold / nrm) else g

    let leapfrogStep gradFn epsilon mass clipThresh state =
        let pHalf = state.P - (epsilon / 2.0) * (clipGrad clipThresh (gradFn state.Q))
        let qNew = state.Q + (epsilon / mass) * pHalf
        let pNew = pHalf - (epsilon / 2.0) * (clipGrad clipThresh (gradFn qNew))
        { Q = qNew; P = pNew }

    let simulateTrajectory gradFn epsilon mass clipThresh steps state =
        let rec loop s grads step =
            if step = steps then s, List.rev grads
            else
                let s' = leapfrogStep gradFn epsilon mass clipThresh s
                let gNorm = gradFn s'.Q |> fun g -> g.norm().toDouble()
                loop s' (gNorm :: grads) (step + 1)
        loop state [] 0

    let acceptanceProb nll mass current proposed =
        let deltaH = (hamiltonian nll mass proposed) - (hamiltonian nll mass current)
        min 1.0 (exp (- deltaH.toDouble()))

    let adaptLeapfrog avgGradNorm alpha p =
        if avgGradNorm < p.ClipThreshold / 2.0 && alpha > p.TargetAccept then
            { p with LeapfrogSteps = min p.Lmax (p.LeapfrogSteps + 1) }
        elif avgGradNorm > p.ClipThreshold && alpha < p.TargetAccept then
            { p with LeapfrogSteps = max p.Lmin (p.LeapfrogSteps - 1) }
        else p

    let adaptEpsilon alpha p =
        { p with Epsilon =
                  max 1e-6 (p.Epsilon * exp (p.AdaptRate * (alpha - p.TargetAccept))) }

    // Pure search — returns a list of iteration results
    let search adaptiveNll gradFn randMomentum randAccept initQ iterations initParams =
        let rec loop iter currentQ bestQ bestNll (pars: Params) acc =
            if iter > iterations then List.rev acc
            else
                let currentP = randMomentum currentQ pars.Mass
                let currentState = { Q = currentQ; P = currentP }
                let proposedState, gradNorms =
                    simulateTrajectory gradFn pars.Epsilon pars.Mass pars.ClipThreshold pars.LeapfrogSteps currentState

                let alpha = acceptanceProb adaptiveNll (dsharp.tensor pars.Mass) currentState proposedState
                let acceptDraw = randAccept()

                let newQ, newBestQ, newBestNll =
                    if acceptDraw < alpha then
                        let nllProp = adaptiveNll proposedState.Q
                        if nllProp < bestNll then
                            proposedState.Q, proposedState.Q, nllProp
                        else proposedState.Q, bestQ, bestNll
                    else currentQ, bestQ, bestNll

                let avgGradNorm = gradNorms |> List.average
                let params' =
                    if iter <= iterations / 2 then
                        pars
                        |> adaptEpsilon alpha
                        |> fun p' ->
                            if iter % 50 = 0 then adaptLeapfrog avgGradNorm alpha p'
                            else p'
                    else pars

                let result =
                    { Iter = iter
                      Params = params'
                      CurrentQ = newQ
                      BestQ = newBestQ
                      BestNll = newBestNll
                      Alpha = alpha
                      AvgGradNorm = avgGradNorm }

                loop (iter + 1) newQ newBestQ newBestNll params' (result :: acc)

        loop 1 initQ initQ (adaptiveNll initQ) initParams []


// =========================
// 7. Run the search
// =========================
let totalIters = 1000
let initialGuess = dsharp.tensor [| 1.0; 0.8; 0.5; 1.2 |]

let bestParams, bestLik =
    hmcSearchAdaptiveL
        initialGuess
        totalIters
        0.65      // target accept
        0.01      // starting ε
        10        // initial leapfrog steps
        1.0       // mass
        3.0       // starting clip threshold
        0.1       // min clip
        100.0     // max clip
        50        // window for clip adaptation
        5         // min leapfrog steps
        50        // max leapfrog steps

let rng = System.Random()

let randMomentum (q: Tensor) (mass: float) =
    dsharp.randn(q.shape) * sqrt mass

let randAccept () =
    rng.NextDouble()

let initParams = {
    Epsilon: float
    LeapfrogSteps: int
    Mass: float
    ClipThreshold: float
    TargetAccept: float
    AdaptRate: float
    Lmin: int
    Lmax: int
}

let results =
    Hmc.search adaptiveNll gradFn randMomentum randAccept initQ 1000 initParams



printfn "Best params found: %A" bestParams
printfn "Best likelihood: %A" bestLik
