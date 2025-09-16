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
// 6. MALA with adaptive RK4 steps
// =========================
let hmcSearchWindowedClip
    (initParams: Tensor)
    (iterations: int)
    (targetAccept: float)
    (epsilonInit: float)
    (leapfrogSteps: int)
    (mass: float)
    (clipInit: float)
    (clipMin: float)
    (clipMax: float)
    (clipWindow: int) =

    let mutable epsilon = epsilonInit
    let adaptRate = 0.2
    let mutable clipThreshold = clipInit
    let mutable currentQ = initParams

    // Initialise best as starting point
    let dummyNll = makeAdaptiveNll 20 200 1.0 (fun () -> 1.0)
    let mutable bestQ, bestNll = currentQ, dummyNll currentQ

    let adaptiveNll = makeAdaptiveNll 20 200 (bestNll.toDouble()) (fun () -> (bestNll.toDouble()))
    let adaptiveGradRaw = makeAdaptiveGrad adaptiveNll

    let rnd = System.Random()

    // Track recent clip usage
    let clipHistory = System.Collections.Generic.Queue<bool>()
    let pushHistory wasClipped =
        clipHistory.Enqueue(wasClipped)
        if clipHistory.Count > clipWindow then clipHistory.Dequeue() |> ignore
        clipHistory |> Seq.averageBy (fun c -> if c then 1.0 else 0.0)

    let clipGrad (g: Tensor) =
        let nrm = g.norm()
        if nrm > dsharp.tensor(clipThreshold) then g * (clipThreshold / nrm) else g

    for iter in 1 .. iterations do
        // Draw momentum
        let currentP = dsharp.randn(currentQ.shape) * sqrt mass

        let q = ref currentQ
        let p = ref currentP

        // Initial half-step
        let grad0Raw = adaptiveGradRaw !q
        let grad0 = clipGrad grad0Raw
        let _ = pushHistory (grad0Raw.norm().toDouble() > clipThreshold)
        p := !p - (epsilon / 2.0) * grad0

        // Leapfrog
        for lf in 1 .. leapfrogSteps do
            // Full position
            q := !q + (epsilon / mass) * !p
            if lf <> leapfrogSteps then
                let gRaw = adaptiveGradRaw !q
                let g = clipGrad gRaw
                let _ = pushHistory (gRaw.norm().toDouble() > clipThreshold)
                p := !p - epsilon * g

        // Final half-step
        let gEndRaw = adaptiveGradRaw !q
        let gEnd = clipGrad gEndRaw
        let _ = pushHistory (gEndRaw.norm().toDouble() > clipThreshold)
        p := !p - (epsilon / 2.0) * gEnd

        // Negate momentum
        p := -(!p)

        // Hamiltonians
        let currentU = adaptiveNll currentQ
        let currentK = 0.5 * (currentP * currentP).sum() / mass
        let proposedU = adaptiveNll !q
        let proposedK = 0.5 * (!p * !p).sum() / mass

        let logAlpha = -(proposedU - currentU) - (proposedK - currentK)
        let alpha = min 1.0 (exp (float logAlpha))

        // Accept/reject
        if rnd.NextDouble() < alpha then
            currentQ <- !q
            if proposedU < bestNll then
                bestQ <- !q
                bestNll <- proposedU

        // Step size adapts every iter
        if iter <= iterations / 2 then
            epsilon <- max 1e-6 (epsilon * exp(adaptRate * (alpha - targetAccept)))

        // Clip adapts every clipWindow iters
        if iter % clipWindow = 0 && iter <= iterations / 2 then
            let fracClipped = clipHistory |> Seq.averageBy (fun c -> if c then 1.0 else 0.0)
            if fracClipped > 0.8 && alpha < targetAccept then
                clipThreshold <- max clipMin (clipThreshold * 0.9)
            elif fracClipped < 0.2 && alpha > targetAccept + 0.05 then
                clipThreshold <- min clipMax (clipThreshold * 1.05)

        if iter % 10 = 0 then
            printfn "Iter %d | ε=%.5f | α=%.3f | -logL=%.4f | clip=%.3f"
                iter epsilon alpha (bestNll.toDouble()) clipThreshold

    bestQ, bestNll


// let malaSearchWindowedClip
//     (initParams: Tensor)
//     (iterations: int)
//     (targetAccept: float)
//     (clipInit: float)
//     (clipMin: float)
//     (clipMax: float)
//     (clipWindow: int) =
    
//     let mutable stepSize = 1e-3
//     let adaptRate = 0.2
//     let mutable clipThreshold = clipInit
//     let mutable current = initParams
//     let dummyNll = makeAdaptiveNll 20 200 1.0 (fun () -> 1.0)
//     let mutable currentN = dummyNll current
//     let mutable best, bestN = current, currentN
//     let startN = bestN
//     let rnd = System.Random()

//     let adaptiveNll = makeAdaptiveNll 20 200 (startN.toDouble()) (fun () -> (bestN.toDouble()))
//     let adaptiveGrad = makeAdaptiveGrad adaptiveNll

//     currentN <- adaptiveNll current
//     bestN <- currentN

//     // History tracking for clip adaptation
//     let clipHistory = System.Collections.Generic.Queue<bool>()
//     let pushHistory wasClipped =
//         clipHistory.Enqueue(wasClipped)
//         if clipHistory.Count > clipWindow then clipHistory.Dequeue() |> ignore
//         clipHistory |> Seq.averageBy (fun c -> if c then 1.0 else 0.0)

//     let clipGrad (g: Tensor) =
//         let nrm = g.norm()
//         if nrm > dsharp.tensor(clipThreshold) then g * (clipThreshold / nrm) else g

//     for iter in 1 .. iterations do
//         let rawGrad = adaptiveGrad current
//         let grad = clipGrad rawGrad
//         let wasClipped = rawGrad.norm().toDouble() > clipThreshold
//         let noise = dsharp.randn(current.shape)
//         let proposal = current - (stepSize * stepSize / 2.0) * grad + stepSize * noise
//         let propN = adaptiveNll proposal
//         let gradProp = clipGrad (adaptiveGrad proposal)

//         let logForward =
//             -((proposal - current + (stepSize * stepSize / 2.0) * grad).pow(2).sum())
//              / (2.0 * stepSize * stepSize)
//         let logBackward =
//             -((current - proposal + (stepSize * stepSize / 2.0) * gradProp).pow(2).sum())
//              / (2.0 * stepSize * stepSize)
//         let logAlpha = -(propN - currentN) + (logBackward - logForward)
//         let alpha = min 1.0 (exp (float logAlpha))

//         // Diagnostics
//         printfn "Iter %d | -lL = %f grad=%.3f -> %.3f | α=%.3f | step=%.5f | clip=%.3f | clipped=%b"
//             iter (currentN.toDouble()) (rawGrad.norm().toDouble()) (grad.norm().toDouble()) alpha stepSize clipThreshold wasClipped

//         // Accept/reject
//         if rnd.NextDouble() < alpha then
//             current <- proposal
//             currentN <- propN
//             if propN < bestN then
//                 best <- proposal
//                 bestN <- propN

//         // Step size adapts every iter
//         if iter <= iterations / 2 then
//             stepSize <- max 1e-6 (stepSize * exp(adaptRate * (alpha - targetAccept)))

//         // Clip adapts every `clipWindow` iters, based on fraction clipped in window
//         if iter % clipWindow = 0 && iter <= iterations / 2 then
//             let fracClipped = pushHistory wasClipped
//             if fracClipped > 0.8 && alpha < targetAccept then
//                 clipThreshold <- max clipMin (clipThreshold * 0.9)
//             elif fracClipped < 0.2 && alpha > targetAccept + 0.05 then
//                 clipThreshold <- min clipMax (clipThreshold * 1.05)

//     best, bestN

// =========================
// 7. Run the search
// =========================
let totalIters = 1000
let initialGuess = dsharp.tensor [| 1.0; 0.8; 0.5; 1.2 |]

// let bestParams, bestLik =
//     malaSearchWindowedClip
//         initialGuess
//         totalIters
//         0.65    // target accept
//         3.0     // starting clip
//         0.1     // minimum clip
//         100.0   // maximum clip
//         50      // adjust clip every 50 iters

let bestParams, bestLik =
    hmcSearchWindowedClip
        initialGuess
        totalIters
        0.65      // target accept
        0.01      // starting ε
        10        // leapfrog steps
        1.0       // mass scalar
        3.0       // starting clip threshold
        0.1       // min clip
        100.0     // max clip
        50        // window for clip adaptation


printfn "Best params found: %A" bestParams
printfn "Best likelihood: %A" bestLik
