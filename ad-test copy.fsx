#r "nuget: DiffSharp-cpu"

open System
open DiffSharp

// ---------- ODEs ----------
let lotkaVolterra t (y:Tensor) (p:Tensor) =
    let prey, predator = y.[0], y.[1]
    let alpha, beta, delta, gamma = p.[0], p.[1], p.[2], p.[3]
    let dPrey     = alpha * prey - beta * prey * predator
    let dPredator = delta * prey * predator - gamma * predator
    dsharp.stack [dPrey; dPredator]

// ---------- Generic RK4 step ----------
let rk4Step f t y p h =
    let half = dsharp.tensor 0.5
    let sixth = dsharp.tensor (1.0/6.0)
    let two = dsharp.tensor 2.0
    let k1 = f t y p
    let k2 = f (t + h*half) (y + (h*half)*k1) p
    let k3 = f (t + h*half) (y + (h*half)*k2) p
    let k4 = f (t + h) (y + h*k3) p
    y + (h*sixth) * (k1 + two*k2 + two*k3 + k4)

// ---------- Forward RK4 with checkpoints ----------
let rk4WithCheckpoints f t0 y0 (obsTimes: Tensor[]) stepsPerInterval p =
    let stepsF = dsharp.tensor (float stepsPerInterval)
    let checkpoints = ResizeArray()
    let mutable tPrev, yPrev = t0, y0
    checkpoints.Add(tPrev, yPrev)
    for obsT in obsTimes do
        let h = (obsT - tPrev) / stepsF
        for _ in 1 .. stepsPerInterval do
            yPrev <- rk4Step f tPrev yPrev p h
            tPrev <- tPrev + h
        checkpoints.Add(obsT, yPrev)
    checkpoints.ToArray()

// ---------- Jacobian helper ----------
let jac arg f t y p =
    match arg with
    | "y" -> dsharp.jacobian (fun yState -> f t yState p) y
    | "p" -> dsharp.jacobian (fun pVals -> f t y pVals) p
    | _   -> failwith "arg must be 'y' or 'p'"

// ---------- Adjoint step (fixed shapes) ----------
let rk4AdjointStep f t y a p h gP =
    let half = dsharp.tensor 0.5
    let sixth = dsharp.tensor (1.0/6.0)
    let two = dsharp.tensor 2.0
    let adjFun (tA: Tensor) (aState: Tensor) (gState: Tensor) =
        let dFdY = jac "y" f tA y p      // [stateDim; stateDim]
        let dFdP = jac "p" f tA y p      // [stateDim; paramDim]
        // State adjoint derivative: -(dF/dY)^T * a
        let aDot = - dFdY.transpose().matmul(aState.unsqueeze(1)) // [stateDim;1]
        // Parameter gradient contribution: a^T * (dF/dP)
        let gDot = aState.unsqueeze(0).matmul(dFdP)                // [1;paramDim]
        aDot.squeeze(), gState + gDot.squeeze()
    let (k1a, k1g) = adjFun t a gP
    let (k2a, k2g) = adjFun (t - h*half) (a + (h*half)*k1a) (gP + (h*half)*k1g)
    let (k3a, k3g) = adjFun (t - h*half) (a + (h*half)*k2a) (gP + (h*half)*k2g)
    let (k4a, k4g) = adjFun (t - h) (a + h*k3a) (gP + h*k3g)
    a + (h*sixth)*(k1a + two*k2a + two*k3a + k4a),
    gP + (h*sixth)*(k1g + two*k2g + two*k3g + k4g)

// ---------- Adjoint driver ----------
let adjointGradient f rk4Forward t0 y0 obsTimes p likelihoodGrad stepsPerInterval =
    let checkpoints: (Tensor * Tensor)[] = rk4Forward f t0 y0 obsTimes stepsPerInterval p
    let mutable a = likelihoodGrad (snd checkpoints.[checkpoints.Length-1])
    let mutable gP = dsharp.zerosLike p
    for i = checkpoints.Length - 1 downto 1 do
        let tStart = fst checkpoints.[i - 1]
        let (tEnd, yEnd) = checkpoints.[i]
        let h = (tStart - tEnd) / dsharp.tensor 1.0  // negative for backward
        let aNew, gNew = rk4AdjointStep f tEnd yEnd a p h gP
        a <- aNew
        gP <- gNew
    gP

// ---------- Reverse-mode AD ----------
let gradientReverseAD likelihood p = dsharp.grad likelihood p

// ---------- Likelihood ----------
let makeOdeLikelihood f t0 y0 (obsTimes: Tensor[]) stepsPerInterval (yObs: Tensor[]) =
    fun p ->
        let mutable total, tPrev, yPrev = dsharp.tensor 0.0, t0, y0
        for i in 0 .. obsTimes.Length-1 do
            let tNext = obsTimes.[i]
            let checkpoints = rk4WithCheckpoints f tPrev yPrev [| tNext |] stepsPerInterval p
            let yPred = snd checkpoints.[checkpoints.Length-1]
            let diff = yObs.[i] - yPred
            total <- total - dsharp.tensor 0.5 * (diff * diff).sum()
            tPrev <- tNext
            yPrev <- yPred
        total

// ---------- Benchmark helper ----------
let timeIt name f arg =
    let sw = Diagnostics.Stopwatch.StartNew()
    let res = f arg
    sw.Stop()
    printfn "%s: %d ms" name sw.ElapsedMilliseconds
    res

// ---------- Run ----------
let t0 = dsharp.tensor 0.0
let obsTimes = [|1.0 .. 25.0|] |> Array.map dsharp.tensor
let y0LV = dsharp.tensor [|10.0; 5.0|]
let pLV = dsharp.tensor [|1.5; 1.0; 0.75; 1.0|]
let yObsLV =
    obsTimes |> Array.map (fun t -> snd (rk4WithCheckpoints lotkaVolterra t0 y0LV [| t |] 200 pLV).[1])


let stepsPerInterval = 200

let lvLikelihood = makeOdeLikelihood lotkaVolterra t0 y0LV obsTimes stepsPerInterval yObsLV

let _ = timeIt "Lotka–Volterra reverse-mode AD" (gradientReverseAD lvLikelihood) pLV

let _ =
    timeIt "Lotka–Volterra adjoint method"
        (fun p -> adjointGradient lotkaVolterra rk4WithCheckpoints
                                  t0 y0LV obsTimes p
                                  (fun _ -> dsharp.zeros([2]))
                                  stepsPerInterval)
        pLV
