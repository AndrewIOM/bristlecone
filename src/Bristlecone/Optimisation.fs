namespace Bristlecone.Optimisation

open Bristlecone
open Bristlecone.Logging
open Bristlecone.EstimationEngine
open Bristlecone.EstimationEngine.Optimisation

type OptimisationError =
    | OutOfBounds
    | ModelError
    | LikelihoodError

module None =

    /// An optimisation function that calculates the value of `f` using
    /// the given bounds. Use when optimisation of the objective is not required.
    let none: Optimiser =
        InDetachedSpace
        <| fun _ writeOut _ domain _ f ->
            writeOut
            <| GeneralEvent "Skipping optimisation: only the result of the given parameters will be computed"

            let point = [| for (min, _, _) in domain -> min |] |> Tensors.Typed.ofVector
            [ f point |> Tensors.Typed.toFloatScalar, point ]

module EndConditions =

    open Bristlecone.Statistics

    /// End the optimisation procedure when a minimum number of iterations is exceeded.
    let afterIteration iteration : EndCondition =
        fun _ currentIteration -> currentIteration >= iteration

    /// An `EndCondition` that calculates that segregates the most recent n results into
    /// five bins, and runs a regression to detect a temporal variation in the mean
    /// squared jumping distance (MSJD). The significance of the slope coefficient of a linear
    /// regression is assessed to determine if the MSJD is increasing through time for every
    /// parameter sequentially: if all p-values are >0.1, then the `EndCondition` is true.
    let stationarySquaredJumpDistance' fixedBin pointsRequired : EndCondition =
        fun results _ ->
            if (results |> Seq.length) % (fixedBin * pointsRequired) = 0 then
                let trendSignificance =
                    results
                    |> List.take (fixedBin * pointsRequired)
                    |> List.chunkBySize fixedBin
                    |> List.map (fun bin ->
                        bin
                        |> List.map (snd >> Tensors.Typed.toFloatArray >> Array.toList)
                        |> List.flip
                        |> List.map (fun p -> p |> List.map Units.removeUnitFromFloat |> List.pairwise |> List.averageBy (fun (a, b) -> (b - a) ** 2.)))
                    |> List.flip
                    |> List.map (fun msjds ->
                        Regression.pValueForLinearSlopeCoefficient
                            [| 1. .. msjds |> Seq.length |> float |]
                            (msjds |> List.toArray))

                not (
                    trendSignificance |> List.exists (fun p -> p <= 0.1)
                    || trendSignificance.Length = 0
                )
            else
                false

    /// True if there is no significant slope in mean squared jumping distances (MSJD),
    /// binned per 200 iterations and a regression of five bins.
    let stationarySquaredJumpDistance: EndCondition =
        stationarySquaredJumpDistance' 200 5

    /// Convergence of results using the Gelman-Rubin Rhat statistic.
    /// `thin` - Only test for convergence at multiples of the following intervals (when all chains are ready).
    /// `chainCount` - The number of chains to test for convergence. This makes the agent wait until results for all chains are in.
    let convergence thin chainCount : EndCondition =

        let assess (chains: Map<int, Solution list>) =
            printfn "Assessing convergence..."

            if chains.Count < chainCount then
                None
            else
                let shortestChainLength = chains |> Seq.map (fun x -> x.Value.Length) |> Seq.min

                let chainsEqualLength =
                    chains
                    |> Seq.map (fun x -> x.Value |> Seq.skip (x.Value.Length - shortestChainLength))

                let parameterValueByChain =
                    let thetaLength = (chains |> Seq.head).Value |> Seq.head |> snd |> Tensors.Typed.length

                    [ 1..thetaLength ]
                    |> Seq.map (fun p ->
                        chainsEqualLength
                        |> Seq.map (fun chain -> chain |> Seq.map (fun v -> v |> snd |> Tensors.Typed.valueAt (p - 1) |> Units.removeUnitFromFloat)))

                printfn "Param values by chain: %A" parameterValueByChain

                parameterValueByChain
                |> Seq.map Convergence.GelmanRubin.rHat
                |> Seq.toList
                |> Some

        let convergenceAgent =
            MailboxProcessor.Start(fun inbox ->
                let rec messageLoop chainResults =
                    async {
                        let! (chainId, results, repl: AsyncReplyChannel<bool>) = inbox.Receive()
                        let freshResults = chainResults |> Map.add chainId results
                        let rHats = assess freshResults

                        match rHats with
                        | Some r ->
                            printfn "RHats are %A" r

                            let converged =
                                (r |> List.where (fun i -> i < 1.1) |> List.length) = (r |> List.length)

                            printfn
                                "Converged on %i / %i parameters"
                                (r |> List.where (fun i -> i < 1.1) |> List.length)
                                (r |> List.length)

                            repl.Reply(converged)
                        | None -> repl.Reply(false)

                        return! messageLoop freshResults
                    }

                messageLoop Map.empty)

        convergenceAgent.Error.Add(fun e -> printfn "Convergence agent reported an error = %s" e.Message)

        fun results currentIteration ->
            if results.Length % thin = 0 && results.Length >= thin then
                printfn "Sending message to convergence agent at %i" currentIteration
                let threadId = System.Threading.Thread.CurrentThread.ManagedThreadId
                convergenceAgent.PostAndReply(fun reply -> (threadId, results, reply))
            else
                false

module Initialise =

    open Bristlecone.Statistics.Distributions

    /// Generate a random point from bounds specified as a `Domain`.
    /// A value for each dimension is drawn from a univariate normal distribution, assuming that
    /// the bounds represent the 99th percentiles of the distribution.
    let initialise (d: Domain) (rng: System.Random) =
        [| for min, max, _ in d ->
               let min, max = Units.removeUnitFromFloat min, Units.removeUnitFromFloat max
               if min < max then
                   (Normal.draw rng (min + (max - min) / 2.) ((max - min) / 6.)) ()
               elif min = max then
                   min
               else
                   (Normal.draw rng (max + (min - max) / 2.) ((min - max) / 6.)) () |]
        |> Array.map ((*) 1.<``optim-space``>)
        |> Tensors.Typed.ofVector

    /// Assesses if theta is valid based on the provided
    /// constraints.
    let isInvalidTheta theta constraints =
        Seq.zip theta constraints
        |> Seq.map (fun (v, c) ->
            match c with
            | Bristlecone.Parameter.Constraint.Unconstrained -> true
            | Bristlecone.Parameter.Constraint.PositiveOnly -> v > 0.<``optim-space``>)
        |> Seq.contains false

    /// Attempts to generate random theta based on starting bounds
    /// for n tries. If the theta does not meet given constraints, or
    /// `f` evaluates to NaN or an infinity then the algorithm tries again.
    let rec tryGenerateTheta (f:Objective) domain random n : Result<Point, string> =
        if n < 0 then
            Error "Could not generate a starting point given the domain"
        else
            let t = initialise domain random
            let isInvalid = isInvalidTheta (Tensors.Typed.toFloatArray t) (domain |> Seq.map (fun (_, _, c) -> c))

            if isInvalid then
                tryGenerateTheta f domain random (n - 1)
            else
                let result = f t |> Tensors.Typed.toFloatScalar
                if Units.isNan result || Units.isInfinite result then
                    tryGenerateTheta f domain random (n - 1)
                else
                    Ok t


/// A module containing Monte Carlo Markov Chain (MCMC) methods for optimisation.
/// An introduction to MCMC approaches is provided by
/// [Reali, Priami, and Marchetti (2017)](https://doi.org/10.3389/fams.2017.00006)
module MonteCarlo =

    open Bristlecone.Statistics.Distributions
    open MathNet.Numerics.LinearAlgebra
    open Bristlecone.Tensors
    open Bristlecone.ModelSystem

    [<Measure>] type ``jump-scale``

    /// Jump in parameter space while reflecting constraints.
    let constrainJump (initial:float<``optim-space``>) (jump:float<``optim-space``>) (scaleFactor: float) c =
        match c with
        | Bristlecone.Parameter.Constraint.Unconstrained -> initial + (jump * scaleFactor)
        | Bristlecone.Parameter.Constraint.PositiveOnly ->
            if (initial + (jump * scaleFactor)) < 0.<``optim-space``> then
                (initial - (jump * scaleFactor))
            else
                (initial + (jump * scaleFactor))

    /// <summary>A recursive metropolis hastings algorithm that ends when `endCondition` returns true.</summary>
    /// <param name="random">`System.Random` to be used for drawing from a uniform distribution.</param>
    /// <param name="writeOut">side-effect function for handling `LogEvent` items.</param>
    /// <param name="endCondition">`EndCondition` that dictates when the MH algorithm ends.</param>
    /// <param name="propose">proposal `'scale -> 'theta -> 'theta` that generates a jump based on the scale value.</param>
    /// <param name="tune">parameter of type `int -> (float * 'a) list -> 'b -> 'b`, where `int` is current iteration,</param>
    /// <param name="f">an objective function, `'a -> float`, to optimise.</param>
    /// <param name="theta1">initial position in parameter space of type `'a`.</param>
    /// <param name="l1">initial value of -log likelihood at theta1 in parameter space</param>
    /// <param name="d">history of the chain, of type `(float * 'a) list`. Passing a list here allows continuation of a previous analysis.</param>
    /// <param name="scale">a scale of type `'b`, which is compatible with the scale tuning function `tune`</param>
    /// <param name="iteration">the current iteration number</param>
    /// <typeparam name="'a"></typeparam>
    /// <returns>`(float * 'a) list * 'b` - A tuple containing a list of results, and the final scale used in
    /// the analysis. The `(float * 'a) list` represents a list of paired -log likelihood values with
    /// the proposed theta.</returns>
    let rec metropolisHastings' random writeOut endCondition propose tune f (theta1:Point) (l1:TypedTensor<Scalar,``-logL``>) d scale iteration : Solution list * 'a =
        if theta1 |> Tensors.Typed.length = 0 then
            invalidOp "Not valid theta"

        let sc = scale |> tune iteration d
        let theta2 = theta1 |> propose sc |> Typed.ofVector

        if (theta2 |> Typed.length) <> (theta1 |> Typed.length) then
            invalidOp "Theta different length"

        let thetaAccepted, lAccepted =
            let l2 = f theta2

            if l2 < l1 then
                (theta2, l2)
            else
                let l1', l2' = l1 |> Typed.toFloatScalar |> Units.removeUnitFromFloat, l2 |> Typed.toFloatScalar |> Units.removeUnitFromFloat
                let rand = ContinuousUniform.draw random 0. 1. ()
                let ratio = -(l2' - l1') |> exp

                if rand < ratio && l2' <> infinity && l2' <> -infinity && l2' <> nan then
                    (theta2, l2)
                else
                    (theta1, l1)

        if endCondition d iteration then
            (d, sc)
        else
            writeOut
            <| OptimisationEvent
                { Iteration = iteration
                  Likelihood = lAccepted |> Tensors.Typed.toFloatScalar
                  Theta = thetaAccepted |> Tensors.Typed.toFloatArray }

            metropolisHastings'
                random
                writeOut
                endCondition
                propose
                tune
                f
                thetaAccepted
                lAccepted
                ((lAccepted |> Typed.toFloatScalar, thetaAccepted) :: d)
                sc
                (iteration + 1<iteration>)


    module TuningMode =

        /// Find an appropriate scale factor for a standard deviation and its acceptance rate.
        let tuneScale scale accRate =
            match accRate with
            | a when a > 0.95 -> scale * 10.0
            | a when a > 0.75 -> scale * 2.0
            | a when a > 0.50 -> scale * 1.1
            | a when a < 0.001 -> scale * 0.1
            | a when a < 0.05 -> scale * 0.5
            | a when a < 0.20 -> scale * 0.9
            | _ -> scale

        /// Modifies a scale factor depending on
        let scaleFactor tuneInterval remainingIterations history scale =
            if remainingIterations % tuneInterval = 0<iteration> then
                if (history |> Seq.length) * 1<iteration> >= tuneInterval then
                    let acceptance =
                        float (
                            (history
                             |> Seq.take (Units.removeUnitFromInt tuneInterval)
                             |> Seq.pairwise
                             |> Seq.where (fun (x, y) -> x <> y)
                             |> Seq.length)
                        )
                        / (float tuneInterval)

                    tuneScale scale acceptance
                else
                    scale
            else
                scale

        /// Tunes a covariance matrix based on recent samples.
        /// Blend two covariance matrices with a given weight.
        /// Works for any unit of measure 'u.
        let tuneCovariance<[<Measure>] 'u>
            (weight: float)
            (recentCovariance: Matrix<float<'u>>)
            (oldCovariance: Matrix<float<'u>>) : Matrix<float<'u>> =

            recentCovariance.Map(fun v -> v * weight) +
            oldCovariance.Map(fun v -> v * (1. - weight))

        /// Parameters to matrix
        let samplesToMatrix<[<Measure>] 'u> samples : Matrix<float<'u>> =
            samples |> DenseMatrix.ofRowArrays

        /// The starting covariance matrix for parameters in a multivariate distribution
        let defaultCovariance n = Bristlecone.Statistics.LinearAlgebra.covarianceMatrix n 2.38

        /// Generic unit-based covariance-from-bounds
        let covarianceFromBounds
            (n: int)
            (domain: Domain)
            (random: System.Random)
            : Matrix<float<``optim-space`` ^2>> =

            [| 1..n |]
            |> Array.map (fun _ ->
                domain
                |> Array.map (fun (low, high, _) ->
                    // Standard deviation = quarter of the range
                    let stdDev = (high - low) / 4.0
                    // Draw from N(0, stdDev²) with units preserved
                    Normal.draw random 0.<``optim-space``> stdDev ()
                )
            )
            |> samplesToMatrix
            |> Bristlecone.Statistics.LinearAlgebra.computeCovariance

        let covarianceFromSigmas n random sigmas =
            [| 1..n |]
            |> Array.map (fun _ -> sigmas |> Array.map (fun s -> Normal.draw random 0. (s ** 2.) ()))
            |> samplesToMatrix
            |> Bristlecone.Statistics.LinearAlgebra.computeCovariance

        /// Tune previously observed covariance based on most recent period
        let covariance tuneInterval weighting remaining (history: Solution seq) scale =
            if remaining % tuneInterval = 0<iteration> then
                if (history |> Seq.length) * 1<iteration> >= tuneInterval then
                    let sc =
                        history
                        |> Seq.map snd
                        |> Seq.take (Units.removeUnitFromInt tuneInterval)
                        |> Seq.map Tensors.Typed.toFloatArray
                        |> matrix
                        |> Bristlecone.Statistics.LinearAlgebra.computeCovariance
                        |> fun c -> tuneCovariance weighting scale c

                    try
                        sc.Cholesky() |> ignore
                        sc // Ensure the matrix is positive definite
                    with _ ->
                        scale
                else
                    scale
            else
                scale

        let dual tuneInterval weighting remaining history tuningFactors =
            let cov, sc = tuningFactors
            let tunedSc = scaleFactor tuneInterval remaining history sc
            let tunedCov = covariance tuneInterval weighting remaining history cov
            (tunedCov, tunedSc)

        let scaleOnly tuneInterval remaining history tuningFactors =
            let cov, sc = tuningFactors
            let tunedSc = scaleFactor tuneInterval remaining history sc
            (cov, tunedSc)

        let covarianceOnly tuneInterval weighting remaining history tuningFactors =
            let cov, sc = tuningFactors
            let tunedCov = covariance tuneInterval weighting remaining history cov
            (tunedCov, sc)

        /// Tune previously observed covariance based on all time
        let covarianceAllTime weighting (history: Solution seq) scale =
            try
                let sc =
                    history
                    |> Seq.map (snd >> Tensors.Typed.toFloatArray)
                    |> matrix
                    |> Bristlecone.Statistics.LinearAlgebra.computeCovariance
                    |> fun c -> tuneCovariance weighting scale c

                sc.Cholesky() |> ignore // Ensure the matrix is positive definite
                sc
            with _ ->
                scale

        let dualTotalHistory tuneInterval weighting remaining (history: Solution seq) tuningFactors =
            let cov, sc = tuningFactors
            let tunedSc = scaleFactor tuneInterval remaining history sc
            let tunedCov = covarianceAllTime weighting history cov
            (tunedCov, tunedSc)

        let none _ _ factors = factors

    type TuneMethod =
        | Covariance of float
        | Scale
        | CovarianceWithScale of float
        | CovarianceWithScaleTotalHistory of float

    let toFn method (interval:int<iteration>) =
        match method with
        | Covariance w -> TuningMode.covarianceOnly interval w
        | Scale -> TuningMode.scaleOnly interval
        | CovarianceWithScale w -> TuningMode.dual interval w
        | CovarianceWithScaleTotalHistory w -> TuningMode.dualTotalHistory interval w

    type TuneStep = {
        Method: TuneMethod
        Frequency: int<iteration>
        EndCondition: EndCondition
    }

    module RandomWalk =

        let proposeJump
            (sample: Matrix<float<``optim-space``^2>> -> unit -> Vector<float<``optim-space``>>)
            (domain: Domain)
            (cov: Matrix<float<``optim-space``^2>>, scale: float)
            (theta: Point) : float<``optim-space``>[] =

            let dim = theta |> Typed.length |> float
            sample (cov * (2.38 ** 2.0) / dim) ()
            |> Vector.toArray
            |> Array.map (fun step -> step * scale)
            |> Array.mapi (fun i step -> (step, domain.[i]))
            |> Array.zip (theta |> Typed.toFloatArray)
            |> Array.map (fun (thetai, (zi, (_, _, con))) -> constrainJump thetai zi scale con)

        let randomWalk'
            initialCovariance
            initialScale
            theta
            (tuningSteps: seq<TuneStep>)
            random
            writeOut
            (endCondition: EndCondition)
            (domain: Domain)
            (f: Objective)
            =
            writeOut <| GeneralEvent(sprintf "[Optimisation] Starting MCMC Random Walk")
            let sample cov = MultivariateNormal.sample cov random
            tuningSteps
            |> Seq.fold
                (fun (s, sc) tuneStep ->
                    let l, t = s |> Seq.head
                    let currenti = List.length s * 1<iteration>
                    metropolisHastings' random writeOut tuneStep.EndCondition (proposeJump sample domain) (toFn tuneStep.Method tuneStep.Frequency) f t (l |> Typed.ofScalar) s sc currenti)
                ([ f theta |> Typed.toFloatScalar, theta ], (initialCovariance, initialScale))
            ||> fun r s ->
                let l, t = r |> Seq.head
                let currenti = r.Length * 1<iteration>
                metropolisHastings' random writeOut endCondition (proposeJump sample domain) TuningMode.none f t (l |> Typed.ofScalar) r s currenti

        let randomWalk (tuningSteps: seq<TuneStep>) : Optimise =
            fun random writeOut n domain startPoint f ->
                let initialCovariance = TuningMode.covarianceFromBounds 10000 domain random

                match Initialise.tryGenerateTheta f domain random 10000 with
                | Ok theta ->
                    writeOut <| GeneralEvent(sprintf "[Optimisation] Initial theta is %A" theta)

                    randomWalk' initialCovariance 1. theta tuningSteps random writeOut n domain f
                    |> fst
                | Error _ -> invalidOp "Could not generate theta"


    /// A Markov Chain Monte Carlo (MCMC) sampling algorithm that randomly 'walks'
    /// through a n-dimensional posterior distribution of the parameter space.
    /// Specify `tuningSteps` to prime the jump size before random walk.
    let randomWalk (tuningSteps: seq<TuneStep>) : Optimiser =
        InDetachedSpace <| RandomWalk.randomWalk tuningSteps

    /// A Markov Chain Monte Carlo (MCMC) sampling algorithm that continually adjusts the
    /// covariance matrix based on the recently-sampled posterior distribution. Proposed
    /// jumps are therefore tuned to the recent history of accepted jumps.
    let adaptiveMetropolis weighting period : Optimiser =
        InDetachedSpace
        <| fun random writeOut n domain startPoint f ->
            RandomWalk.randomWalk
                [ { Method = CovarianceWithScale weighting; Frequency = period; EndCondition = n } ]
                random
                writeOut
                (EndConditions.afterIteration 0<iteration>)
                domain
                startPoint
                f


    module MetropolisWithinGibbs =

        [<Measure>] type batch

        /// Log of the standard deviation (unitless)
        type LogSigma = float

        module Core =

            open Bristlecone.Statistics

            /// Propose a jump drawn from a Normal(0, exp(lsj)²) distribution,
            /// while leaving all but one parameter value fixed.
            let propose
                (theta: float<``optim-space``>[])
                (j: int)
                (lsj: LogSigma)
                (domain: Domain)
                (random: System.Random) : float<``optim-space``>[] =

                theta
                |> Array.mapi (fun i e ->
                    if i = j then
                        let stdev = exp lsj * 1.0<``optim-space``>
                        let jump = Normal.draw<``optim-space``> random 0.0<``optim-space``> stdev ()
                        (e, jump)
                    else
                        (e, 0.0<``optim-space``>)
                )
                |> Array.zip domain
                |> Array.map (fun ((_, _, con), (e, jump)) -> constrainJump e jump 1. con)

            /// Tune variance of a parameter based on its acceptance rate.
            /// The magnitude of tuning reduces as more batches have been run.
            let tune (ls: LogSigma) (acceptanceRate: float) (batchNumber: int<batch>) : LogSigma =
                let delta = 1.0 / float batchNumber
                if acceptanceRate < 0.44 then ls - delta
                else ls + delta

            /// Run MH updates for a single parameter index
            let updateParameter
                j
                (lsj: LogSigma)
                theta
                domain
                random
                f
                batchLength
                (results:Solution list) =
                let proposeJump _ t = propose (t |> Typed.toFloatArray) j lsj domain random
                metropolisHastings'
                    random
                    ignore
                    (EndConditions.afterIteration batchLength)
                    proposeJump
                    TuningMode.none
                    f
                    theta
                    (fst (List.head results) |> Typed.ofScalar)
                    []
                    ()
                    (List.length results * 1<iteration>)
                |> fst

            /// Compute acceptance rates per parameter
            let acceptanceRates ds batchLength =
                ds
                |> Array.map (fun results ->
                    let accepted =
                        results |> List.pairwise |> List.where (fun (x, y) -> x <> y) |> List.length
                    float accepted / float (Units.removeUnitFromInt batchLength))

            /// Adaptive tuning step
            let adaptiveStep (sigmas: LogSigma[]) acceptanceRates batchNumber : LogSigma[] =
                acceptanceRates
                |> Array.zip sigmas
                |> Array.map (fun (ls, ar) -> tune ls ar batchNumber)

            /// Trend check step (non‑adaptive)
            let trendCheckStep (fullResults: Solution list) (batchLength: int<iteration>) paramCount =
                fullResults
                |> List.chunkBySize (batchLength / 1<iteration>)
                |> List.mapi (fun i batch ->
                    let paramNumber = i % paramCount
                    let paramValues = batch |> List.map snd |> List.averageBy (fun x -> Typed.valueAt paramNumber x)
                    (paramNumber, paramValues))
                |> List.groupBy fst
                |> List.where (fun (_, g) -> g.Length >= 5)
                |> List.map (fun (_, p) ->
                    let x, y =
                        p
                        |> List.take 5
                        |> List.map snd
                        |> List.mapi (fun i v -> (float i, float v)) // strip units
                        |> List.toArray
                        |> Array.unzip
                    Regression.pValueForLinearSlopeCoefficient x y)


        /// Adaptive-metropolis-within-Gibbs algorithm, which can work in both adaptive and fixed modes.
        /// Adaptive mode: computes per‑parameter acceptance rates and tunes sigmas accordingly,
        /// repeating until all acceptance rates are in the target range.
        /// Non‑adaptive mode: checks for linear trends in parameter values over batches
        /// (via regression p‑values) and repeats until no significant trends remain.
        let rec core isAdaptive writeOut random domain f (results:Solution list) (batchLength:int<iteration>) (batchNumber: int<batch>) (theta:Point) (sigmas: LogSigma[]) =
            let ds =
                sigmas
                |> Array.mapi (fun j logSigma ->
                    Core.updateParameter j logSigma theta domain random f batchLength results)

            let d = ds |> Array.rev |> List.concat
            let fullResults = d @ results

            writeOut <| OptimisationEvent {
                Iteration = List.length d * 1<iteration>
                Likelihood = d |> List.head |> fst
                Theta = d |> List.head |> snd |> Typed.toFloatArray
            }

            match isAdaptive with
            | true ->
                let ar = Core.acceptanceRates ds batchLength
                let tunedSigmas = Core.adaptiveStep sigmas ar batchNumber
                writeOut <| GeneralEvent(sprintf "[Tuning] Sigmas: %A | Acceptance rates: %A" tunedSigmas ar)
                if ar |> Array.exists (fun s -> s < 0.28 || s > 0.60) then
                    core isAdaptive writeOut random domain f fullResults batchLength (batchNumber + 1<batch>) (fullResults |> Seq.head |> snd) tunedSigmas
                else
                    (batchNumber, fullResults, tunedSigmas)
            | false ->
                let dims = Typed.length theta
                let pValues = Core.trendCheckStep fullResults batchLength dims
                writeOut <| GeneralEvent(sprintf "Linear trend p-values: %A" pValues)
                if pValues |> List.exists (fun p -> p <= 0.1) || pValues.IsEmpty then
                    core isAdaptive writeOut random domain f fullResults batchLength (batchNumber + 1<batch>) (fullResults |> Seq.head |> snd) sigmas
                else
                    (batchNumber, fullResults, sigmas)


    /// An adaptive Metropolis-within-Gibbs sampler that tunes the variance of
    /// each parameter according to the per-parameter acceptance rate.
    /// Reference: Bai Y (2009). “An Adaptive Directional Metropolis-within-Gibbs Algorithm.”
    /// Technical Report in Department of Statistics at the University of Toronto.
    let ``Adaptive-Metropolis-within Gibbs``: Optimiser =
        InDetachedSpace
        <| fun random writeOut endCon domain startPoint f ->
            match Initialise.tryGenerateTheta f domain random 10000 with
            | Ok theta ->
                writeOut <| GeneralEvent(sprintf "[Optimisation] Initial theta is %A" theta)
                let sigmas = theta |> Array.map (fun _ -> 0.)

                let _, result, _ =
                    MetropolisWithinGibbs.core true writeOut random domain f [] 100 1 (theta: float[]) sigmas

                result
            | Error _ -> invalidOp "Could not generate theta"

    /// A non-adaptive Metropolis-within-gibbs Sampler. Each parameter is updated
    /// individually, unlike the random walk algorithm.
    let ``Metropolis-within Gibbs``: Optimiser =
        InDetachedSpace
        <| fun random writeOut endCon domain startPoint (f: Objective) ->
            let theta = Initialise.initialise domain random
            let sigmas = theta |> Array.map (fun _ -> 0.)

            let _, result, _ =
                MetropolisWithinGibbs.core false writeOut random domain f [] 100 1 (theta: float[]) sigmas

            result

    /// Implementation similar to that proposed by Yang and Rosenthal: "Automatically Tuned
    /// General-Purpose MCMC via New Adaptive Diagnostics"
    /// Reference: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.70.7198&rep=rep1&type=pdf
    let ``Automatic (Adaptive Diagnostics)``: Optimiser =
        InDetachedSpace
        <| fun random writeOut endCon domain startPoint (f: Objective) ->

            // Starting condition
            let initialTheta = Initialise.initialise domain random
            let initialSigma = Array.init (Typed.length initialTheta) (fun _ -> 0.) |> Typed.ofVector

            let mwg adapt batchSize currentBatch theta sigmas =
                MetropolisWithinGibbs.core adapt writeOut random domain f [] batchSize currentBatch theta sigmas

            // 1st Adaptive Phase
            writeOut <| GeneralEvent "Generalised MCMC: Starting 1st Adaptive Phase"

            let batches, results, tunedSigmas =
                initialSigma
                |> mwg true 100<iteration> 1<iteration> initialTheta
                |> fun (b, r, s) ->
                    let _, theta = r |> Seq.head
                    mwg true 200<iteration> b theta s
                |> fun (b, r, s) ->
                    let _, theta = r |> Seq.head
                    mwg true 400<iteration> b theta s
                // Transient phase
                |> fun (b, r, s) ->
                    writeOut <| GeneralEvent "Generalised MCMC: Starting Transient Phase"
                    let _, theta = r |> Seq.head
                    mwg false 200<iteration> b theta s

            // 2nd Adaptive Phase
            // a) Compute a covariance matrix from the previous sigma values
            let covariance =
                tunedSigmas |> Array.map exp |> TuningMode.covarianceFromSigmas 10000 random

            // b) Continue the chain using the covariance matrix and last theta
            writeOut <| GeneralEvent "Generalised MCMC: Starting 2nd Adaptive Phase"

            let secondAdaptation, finalScale =
                RandomWalk.randomWalk'
                    covariance
                    1.
                    (results |> Seq.head |> snd)
                    [ (TuneMethod.CovarianceWithScaleTotalHistory 0.750,
                       200,
                       EndConditions.stationarySquaredJumpDistance) ]
                    random
                    writeOut
                    (EndConditions.afterIteration 0<iteration>)
                    domain
                    f

            // 3. Burn-in and sampling
            writeOut
            <| GeneralEvent "Generalised MCMC: Starting Sampling Phase (random walk MCMC, with burnin and clean trace)"

            RandomWalk.randomWalk'
                (finalScale |> fst)
                (finalScale |> snd)
                (secondAdaptation |> Seq.head |> snd)
                []
                random
                writeOut
                endCon
                domain
                f
            |> fst


    /// A meta-heuristic that approximates a global optimium by
    /// simulating slow cooling as a slow decrease in the probability
    /// of temporarily accepting worse solutions.
    module SimulatedAnnealing =

        /// Cooling schemes dictate the conditions under which the temperature is cooled
        /// during simulated annealing.
        module CoolingSchemes =

            /// Commonly set alpha to 0.95.
            /// Tk is the temperature after k cooling iterations.
            let exponential alpha t0 t k : float = alpha * t

            let fastCauchyCoolingSchedule (t0: float) t k = t0 / (float k)


        module Machines =

            /// e = new minus old energy (or -logL)
            let boltzmann t e : float = -e / t |> exp


        module EndConditions =

            let defaultTolerance = 1e-06<``-logL``>

            /// Given a list of solutions, which are ordered most recent first,
            /// returns `true` if there are at least `chains` recent results, and
            /// the change within the recent results is no more than `tolerance`.
            let stoppedImproving chains (minimums: Solution list) =
                if minimums |> List.length < chains then
                    false
                else
                    (minimums
                     |> List.map fst
                     |> List.take chains
                     |> List.pairwise
                     |> List.sumBy (fun (n, old) -> max 0.<``-logL``> (old - n)) // Captures backwards jumps
                     |> (fun x ->
                         printfn "Better by %f" x
                         x))
                    <= defaultTolerance

            let noImprovement (chain: Solution list) =
                (chain
                 |> List.map fst
                 |> List.pairwise
                 |> List.averageBy (fun (n, old) -> if n > old then n - old else old - n) // Mean change in objective value
                 |> (fun x ->
                     printfn "Average jump is %f" x
                     x))
                <= defaultTolerance

            let improvementCount count interval : EndCondition =
                fun results iteration ->
                    if iteration % interval = 0<iteration> then
                        (results |> List.pairwise |> List.where (fun (x, y) -> x < y) |> List.length)
                        >= count
                    else
                        false

        /// Represents configurable settings of an annealing procedure
        /// that supports (a) heating, followed by (b) annealing.
        type AnnealSettings =
            { HeatStepLength: EndCondition
              HeatRamp: float -> float
              TemperatureCeiling: float option
              BoilingAcceptanceRate: float
              InitialTemperature: float
              PreTuneLength: int<iteration>
              TuneLength: int<iteration>
              TuneN: int<iteration>
              AnnealStepLength: EndCondition }

            static member Default =
                { HeatStepLength = EndConditions.afterIteration 250<iteration>
                  HeatRamp = fun t -> t * 1.10
                  BoilingAcceptanceRate = 0.85
                  TemperatureCeiling = Some 200.
                  InitialTemperature = 1.00
                  PreTuneLength = 5000<iteration>
                  TuneLength = 100000<iteration>
                  TuneN = 50<iteration>
                  AnnealStepLength = EndConditions.improvementCount 250 250<iteration> }

        /// Jump based on a proposal function and probability function
        let tryMove propose probability random (f:Objective) tries (l1, theta1) : Solution option =
            let rec catchNan tries =
                let theta2 = theta1 |> propose
                let l2 = f theta2 |> Typed.toFloatScalar

                if Units.isNan l2 then
                    if tries <= 1 then None else catchNan (tries - 1)
                else
                    Some(theta2, l2)

            catchNan tries
            |> Option.map (fun (theta2, l2) ->
                if l2 < l1 then
                    (l2, theta2)
                else
                    let rand = ContinuousUniform.draw random 0. 1. ()
                    let ratio = probability (l2 - l1)
                    if rand < ratio then (l2, theta2) else (l1, theta1))

        /// Run a homogenous Markov chain recursively until an end condition - `atEnd` - is met.
        let rec markovChain writeOut atEnd propose probability random f temperature initialPoint =
            let propose' = tryMove propose (temperature |> probability) random f 100

            let rec run (point:Solution) d iteration =
                let newPoint = point |> propose'

                match newPoint with
                | None ->
                    writeOut
                    <| GeneralEvent(
                        sprintf "Abandoning chain at iteration %i as could not move after 100 tries." iteration
                    )

                    d
                | Some newPoint ->
                    if newPoint |> fst |> Units.isNan then
                        run point d iteration
                    else
                        let state = newPoint :: d

                        if atEnd state iteration then
                            state
                        else
                            writeOut
                            <| OptimisationEvent
                                { Iteration = iteration
                                  Likelihood = newPoint |> fst
                                  Theta = newPoint |> snd |> Typed.toFloatArray }

                            run (state |> Seq.head) state (iteration + 1<iteration>)

            run initialPoint [ initialPoint ] 1<iteration>

        /// Cool between homoegenous markov chains according to `cool` schedule.
        /// Each anneal recursion begins from the end of the previous markov chain.
        let rec anneal writeOut chainEnd annealEnd cool markov temperature point previousBests =
            let results = point |> markov chainEnd temperature
            let bestAtTemperature = results |> List.minBy fst
            let history = bestAtTemperature :: previousBests

            if (* annealEnd history *) temperature < 0.75 then
                history
            else
                writeOut
                <| GeneralEvent(
                    sprintf "[Annealing] Best point is %f at temperature %f" (bestAtTemperature |> fst) temperature
                )

                anneal
                    writeOut
                    chainEnd
                    annealEnd
                    cool
                    markov
                    (cool temperature (history |> List.length))
                    bestAtTemperature (*(results |> List.head)*)
                    history

        /// Heat up temperature until acceptance rate of bad moves is above the threshold `endAcceptanceRate`.
        /// If it becomes impossible to propose a move during heating, then heating ends.
        let rec heat write endCondition ceiling endAcceptanceRate heatingSchedule markov bestTheta temperature =
            let chain = markov endCondition temperature bestTheta
            let min = chain |> List.minBy fst

            let ar =
                let a, r =
                    chain
                    |> List.map fst
                    |> List.pairwise
                    |> List.map (fun (x, y) -> ((x > y), (x = y))) // Jumped back, bad jump rejected
                    |> List.unzip

                let badAccepted = a |> List.where id |> List.length |> float
                let badRejected = r |> List.where id |> List.length |> float
                badAccepted / (badAccepted + badRejected)

            write
            <| GeneralEvent(
                sprintf
                    "Heating - Jump average is %f"
                    (chain |> List.map fst |> List.pairwise |> List.averageBy (fun (a, b) -> b - a))
            )

            write
            <| GeneralEvent(sprintf "Heating (T=%f) - AR of bad moves is %f" temperature ar)

            let aboveCeiling =
                if Option.isSome ceiling then
                    (temperature >= ceiling.Value)
                else
                    false

            if ar < endAcceptanceRate && not aboveCeiling then
                heat
                    write
                    endCondition
                    ceiling
                    endAcceptanceRate
                    heatingSchedule
                    markov
                    min
                    (temperature |> heatingSchedule)
            else
                (temperature, min)

        // Given a candidate distribution + machine, run base SA algorithm
        let simulatedAnnealing
            scale
            settings
            annealEnd
            machine
            (jump: System.Random -> float -> float -> unit -> float<``optim-space``>)
            cool
            random
            writeOut
            domain
            startPoint
            f
            =

            // 1. Initial conditions
            let draw' = jump random
            let theta1 = Initialise.initialise domain random
            let l1 = f theta1

            // 2. Chain generator
            let homogenousChain (scales:float[]) e temperature =
                let propose scale (theta: Point) =
                    Array.zip3 (theta |> Typed.toFloatArray) scale domain
                    |> Array.map (fun (x, sc, (_, _, con)) -> constrainJump x (draw' sc temperature ()) 1. con)
                    |> Typed.ofVector

                markovChain writeOut e (propose scales) machine random f temperature

            // 3. Tune individual step size based on acceptance rate
            let initialScale = [| 1 .. theta1 |> Typed.length |] |> Array.map (fun _ -> scale)

            let kMax = settings.TuneLength

            let rec tune (p: (float * float[])[]) k (l1, theta1) =
                let chance = (float k) / (float kMax)
                let parameterToChange = random.Next(0, (p |> Array.length) - 1)

                let scalesToChange =
                    p
                    |> Array.mapi (fun i x -> (x, random.NextDouble() < chance || i = parameterToChange))

                let propose (theta:Point) =
                    Array.zip3 (theta |> Typed.toFloatArray) scalesToChange domain
                    |> Array.map (fun (x, ((ti, n), shouldChange), (_, _, con)) ->
                        if shouldChange then
                            constrainJump x (draw' ti 1. ()) 1. con
                        else
                            x)
                    |> Typed.ofVector

                let result = tryMove propose (machine 1.) random f 100 (l1, theta1)

                if result.IsNone then
                    failwith "Could not move in parameter space."

                let newScaleInfo =
                    scalesToChange
                    |> Array.zip (result.Value |> snd |> Typed.toFloatArray)
                    |> Array.map (fun (v, ((ti, previous), changed)) ->
                        let ti, previous =
                            if changed then
                                (ti, (previous |> Array.append [| v |])) // Append new parameter values to previous ones
                            else
                                (ti, previous)

                        if Array.length previous * 1<iteration> = settings.TuneN then
                            let changes =
                                previous |> Array.pairwise |> Array.where (fun (a, b) -> a <> b) |> Array.length

                            match (float changes) / (float settings.TuneN) with
                            | ar when ar < 0.35 -> (ti * 0.80, Array.empty)
                            | ar when ar > 0.50 -> (ti * 1.20, Array.empty)
                            | _ -> (ti, Array.empty)
                        else
                            (ti, previous))

                if k % 1000<iteration> = 0<iteration> then
                    writeOut
                    <| GeneralEvent(
                        sprintf
                            "Tuning is at %A (k=%i/%i) [-logL %f]"
                            (newScaleInfo |> Array.map fst)
                            k
                            kMax
                            (result.Value |> fst)
                    )

                if k < kMax then
                    tune newScaleInfo (k + 1<iteration>) result.Value
                else
                    (newScaleInfo |> Array.map fst, l1, theta1)

            let tunedScale, l2, theta2 =
                homogenousChain initialScale (EndConditions.afterIteration settings.PreTuneLength) 1. (l1, theta1)
                |> List.minBy fst
                |> tune (initialScale |> Array.map (fun t -> (t, Array.empty))) 1<iteration>

            writeOut <| GeneralEvent(sprintf "Tuned = %A" tunedScale)

            // 4. Heat up
            let boilingPoint, min =
                heat
                    writeOut
                    settings.HeatStepLength
                    settings.TemperatureCeiling
                    settings.BoilingAcceptanceRate
                    settings.HeatRamp
                    (homogenousChain tunedScale)
                    (l2, theta2)
                    settings.InitialTemperature

            // 5. Gradually cool down (from best point during heat-up)
            anneal
                writeOut
                settings.AnnealStepLength
                annealEnd
                (cool boilingPoint)
                (homogenousChain tunedScale)
                boilingPoint
                min
                []

        /// Candidate distribution: Gaussian univariate []
        /// Probability: Boltzmann Machine
        let classicalSimulatedAnnealing scale tDependentProposal settings : Optimiser =
            InDetachedSpace
            <| fun random writeOut endCon domain startPoint (f: Objective) ->
                let gaussian rnd scale t =
                    let s = if tDependentProposal then scale * (sqrt t) else scale
                    fun () ->
                        Bristlecone.Statistics.Distributions.Normal.draw rnd 0. s ()
                        |> (*) 1.<``optim-space``>

                simulatedAnnealing
                    scale
                    settings
                    endCon
                    Machines.boltzmann
                    gaussian
                    (CoolingSchemes.exponential 0.05)
                    random
                    writeOut
                    domain
                    startPoint
                    f

        /// Candidate distribution: Cauchy univariate []
        /// Probability: Bottzmann Machine
        let fastSimulatedAnnealing scale tDependentProposal settings : Optimiser =
            InDetachedSpace
            <| fun random writeOut endCon domain startPoint (f: Objective) ->
                let cauchy rnd scale t =
                    let s = if tDependentProposal then scale * (sqrt t) else scale
                    let c = MathNet.Numerics.Distributions.Cauchy(0., s, rnd)
                    fun () -> c.Sample() * 1.<``optim-space``>

                simulatedAnnealing
                    scale
                    settings
                    endCon
                    Machines.boltzmann
                    cauchy
                    (CoolingSchemes.fastCauchyCoolingSchedule)
                    random
                    writeOut
                    domain
                    startPoint
                    f


//     /// An adaptation of the Filzbach method (originally by Drew Purves)
//     module Filzbach =

//         type FilzbachSettings<'a> =
//             { TuneAfterChanges: int
//               MaxScaleChange: float
//               MinScaleChange: float
//               BurnLength: EndCondition }

//             static member Default =
//                 { TuneAfterChanges = 50
//                   MaxScaleChange = 100.00
//                   MinScaleChange = 0.0010
//                   BurnLength = EndConditions.afterIteration 2000 }

//         let filzbach'
//             settings
//             (theta: Point)
//             random
//             writeOut
//             (sampleEnd: EndCondition)
//             (domain: Domain)
//             (f: Objective)
//             =
//             writeOut
//             <| GeneralEvent(sprintf "[Optimisation] Starting Filzbach-style MCMC optimisation")

//             let sample sd = Normal.draw random 0. sd

//             let scaleRnd =
//                 Bristlecone.Statistics.Distributions.ContinuousUniform.draw random 0. 1.

//             let paramRnd =
//                 MathNet.Numerics.Distributions.DiscreteUniform(0, (theta |> Typed.length) - 1, random)

//             let initialScale = domain |> Array.map (fun (l, u, _) -> (u - l) / 6.)
//             printfn "Initial scale: %A" initialScale
//             let l1 = f theta

//             let rec step burning (p: (float * float[])[]) endWhen (l1, theta1) d currentIteration =
//                 // Change one to many parameter at once (Filzbach-style)
//                 let scalesToChange =
//                     if scaleRnd () < 0.670 then
//                         // Choose one parameter to change
//                         let rnd = paramRnd.Sample()
//                         // Change also nearby parameters with probability 1/2:
//                         p
//                         |> Array.mapi (fun i x ->
//                             (x,
//                              if i = rnd then true
//                              else if i = (rnd - 1) then scaleRnd () < 0.5
//                              else if i = (rnd + 1) then scaleRnd () < 0.5
//                              else false))
//                     else
//                         // Probability of change:
//                         let pChange = exp (4.0 * (scaleRnd () - 0.50))
//                         // Try and allocate random changes to array
//                         let rec changeRandom p =
//                             let r = p |> Array.mapi (fun i x -> (x, scaleRnd () < pChange))

//                             if r |> Array.where (fun (_, b) -> b) |> Array.isEmpty then
//                                 changeRandom p
//                             else
//                                 r

//                         changeRandom p

//                 let propose theta =
//                     Array.zip3 theta scalesToChange domain
//                     |> Array.map (fun (x, ((ti, n), shouldChange), (_, _, con)) ->
//                         if shouldChange then
//                             constrainJump x (sample ti () * 1.<``optim-space``>) 1. con
//                         else
//                             x)

//                 // Metropolis step here
//                 let result =
//                     SimulatedAnnealing.tryMove
//                         propose
//                         (SimulatedAnnealing.Machines.boltzmann 1.)
//                         random
//                         f
//                         100
//                         (l1, theta1)

//                 if Option.isNone result then
//                     failwith "Could not move in parameter space."
//                 // End metropolis step

//                 // Tune Scales (burnin only)
//                 let newScaleInfo =
//                     if not burning then
//                         p
//                     else
//                         scalesToChange
//                         |> Array.zip (result.Value |> snd)
//                         |> Array.mapi (fun parameteri (v, ((ti, previous), changed)) ->
//                             let ti, previous =
//                                 if changed then
//                                     (ti, (previous |> Array.append [| v |])) // Append new parameter values to previous ones
//                                 else
//                                     (ti, previous)

//                             if previous |> Array.length = settings.TuneAfterChanges then
//                                 let changes =
//                                     previous |> Array.pairwise |> Array.where (fun (a, b) -> a <> b) |> Array.length

//                                 match (float changes) / (float settings.TuneAfterChanges) with
//                                 | ar when ar < 0.25 ->
//                                     if (ti * 0.80) < (initialScale.[parameteri] * settings.MinScaleChange) then
//                                         (initialScale.[parameteri] * settings.MinScaleChange, Array.empty)
//                                     else
//                                         (ti * 0.80, Array.empty)
//                                 | ar when ar > 0.25 ->
//                                     if (ti * 1.20) > (initialScale.[parameteri] * settings.MaxScaleChange) then
//                                         (initialScale.[parameteri] * settings.MaxScaleChange, Array.empty)
//                                     else
//                                         (ti * 1.20, Array.empty)
//                                 | _ -> (ti, Array.empty)
//                             else
//                                 (ti, previous))
//                 // End Tune Scales (burnin only)

//                 let newResult = result.Value :: d

//                 if endWhen d currentIteration then
//                     (newResult, newScaleInfo |> Array.map fst)
//                 else
//                     writeOut
//                     <| OptimisationEvent
//                         { Iteration = currentIteration
//                           Likelihood = result.Value |> fst
//                           Theta = result.Value |> snd }

//                     step burning newScaleInfo endWhen result.Value newResult (currentIteration + 1)

//             writeOut
//             <| GeneralEvent(sprintf "[Filzbach] Starting burn-in at point %A (L = %f)" theta l1)

//             let burnResults, burnScales =
//                 step true (initialScale |> Array.map (fun x -> x, Array.empty)) settings.BurnLength (l1, theta) [] 0

//             writeOut
//             <| GeneralEvent(
//                 sprintf "[Filzbach] Burn-in complete. Starting sampling at point %A" (burnResults |> Seq.head)
//             )

//             let results, _ =
//                 step
//                     false
//                     (burnScales |> Array.map (fun t -> (t, Array.empty)))
//                     sampleEnd
//                     (burnResults |> Seq.head)
//                     []
//                     0

//             [ results; burnResults ] |> List.concat

//         /// A Monte Carlo Markov Chain sampler based on the 'Filzbach' algorithm from
//         /// Microsoft Research Cambridge.
//         let filzbach settings : Optimiser =
//             InDetachedSpace
//             <| fun random writeOut endCon domain startPoint (f: Objective) ->
//                 match startPoint with
//                 | Some theta ->
//                     writeOut
//                     <| GeneralEvent(sprintf "[Optimisation] Pre-defined initial theta is %A" theta)

//                     filzbach' settings theta random writeOut endCon domain f
//                 | None ->
//                     match Initialise.tryGenerateTheta f domain random 10000 with
//                     | Ok theta ->
//                         writeOut <| GeneralEvent(sprintf "[Optimisation] Initial theta is %A" theta)
//                         filzbach' settings theta random writeOut endCon domain f
//                     | Error _ -> invalidOp "Could not generate theta"


// /// Nelder Mead implementation
// /// Adapted from original at: https://github.com/mathias-brandewinder/Amoeba
// module Amoeba =

//     module Solver =

//         type Amoeba =
//             { Dim: int
//               Solutions: Solution<float>[] }

//             member this.Size = this.Solutions.Length

//             member this.Best = this.Solutions.[0]

//             member this.Worst = this.Solutions.[this.Size - 1]

//         type Settings =
//             { Alpha: float
//               Sigma: float
//               Gamma: float
//               Rho: float
//               Size: int }

//         let Default =
//             { Alpha = 1.
//               Sigma = 0.5
//               Gamma = 2.
//               Rho = (-0.5)
//               Size = 3 }

//         let evaluate (f: Objective) (x: Point) = (f x, x)
//         let valueOf (s: Solution) = fst s

//         let replace (a: Amoeba) (s: Solution) =
//             let last = a.Size - 1
//             let a' = Array.copy a.Solutions
//             a'.[last] <- s

//             { a with
//                 Solutions = a' |> Array.sortBy fst }

//         let centroid (a: Amoeba) =
//             [| for d in 0 .. (a.Dim - 1) -> (a.Solutions.[0 .. a.Size - 2] |> Array.averageBy (fun (_, x) -> x.[d])) |]

//         let stretch ((X, Y): Point<float> * Point<float>) (s: float) =
//             Array.map2 (fun x y -> x + s * (x - y)) X Y

//         let reflected v s = stretch v s.Alpha

//         let expanded v s = stretch v s.Gamma

//         let contracted v s = stretch v s.Rho

//         let shrink (a: Amoeba) (f: Objective<Point<float>, float>) s =
//             let best = snd a.Best

//             { a with
//                 Solutions =
//                     a.Solutions
//                     |> Array.map (fun p -> stretch (best, snd p) -s.Sigma)
//                     |> Array.map (evaluate f) }

//         let update (a: Amoeba) (f: Objective<Point<float>, float>) (s: Settings) =
//             let cen = centroid a
//             let rv, r = reflected (cen, (snd a.Worst)) s |> evaluate f

//             if ((valueOf (a.Best) <= rv) && (rv < (valueOf (a.Solutions.[a.Size - 2])))) then
//                 replace a (rv, r)
//             else if (rv < valueOf (a.Best)) then
//                 let ev, e = expanded (cen, r) s |> evaluate f
//                 if (ev < rv) then replace a (ev, e) else replace a (rv, r)
//             else
//                 let (cv, c) = contracted (cen, (snd a.Worst)) s |> evaluate f

//                 if (cv < valueOf (a.Worst)) then
//                     replace a (cv, c)
//                 else
//                     shrink a f s

//         let solve settings rng writeOut atEnd domain _ f =
//             let dim = Array.length domain

//             let start =
//                 [| for _ in 1 .. settings.Size -> Initialise.tryGenerateTheta f domain rng 1000 |]
//                 |> Array.map (fun r ->
//                     match r with
//                     | Ok r -> r
//                     | Error _ -> failwith "Could not generate theta")
//                 |> Array.map (evaluate f)
//                 |> Array.sortBy fst

//             let amoeba = { Dim = dim; Solutions = start }

//             let rec search i trace atEnd (a: Amoeba) =
//                 if not <| atEnd trace i then
//                     // writeOut <| OptimisationEvent { Iteration = i; Likelihood = trace; Theta = thetaAccepted }
//                     search (i + 1) (a.Best :: trace) atEnd (update a f settings)
//                 else
//                     writeOut <| GeneralEvent(sprintf "Solution: -L = %f" (fst a.Solutions.[0]))
//                     a.Solutions |> Array.toList

//             search 0 [] atEnd amoeba


//         let rec swarm
//             settings
//             rng
//             logger
//             numberOfLevels
//             iterationsPerLevel
//             numberOfAmoeba
//             (paramBounds: Domain)
//             startPoint
//             (f: Objective<Point<float>, float>)
//             =

//             let amoebaResults =
//                 [| 1..numberOfAmoeba |]
//                 |> Array.collect (fun _ ->
//                     try
//                         [| solve settings rng logger iterationsPerLevel paramBounds startPoint f |]
//                     with e ->
//                         logger
//                         <| GeneralEvent(
//                             sprintf
//                                 "Warning: Could not generate numercal solution for point (with EXN %s): %A"
//                                 e.Message
//                                 paramBounds
//                         )

//                         [||])

//             // Drop worst 20% of likelihoods
//             let mostLikely = amoebaResults |> Array.map List.head |> Array.minBy fst

//             let percentile80thRank =
//                 int (System.Math.Floor(float (80. / 100. * (float amoebaResults.Length + 1.))))

//             let ranked =
//                 amoebaResults
//                 |> Array.map List.head
//                 |> Array.sortBy fst
//                 |> Array.take (amoebaResults.Length - percentile80thRank)

//             let dims = Array.length paramBounds
//             let boundsList = ranked |> Array.map snd

//             let getBounds (dim: int) (points: Point<'a> array) =
//                 let max = points |> Array.maxBy (fun p -> p.[dim])
//                 let min = points |> Array.minBy (fun p -> p.[dim])
//                 logger <| GeneralEvent(sprintf "Min %A Max %A" min.[dim] max.[dim])
//                 (min.[dim], max.[dim], Bristlecone.Parameter.Constraint.Unconstrained)

//             let bounds =
//                 [| 0 .. dims - 1 |] |> Array.map (fun dim -> (boundsList |> getBounds dim))

//             let boundWidth = bounds |> Array.sumBy (fun (l, h, _) -> h - l)
//             logger <| GeneralEvent(sprintf "Bound width: %f" boundWidth)

//             if (numberOfLevels > 1 && boundWidth > 0.01) then
//                 swarm settings rng logger (numberOfLevels - 1) iterationsPerLevel numberOfAmoeba bounds startPoint f
//             else
//                 mostLikely


//     /// Optimise an objective function using a single downhill Nelder Mead simplex.
//     let single settings : Optimiser =
//         InTransformedSpace <| Solver.solve settings

//     /// Optimisation heuristic that creates a swarm of amoeba (Nelder-Mead) solvers.
//     /// The swarm proceeds for `numberOfLevels` levels, constraining the starting bounds
//     /// at each level to the 80th percentile of the current set of best likelihoods.
//     let swarm levels amoebaAtLevel settings : Optimiser =
//         InTransformedSpace
//         <| fun rng logger endAt domain startPoint f ->
//             [ Solver.swarm settings rng logger levels endAt amoebaAtLevel domain startPoint f ]
