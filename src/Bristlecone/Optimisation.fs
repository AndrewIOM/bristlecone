namespace Bristlecone.Optimisation

open Bristlecone.Logging

/// Point is generic to allow choice of number precision
type Point<'a> = 'a []
type Solution<'a> = float * Point<'a>
type Objective<'a> = Point<'a> -> float
type EndCondition<'a> = Solution<'a> list -> bool
type Domain = (float*float*Bristlecone.Parameter.Constraint) []

module EndConditions =

    open Bristlecone.Statistics

    /// End the optimisation procedure when a minimum number of iterations is exceeded.
    let afterIteration iteration : EndCondition<float> =
        fun results -> 
            results |> Seq.length >= iteration

    /// An `EndCondition` that calculates that segregates the most recent n results into
    /// five bins, and runs a regression to detect a temporal variation in the mean
    /// squared jumping distance (MSJD). The significance of the slope coefficient of a linear
    /// regression is assessed to determine if the MSJD is increasing through time for every
    /// parameter sequentially: if all p-values are >0.1, then the `EndCondition` is true.
    let stationarySquaredJumpDistance' fixedBin pointsRequired : EndCondition<float> =
        fun results ->
            if (results |> Seq.length) % (fixedBin * pointsRequired) = 0
            then
                let trendSignificance =
                    results
                    |> List.take (fixedBin * pointsRequired)
                    |> List.chunkBySize fixedBin
                    |> List.map(fun bin ->
                        bin
                        |> List.map (snd >> Array.toList)
                        |> Bristlecone.List.flip
                        |> List.map(fun p -> 
                            p 
                            |> List.pairwise
                            |> List.averageBy(fun (a,b) -> (b - a) ** 2.)))
                    |> Bristlecone.List.flip
                    |> List.map(fun msjds -> Regression.pValueForLinearSlopeCoefficient [|1. .. msjds |> Seq.length |> float |] (msjds |> List.toArray))
                not (trendSignificance |> List.exists(fun p -> p <= 0.1) || trendSignificance.Length = 0)
            else false

    /// True if there is no significant slope in mean squared jumping distances (MSJD),
    /// binned per 200 iterations and a regression of five bins.
    let stationarySquaredJumpDistance : EndCondition<float> =
        stationarySquaredJumpDistance' 200 5

    /// Convergence of results using the Gelman-Rubin Rhat statistic.
    /// * `thin` - Only test for convergence at multiples of the following intervals (when all chains are ready).
    /// * `chainCount` - The number of chains to test for convergence. This makes the agent wait until results for all chains are in.
    let convergence thin chainCount : EndCondition<float> =

        let assess (chains:Map<int,Solution<float> list>) = 
            printfn "Assessing convergence..."
            if chains.Count < chainCount 
            then None
            else 
                let shortestChainLength = chains |> Seq.map (fun x -> x.Value.Length) |> Seq.min
                let chainsEqualLength = chains |> Seq.map (fun x -> x.Value |> Seq.skip (x.Value.Length - shortestChainLength))
                let parameterValueByChain =
                    let thetaLength = (chains |> Seq.head).Value |> Seq.head |> snd |> Seq.length
                    [ 1 .. thetaLength ]
                    |> Seq.map(fun p ->
                        chainsEqualLength
                        |> Seq.map(fun chain ->
                            chain |> Seq.map(fun v -> v |> snd |> Array.item (p - 1))))
                printfn "Param values by chain: %A" parameterValueByChain
                parameterValueByChain
                |> Seq.map Bristlecone.Statistics.Convergence.GelmanRubin.rHat
                |> Seq.toList
                |> Some

        let convergenceAgent = MailboxProcessor.Start(fun inbox -> 
            let rec messageLoop chainResults = async {
                let! (chainId,results,repl:AsyncReplyChannel<bool>) = inbox.Receive()
                let freshResults = chainResults |> Map.add chainId results
                let rHats = assess freshResults
                match rHats with
                | Some r ->
                    printfn "RHats are %A" r
                    let converged = (r |> List.where(fun i -> i < 1.1) |> List.length) = (r |> List.length)
                    printfn "Converged on %i / %i parameters" (r |> List.where(fun i -> i < 1.1) |> List.length) (r |> List.length)
                    repl.Reply(converged)
                | None -> repl.Reply(false)
                return! messageLoop freshResults
                }
            messageLoop Map.empty )

        convergenceAgent.Error.Add(fun e -> printfn "Convergence agent reported an error = %s" e.Message)

        fun results -> 
            if results.Length % thin = 0 && results.Length >= thin
            then 
                printfn "Sending message to convergence agent at %i" results.Length
                let threadId = System.Threading.Thread.CurrentThread.ManagedThreadId
                convergenceAgent.PostAndReply (fun reply -> (threadId, results, reply))
            else false


/// A module containing Monte Carlo Markov Chain (MCMC) methods for optimisation.
/// An introduction to MCMC approaches is provided by 
/// [Reali, Priami, and Marchetti (2017)](https://doi.org/10.3389/fams.2017.00006)
module MonteCarlo =

    open Bristlecone.Statistics.Distributions
    open MathNet.Numerics.LinearAlgebra

    /// Generate a random point from bounds specified as a `Domain`.  
    /// A value for each dimension is drawn from a univariate normal distribution, assuming that
    /// the bounds represent the 99th percentiles of the distribution.
    let initialise (d:Domain) (rng:System.Random) =
        [| for (min,max,_) in d -> (Normal.draw rng (min + (max-min) / 2.) ((max - min) / 6.))() |]

    let constrainJump a b (scaleFactor:float) c =
        match c with
        | Bristlecone.Parameter.Constraint.Unconstrained -> a + (b * scaleFactor)
        | Bristlecone.Parameter.Constraint.PositiveOnly ->
            if (a + (b * scaleFactor)) < 0.
                then (a - (b * scaleFactor)) 
                else (a + (b * scaleFactor))


    /// A recursive metropolis hastings algirhtm, when ends when `endCondition` returns true.
    /// 
    /// **Parameters**
    ///   * `random` - `System.Random` to be used for drawing from a uniform distribution.
    ///   * `writeOut` - side-effect function for handling `LogEvent` items.
    ///   * `endCondition` - `EndCondition` that dictates when the MH algorithm ends.
    ///   * `propose` - proposal `'scale -> 'theta -> 'theta` that generates a jump based on the scale value.
    ///   * `tune` - parameter of type `int -> (float * 'a) list -> 'b -> 'b`, where `int` is current iteration, 
    ///   * `f` - an objective function, `'a -> float`, to optimise.
    ///   * `theta1` - initial position in parameter space of type `'a`.
    ///   * `l1` - initial value of -log likelihood at theta1 in parameter space
    ///   * `d` - history of the chain, of type `(float * 'a) list`. Passing a list here allows continuation of a previous analysis.
    ///   * `scale` - a scale of type `'b`, which is compatible with the scale tuning function `tune`
    ///
    /// **Output Type**
    ///   * `(float * 'a) list * 'b` - A tuple containing a list of results, and the final scale used in
    ///   the analysis. The `(float * 'a) list` represents a list of paired -log likelihood values with
    ///   the proposed theta. 
    /// 
    let rec metropolisHastings' random writeOut endCondition propose tune f theta1 l1 d scale =
        let iteration = d |> Seq.length
        let sc = scale |> tune iteration d
        let theta2 = theta1 |> propose sc
        let thetaAccepted, lAccepted = 
            let l2 = f theta2
            if l2 < l1
            then (theta2, l2)
            else
                let rand = ContinuousUniform.draw random 0. 1. ()
                let ratio = - (l2 - l1) |> exp
                if rand < ratio && l2 <> infinity && l2 <> -infinity && l2 <> nan
                    then (theta2, l2)
                    else (theta1, l1)
        if endCondition d
        then (d, sc)
        else
            writeOut <| OptimisationEvent { Iteration = iteration; Likelihood = lAccepted; Theta = thetaAccepted }
            metropolisHastings' random writeOut endCondition propose tune f thetaAccepted lAccepted ((lAccepted,thetaAccepted)::d) sc


    module TuningMode =

        /// Find an appropriate scale factor for a standard deviation and its acceptance rate.
        let tuneScale scale accRate =
            match accRate with
            | a when a > 0.95   -> scale * 10.0
            | a when a > 0.75   -> scale * 2.0
            | a when a > 0.50   -> scale * 1.1
            | a when a < 0.001  -> scale * 0.1
            | a when a < 0.05   -> scale * 0.5
            | a when a < 0.20   -> scale * 0.9
            | _ -> scale

        /// Modifies a scale factor depending on 
        let scaleFactor tuneInterval remainingIterations history scale =
            if remainingIterations % tuneInterval = 0 then
                if history |> Seq.length >= tuneInterval then
                    let acceptance = float ((history |> Seq.take tuneInterval |> Seq.pairwise |> Seq.where(fun (x,y) -> x <> y) |> Seq.length)) / (float tuneInterval)
                    tuneScale scale acceptance
                else scale
            else scale

        /// Tunes a covariance matrix based on recent samples.
        let tuneCovariance weight (recentCovariance:Matrix<float>) (oldCovariance:Matrix<float>) =
            weight * recentCovariance + (1. - weight) * oldCovariance

        /// Generate a covariance matrix
        let covarianceMatrix parameterCount (scale:float) =
            let m4 = MathNet.Numerics.LinearAlgebra.DiagonalMatrix.identity<float> parameterCount
            (m4 * scale) / sqrt (float parameterCount)

        /// Calculates the covariance of a given matrix
        let computeCovariance (matrix:Matrix<float>) =
            let columnAverages = matrix.ColumnSums() / (float matrix.RowCount)
            let centredColumns = matrix.EnumerateColumns() |> Seq.zip columnAverages |> Seq.map (fun (col,avg) -> col - avg) |> Seq.toList
            let centred = MathNet.Numerics.LinearAlgebra.DenseMatrix.ofColumns centredColumns
            let normalisationFactor = float <| if matrix.RowCount = 1 then 1 else matrix.RowCount - 1
            centred.TransposeThisAndMultiply(centred) / normalisationFactor

        /// Parameters to matrix
        let samplesToMatrix (samples:float[][]) =
            samples |> matrix

        /// The starting covariance matrix for parameters in a multivariate distribution
        let defaultCovariance n = covarianceMatrix n 2.38

        let covarianceFromBounds n (domain:Domain) random =
            [| 1 .. n |]
            |> Array.map(fun _ ->
                domain
                |> Array.map (fun (low,high,_) -> Normal.draw random 0. ((high - low) / 4.) () ) )
                |> samplesToMatrix
                |> computeCovariance

        let covarianceFromSigmas n random sigmas =
            [| 1 .. n |]
            |> Array.map(fun _ ->
                sigmas
                |> Array.map (fun s -> Normal.draw random 0. (s**2.) () ) )
                |> samplesToMatrix
                |> computeCovariance

        /// Tune previously observed covariance based on most recent period
        let covariance tuneInterval weighting remaining (history:(float*float[]) seq) scale =
            if remaining % tuneInterval = 0 then
                if history |> Seq.length >= tuneInterval then
                    let sc =
                        history
                        |> Seq.map snd
                        |> Seq.take tuneInterval
                        |> matrix
                        |> computeCovariance
                        |> fun c -> tuneCovariance weighting scale c
                    try sc.Cholesky() |> ignore; sc // Ensure the matrix is positive definite
                    with | _ -> scale
                else scale
            else scale

        let dual tuneInterval weighting remaining history tuningFactors =
            let cov,sc = tuningFactors
            let tunedSc = scaleFactor tuneInterval remaining history sc
            let tunedCov = covariance tuneInterval weighting remaining history cov
            (tunedCov, tunedSc)

        let scaleOnly tuneInterval remaining history tuningFactors =
            let cov,sc = tuningFactors
            let tunedSc = scaleFactor tuneInterval remaining history sc
            (cov, tunedSc)

        let covarianceOnly tuneInterval weighting remaining history tuningFactors =
            let cov,sc = tuningFactors
            let tunedCov = covariance tuneInterval weighting remaining history cov
            (tunedCov, sc)

        /// Tune previously observed covariance based on all time
        let covarianceAllTime weighting (history:(float*float[]) seq) scale =
            try 
                let sc =
                    history
                    |> Seq.map snd
                    |> matrix
                    |> computeCovariance
                    |> fun c -> tuneCovariance weighting scale c
                sc.Cholesky() |> ignore // Ensure the matrix is positive definite
                sc
            with | _ -> scale

        let dualTotalHistory tuneInterval weighting remaining history tuningFactors =
            let cov,sc = tuningFactors
            let tunedSc = scaleFactor tuneInterval remaining history sc
            let tunedCov = covarianceAllTime weighting history cov
            (tunedCov, tunedSc)

        let none _ _ factors = factors

    type TuneMethod =
        | Covariance of float
        | Scale
        | CovarianceWithScale of float
        | CovarianceWithScaleTotalHistory of float

    let toFn method interval =
        match method with
        | Covariance w -> TuningMode.covarianceOnly interval w
        | Scale -> TuningMode.scaleOnly interval
        | CovarianceWithScale w -> TuningMode.dual interval w
        | CovarianceWithScaleTotalHistory w -> TuningMode.dualTotalHistory interval w

    type Frequency = int
    type TuneStep<'a> = TuneMethod * Frequency * EndCondition<'a>

    let randomWalk' initialCovariance initialScale theta (tuningSteps:seq<TuneStep<float>>) random writeOut (endCondition:EndCondition<float>) (domain:Domain) (f:Objective<float>) =
        writeOut <| GeneralEvent (sprintf "[Optimisation] Starting MCMC Random Walk")
        let sample cov = MutlivariateNormal.sample cov random
        let proposeJump (cov:Matrix<float>,scale) (theta:float[]) =
            sample ((2.38 ** 2.) * cov / (float theta.Length)) <| ()
            |> Vector.toArray
            |> Array.map (fun i -> i * scale)
            |> Array.mapi (fun i x -> (x, domain.[i]) )
            |> Array.zip theta
            |> Array.map (fun (thetai,(zi,(_,_,con))) -> constrainJump thetai zi scale con)

        tuningSteps
        |> Seq.fold(fun (s,sc) (method,frequency,endCon) -> 
            let l,t = s |> Seq.head
            metropolisHastings' random writeOut endCon proposeJump (toFn method frequency) f t l s sc ) ([f theta, theta], (initialCovariance,initialScale))
        ||> fun r s -> 
            let l,t = r |> Seq.head
            metropolisHastings' random writeOut endCondition proposeJump (TuningMode.none) f t l r s

    let randomWalk (tuningSteps:seq<TuneStep<float>>) writeOut n domain (f:Objective<float>) : (float * float[]) list =
        let random = MathNet.Numerics.Random.MersenneTwister(true)
        let initialCovariance = TuningMode.covarianceFromBounds 10000 domain random
        let theta = initialise domain random
        writeOut <| GeneralEvent (sprintf "[Optimisation] Initial theta is %A" theta)
        randomWalk' initialCovariance 1. theta tuningSteps random writeOut n domain f |> fst

    let adaptiveMetropolis weighting period writeOut n domain (f:Objective<float>) : (float * float[]) list =
        randomWalk [ (TuneMethod.CovarianceWithScale weighting, period, n) ] writeOut (EndConditions.afterIteration 0) domain f


    module MetropolisWithinGibbs =

        open Bristlecone.Statistics

        /// Propose a jump, while leaving all but one parameter value fixed
        let propose (theta:float[]) j lsj domain random =
            theta
            |> Array.mapi(fun i e ->
                if i = j
                then (e, MathNet.Numerics.Distributions.Normal(0.,(exp lsj)**2.,random).Sample())
                else (e, 0.) )
            |> Array.zip domain
            |> Array.map (fun ((_,_,con), (e,jump)) -> constrainJump e jump 1. con)

        /// Tune variance of a parameter based on its acceptance rate.
        /// The magnitude of tuning reduces as more batches have been run.
        let tune sigma acceptanceRate batchNumber =
            if acceptanceRate < 0.44
            then sigma - (1./(float batchNumber)) // Lower variance
            else sigma + (1./(float batchNumber)) // Increase variance

        /// Adaptive-metropolis-within-Gibbs algorithm, which can work in both adaptive and fixed modes
        let rec core isAdaptive writeOut random domain f results batchLength batchNumber (theta:float[]) sigmas =

            let ds =
                theta
                |> Array.zip sigmas
                |> Array.scan (fun (j,results) (lsj,_) -> 
                    let l,theta = results |> Seq.head
                    let proposeJump = fun _ t -> propose t j lsj domain random
                    let results = metropolisHastings' random ignore (EndConditions.afterIteration batchLength) proposeJump TuningMode.none f theta l [] () |> fst
                    (j+1, results)) (0, [ (f theta, theta) ])
                |> Array.tail       // Skip initial state
                |> Array.map snd    // Discard parameter number

            let d =
                ds
                |> Array.rev        // MH results are ordered with most recent at head
                |> List.concat      // Results sorted last to first

            let fullResults = List.concat [d; results]

            writeOut <| OptimisationEvent { Iteration = batchNumber; Likelihood = d |> Seq.head |> fst; Theta = d |> Seq.head |> snd }

            if isAdaptive then
                // Tune variance according to the per-parameter acceptance rate.
                // Only end when all acceptance rates are in acceptable range.
                let acceptanceRates =
                    ds |> Array.map(fun results ->
                        let accepted = 
                            results
                            |> List.pairwise
                            |> List.where(fun (x,y) -> x <> y)
                            |> List.length
                        (float accepted) / (float batchLength) )
                let tunedSigmas =
                    acceptanceRates
                    |> Array.zip sigmas
                    |> Array.map(fun (ls,ar) -> tune ls ar batchNumber)
                writeOut <| GeneralEvent (sprintf "[Tuning] Sigmas: %A | Acceptance rates: %A" tunedSigmas acceptanceRates)
                if acceptanceRates |> Array.exists(fun s -> s < 0.28 || s > 0.60)
                then core isAdaptive writeOut random domain f fullResults batchLength (batchNumber+1) (fullResults |> Seq.head |> snd) tunedSigmas
                else (batchNumber, fullResults, tunedSigmas)

            else 
                // Stop only when there is no linear trend
                let linearTrendPValues = 
                    fullResults |> List.chunkBySize batchLength  // So we now have each individual 'fixed' run listed out
                    |> List.mapi(fun i batch ->      // Get unfixed values and their parameter index number
                        let paramNumber = i % theta.Length
                        let paramValues = batch |> List.map snd |> List.averageBy(fun x -> x.[paramNumber])
                        (paramNumber, paramValues) )
                    |> List.groupBy fst
                    |> List.where(fun (_,g) -> g.Length >= 5)   // Only calculate regression when 5 or more points
                    |> List.map (fun (i,p) -> 
                        let x,y = 
                            p
                            |> List.take 5                      // Use only 5 most recent batches
                            |> List.map snd
                            |> List.mapi (fun i v -> (float i, v) )
                            |> List.toArray
                            |> Array.unzip
                        let pValue = Regression.pValueForLinearSlopeCoefficient x y
                        pValue )

                writeOut <| GeneralEvent (sprintf "Linear trend p-values: %A" linearTrendPValues)

                if linearTrendPValues |> List.exists(fun p -> p <= 0.1) || linearTrendPValues.Length = 0
                then core isAdaptive writeOut random domain f fullResults batchLength (batchNumber+1) (fullResults |> Seq.head |> snd) sigmas
                else (batchNumber, fullResults, sigmas)


    let ``Adaptive-Metropolis-within Gibbs`` writeOut endCon domain (f:Objective<float>) : (float * float[]) list =
        let random = MathNet.Numerics.Random.MersenneTwister(true)
        let theta = initialise domain random
        let sigmas = theta |> Array.map(fun _ -> 0.)
        let _,result,_ = MetropolisWithinGibbs.core true writeOut random domain f [] 100 1 (theta:float[]) sigmas
        result

    let ``Metropolis-within Gibbs`` writeOut endCon domain (f:Objective<float>) : (float * float[]) list =
        let random = MathNet.Numerics.Random.MersenneTwister(true)
        let theta = initialise domain random
        let sigmas = theta |> Array.map(fun _ -> 0.)
        let _,result,_ = MetropolisWithinGibbs.core false writeOut random domain f [] 100 1 (theta:float[]) sigmas
        result

    /// Implementation similar to that proposed by Yang and Rosenthal: "Automatically Tuned General-Purpose MCMC via New Adaptive Diagnostics"
    /// Reference: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.70.7198&rep=rep1&type=pdf
    let ``Automatic MCMC (Adaptive Diagnostics)`` writeOut n domain (f:Objective<float>) : Solution<float> list =

        // Starting condition
        let random = MathNet.Numerics.Random.MersenneTwister(true)
        let initialTheta = initialise domain random
        let initialSigma = initialTheta |> Array.map(fun _ -> 0.)

        let mwg adapt batchSize currentBatch theta sigmas =
                MetropolisWithinGibbs.core adapt writeOut random domain f [] batchSize currentBatch theta sigmas

        // 1st Adaptive Phase
        writeOut <| GeneralEvent "Generalised MCMC: Starting 1st Adaptive Phase"
        let batches,results,tunedSigmas = 
            initialSigma
            |> mwg true 100 1 initialTheta
            |> fun (b,r,s) -> 
                let _,theta = r |> Seq.head
                mwg true 200 b theta s
            |> fun (b,r,s) -> 
                let _,theta = r |> Seq.head
                mwg true 400 b theta s
            // Transient phase
            |> fun (b,r,s) -> 
                writeOut <| GeneralEvent "Generalised MCMC: Starting Transient Phase"
                let _,theta = r |> Seq.head
                mwg false 200 b theta s

        // 2nd Adaptive Phase
        // a) Compute a covariance matrix from the previous sigma values
        let covariance = 
            tunedSigmas
            |> Array.map exp
            |> TuningMode.covarianceFromSigmas 10000 random

        // b) Continue the chain using the covariance matrix and last theta
        writeOut <| GeneralEvent "Generalised MCMC: Starting 2nd Adaptive Phase"
        let secondAdaptation, finalScale = 
            randomWalk' covariance 1. (results |> Seq.head |> snd) [ (TuneMethod.CovarianceWithScaleTotalHistory 0.750, 200, EndConditions.stationarySquaredJumpDistance) ] random writeOut (EndConditions.afterIteration 0) domain f

        // 3. Burn-in and sampling
        writeOut <| GeneralEvent "Generalised MCMC: Starting Sampling Phase (random walk MCMC, with burnin and clean trace)"
        randomWalk' (finalScale |> fst) (finalScale |> snd) (secondAdaptation |> Seq.head |> snd) [] random writeOut n domain f |> fst


    /// A meta-heuristic that approximates a global optimium by
    /// simulating slow cooling as a slow decrease in the probability
    /// of temporarily accepting worse solutions.
    module SimulatedAnnealing =

        /// Cooling schemes dictate the conditions under which the temperature is cooled
        /// during simulated annealing.
        module CoolingSchemes =

            /// Commonly set alpha to 0.95.
            /// Tk is the temperature after k cooling iterations.
            let exponential alpha t0 t k : float =
                alpha * t

            let fastCauchyCoolingSchedule (t0:float) t k =
                t0 / (float k)


        module Machines =

            /// e = new minus old energy (or -logL)
            let boltzmann t e =
                -e / t |> exp


        module EndConditions =

            let defaultTolerance = 1e-06

            /// Given a list of solutions, which are ordered most recent first,
            /// returns `true` if there are at least `chains` recent results, and
            /// the change within the recent results is no more than `tolerance`.
            let stoppedImproving chains (minimums:Solution<float> list) =
                if minimums |> List.length < chains
                then false
                else
                    (minimums
                    |> List.map fst
                    |> List.take chains
                    |> List.pairwise
                    |> List.sumBy(fun (n,old) -> max 0. (old - n)) // Captures backwards jumps
                    |> (fun x -> printfn "Better by %f" x; x)) <= defaultTolerance

            let noImprovement (chain:Solution<float> list) =
                (chain
                |> List.map fst
                |> List.pairwise
                |> List.averageBy(fun (n,old) -> if n > old then n - old else old - n ) // Mean change in objective value
                |> (fun x -> printfn "Average jump is %f" x; x)) <= defaultTolerance

            let improvementCount count interval : EndCondition<float> =
                fun results ->
                    if (results |> List.length) % interval = 0 
                    then (results |> List.pairwise |> List.where(fun (x,y) -> x < y) |> List.length) >= count
                    else false

        /// Represents configurable settings of an annealing procedure
        /// that supports (a) heating, followed by (b) annealing.
        type AnnealSettings<'a> = 
            { HeatStepLength: EndCondition<'a>
              HeatRamp: float -> float
              TemperatureCeiling: float option
              BoilingAcceptanceRate: float
              InitialTemperature: float
              TuneLength: int
              AnnealStepLength: EndCondition<'a> }
            
            static member Default = {
                HeatStepLength = EndConditions.afterIteration 250
                HeatRamp = fun t -> t * 1.10
                BoilingAcceptanceRate = 0.85
                TemperatureCeiling = Some 200.
                InitialTemperature = 1.00
                TuneLength = 100000
                AnnealStepLength = EndConditions.improvementCount 250 250
            }

        /// Jump based on a proposal function and probability function
        let tryMove propose probability random f (l1,theta1) : Solution<float> =
            let rec catchNan () =
                let theta2 = theta1 |> propose
                let l2 = f theta2
                if System.Double.IsNaN l2 then catchNan () else (theta2, l2)
            let theta2,l2 = catchNan()
            if l2 < l1
            then (l2, theta2)
            else
                let rand = ContinuousUniform.draw random 0. 1. ()
                let ratio = probability (l2 - l1)
                //printfn "Random = %f Ratio = %f (L1=%f L2=%f)" rand ratio l1 l2
                if rand < ratio
                    then (l2, theta2)
                    else (l1, theta1)

        /// Run a homogenous Markov chain recursively until an end condition - `atEnd` - is met.
        let rec markovChain writeOut atEnd propose probability random f temperature initialPoint =
            let propose' = tryMove propose (temperature |> probability) random f
            let rec run point d =
                let newPoint = point |> propose'
                if newPoint |> fst = nan
                then run point d
                else
                    let state = newPoint::d
                    if state |> atEnd 
                    then state
                    else 
                        writeOut <| OptimisationEvent { Iteration = d |> List.length; Likelihood = newPoint |> fst; Theta = newPoint |> snd }
                        run (state |> Seq.head) state
            run initialPoint [ initialPoint ]

        /// Cool between homoegenous markov chains according to `cool` schedule.
        /// Each anneal recursion begins from the end of the previous markov chain.
        let rec anneal writeOut chainEnd annealEnd cool markov temperature point previousBests =
            let results = point |> markov chainEnd temperature
            let bestAtTemperature = results |> List.minBy fst
            let history = bestAtTemperature::previousBests
            if (* annealEnd history *)temperature < 0.75 then history
            else 
                writeOut <| GeneralEvent (sprintf "[Annealing] Best point is %f at temperature %f" (bestAtTemperature |> fst) temperature)
                anneal writeOut chainEnd annealEnd cool markov (cool temperature (history |> List.length)) bestAtTemperature (*(results |> List.head)*) history

        /// Heat up temperature intil acceptance rate of bad moves is above the threshold `endAcceptanceRate`.
        let rec heat write endCondition ceiling endAcceptanceRate heatingSchedule markov bestTheta temperature =
            let chain = markov endCondition temperature bestTheta
            let min = chain |> List.minBy fst
            let ar =
                let a,r =
                    chain
                    |> List.map fst
                    |> List.pairwise
                    |> List.map(fun (x,y) -> ((x > y), (x = y))) // Jumped back, bad jump rejected
                    |> List.unzip
                let badAccepted = a |> List.where id |> List.length |> float
                let badRejected = r |> List.where id |> List.length |> float
                badAccepted / (badAccepted + badRejected)
            write <| GeneralEvent (sprintf "Heating - Jump average is %f" (chain |> List.map fst |> List.pairwise |> List.averageBy (fun (a,b) -> b - a)))
            write <| GeneralEvent (sprintf "Heating (T=%f) - AR of bad moves is %f" temperature ar)
            let aboveCeiling = if Option.isSome ceiling then (temperature >= ceiling.Value) else false
            if ar < endAcceptanceRate && not aboveCeiling
            then heat write endCondition ceiling endAcceptanceRate heatingSchedule markov min (temperature |> heatingSchedule)
            else (temperature, min)

        // Given a candidate distribution + machine, run base SA algorithm
        let simulatedAnnealing scale settings annealEnd machine (jump:System.Random->float->float->unit->float) cool writeOut domain f =

            // 1. Initial conditions
            let random = MathNet.Numerics.Random.MersenneTwister(true)
            let draw' = jump random
            let theta1 = initialise domain random
            let l1 = f theta1

            // 2. Chain generator
            let homogenousChain scales e temperature = 
                let propose (scale:Point<float>) (theta:Point<float>) =
                    Array.zip3 theta scale domain
                    |> Array.map(fun (x,sc,(_,_,con)) -> constrainJump x (draw' sc temperature ()) 1. con )
                markovChain writeOut e (propose scales) machine random f temperature 

            // 3. Tune individual step size based on acceptance rate
            let initialScale = 
                [| 1 .. theta1.Length |] |> Array.map (fun _ -> scale)

            let kMax = settings.TuneLength
            let rec tune (p:(float*float[])[]) k (l1, theta1) =
                let chance = (float k) / (float kMax)
                let parameterToChange = random.Next(0, (p |> Array.length) - 1)
                let scalesToChange = p |> Array.mapi (fun i x -> (x, random.NextDouble() < chance || i = parameterToChange))
                let propose theta =
                    Array.zip3 theta scalesToChange domain
                    |> Array.map(fun (x,((ti,n),shouldChange),(_,_,con)) -> 
                        if shouldChange
                        then constrainJump x (draw' ti 1. ()) 1. con
                        else x )
                let result = tryMove propose (machine 1.) random f (l1, theta1)
                let tuneN = 50
                let newScaleInfo = 
                    scalesToChange 
                    |> Array.zip (result |> snd)
                    |> Array.map(fun (v, ((ti,previous),changed)) ->
                        if changed then (ti, (previous |> Array.append [|v|]))  // Append new parameter values to previous ones
                        else (ti, previous) )
                    |> Array.map(fun (ti,previous) ->
                        if previous |> Array.length = tuneN
                        then
                            let changes = previous |> Array.pairwise |> Array.where(fun (a,b) -> a <> b) |> Array.length
                            match (float changes) / (float tuneN) with
                            | ar when ar < 0.35 -> (ti * 0.80, Array.empty)
                            | ar when ar > 0.50 -> (ti * 1.20, Array.empty)
                            | _ -> (ti, Array.empty)
                        else (ti, previous) )

                if k % 1000 = 0 then
                    writeOut <| GeneralEvent (sprintf "Tuning is at %A (k=%i/%i) [-logL %f]" (newScaleInfo |> Array.map fst) k kMax (result |> fst))

                if k < kMax
                then tune newScaleInfo (k + 1) result
                else (newScaleInfo |> Array.map fst, l1, theta1)

            let tunedScale,l2,theta2 = 
                homogenousChain initialScale (EndConditions.afterIteration 5000) 1. (l1, theta1)
                |> List.head
                |> tune (initialScale |> Array.map(fun t -> (t, Array.empty))) 1
            writeOut <| GeneralEvent (sprintf "Tuned = %A" tunedScale)

            // 4. Heat up
            let boilingPoint,min = heat writeOut settings.HeatStepLength settings.TemperatureCeiling settings.BoilingAcceptanceRate settings.HeatRamp (homogenousChain tunedScale) (l2, theta2) settings.InitialTemperature

            // 5. Gradually cool down (from best point during heat-up)
            anneal writeOut settings.AnnealStepLength annealEnd (cool boilingPoint) (homogenousChain tunedScale) boilingPoint min []

        /// Candidate distribution: Gaussian univariate []
        /// Probability: Boltzmann Machine
        let classicalSimulatedAnnealing scale tDependentProposal settings writeOut n domain (f:Objective<float>) : Solution<float> list =
            let gaussian rnd scale t = 
                let s = if tDependentProposal then scale * (sqrt t) else scale
                Bristlecone.Statistics.Distributions.Normal.draw rnd 0. s
            simulatedAnnealing scale settings n Machines.boltzmann gaussian (CoolingSchemes.exponential 0.05) writeOut domain f

        /// Candidate distribution: Cauchy univariate []
        /// Probability: Bottzmann Machine
        let fastSimulatedAnnealing scale tDependentProposal settings writeOut n domain (f:Objective<float>) : (float * float[]) list =
            let cauchy rnd scale t = 
                let s = if tDependentProposal then scale * (sqrt t) else scale
                let c = MathNet.Numerics.Distributions.Cauchy(0., s, rnd)
                fun () -> c.Sample()
            simulatedAnnealing scale settings n Machines.boltzmann cauchy (CoolingSchemes.fastCauchyCoolingSchedule) writeOut domain f


    /// An adaptation of the Filzbach method (originally by Drew Purves)
    module Filzbach =
    
        // Implementation details:
        // - Sample from one, three, or many parameters at once
        // - During burnin, tune these:
        //      - AFTER 20 changes (for a single parameter)
        //      - IF ACCEPTANCE < 5
        //      - THEN Delta (Temperature) *= 0.80
        //      - ELSE Delta (Temperature) *= 1.20
        // - Cap deltas at 0.010 and 10.00
        
        // In summary:
        // Parameters jumped is random
        // Parameters jumped is recorded
        // Tuning happens given the jumping information            


        let filzbach' initialScale (theta:float[]) (burnEnd:EndCondition<float>) random writeOut (sampleEnd:EndCondition<float>) (domain:Domain) (f:Objective<float>) =
            writeOut <| GeneralEvent (sprintf "[Optimisation] Starting Filzbach-style MCMC optimisation")
            let sample sd = Normal.draw random 0. sd
            let scaleRnd = Bristlecone.Statistics.Distributions.ContinuousUniform.draw random 0. 1.
            let paramRnd = MathNet.Numerics.Distributions.DiscreteUniform(0, theta.Length - 1, random)

            let rec step burning (p:(float*float[])[]) endWhen (l1, theta1) d =
                // Change one to many parameter at once (Filzbach-style)
                let scalesToChange = 
                    if scaleRnd() < 0.670
                    then
                        // Choose one parameter to change
                        let rnd = paramRnd.Sample()
                        // Change also nearby parameters with probability 1/2:
                        p |> Array.mapi(fun i x -> 
                            (x, if i = rnd then true
                                else if i = (rnd - 1) then scaleRnd() < 0.5
                                else if i = (rnd + 1) then scaleRnd() < 0.5
                                else false))
                    else
                        // Probability of change:
                        let pChange = exp(4.0 * (scaleRnd() - 0.50))
                        // Try and allocate random changes to array
                        let rec changeRandom p =
                            let r = p |> Array.mapi(fun i x -> (x, scaleRnd() < pChange)) 
                            if r |> Array.where(fun (_,b) -> b) |> Array.isEmpty
                            then changeRandom p
                            else r
                        changeRandom p
                
                let propose theta =
                    Array.zip3 theta scalesToChange domain
                    |> Array.map(fun (x,((ti,n),shouldChange),(_,_,con)) -> 
                        if shouldChange
                        then constrainJump x (sample ti ()) 1. con
                        else x )

                // Metropolis step here
                let result = SimulatedAnnealing.tryMove propose (SimulatedAnnealing.Machines.boltzmann 1.) random f (l1, theta1)
                // End metropolis step
                
                // Tune Scales (burnin only)
                let tuneN = 20
                let newScaleInfo = 
                    if not burning then p
                    else 
                        scalesToChange 
                        |> Array.zip (result |> snd)
                        |> Array.map(fun (v, ((ti,previous),changed)) ->
                            if changed then (ti, (previous |> Array.append [|v|]))  // Append new parameter values to previous ones
                            else (ti, previous) )
                        |> Array.map(fun (ti,previous) ->
                            if previous |> Array.length = tuneN
                            then
                                let changes = previous |> Array.pairwise |> Array.where(fun (a,b) -> a <> b) |> Array.length
                                match (float changes) / (float tuneN) with
                                | ar when ar < 0.25 -> (ti * 0.80, Array.empty)
                                | ar when ar > 0.25 -> (ti * 1.20, Array.empty)
                                | _ -> (ti, Array.empty)
                            else (ti, previous) )
                // End Tune Scales (burnin only)

                let newResult = result::d
                if endWhen d
                then (newResult, newScaleInfo |> Array.map fst)
                else 
                    writeOut <| OptimisationEvent { Iteration = newResult |> List.length; Likelihood = result |> fst; Theta = result |> snd }
                    step burning newScaleInfo endWhen result newResult

            let l1 = f theta
            
            let burnResults,burnScales = step true (initialScale |> Array.map(fun t -> (t, Array.empty))) burnEnd (l1, theta) []
            let results,_ = step false (burnScales |> Array.map(fun t -> (t, Array.empty))) sampleEnd (burnResults |> Seq.head) burnResults
            results

        let filzbach initialScale burnEnd writeOut n domain (f:Objective<float>) : (float * float[]) list =
            let random = MathNet.Numerics.Random.MersenneTwister(true)
            let theta = initialise domain random
            let scale = [| 1 .. theta.Length |] |> Array.map (fun _ -> initialScale)
            writeOut <| GeneralEvent (sprintf "[Optimisation] Initial theta is %A" theta)
            filzbach' scale theta burnEnd random writeOut n domain f


// Nelder Mead implementation
// Modified from https://github.com/mathias-brandewinder/Amoeba
module Amoeba =

    module Solver = 

        type Amoeba = 
            { Dim:int; Solutions:Solution<float> [] }
            member this.Size = this.Solutions.Length

            member this.Best = this.Solutions.[0]

            member this.Worst = this.Solutions.[this.Size - 1]

        type Settings = { Alpha:float; Sigma:float; Gamma:float; Rho:float; Size:int }

        let Default = { Alpha=1.0; Sigma=0.5; Gamma=2.0; Rho=(-0.5); Size=3 }

        let print logger a = 
            logger <| GeneralEvent "Amoeba state"
            a.Solutions 
            |> Seq.iter (fun (v,x) -> 
                logger <| GeneralEvent (sprintf "  %.2f, %s" v (x |> Seq.map string |> String.concat ",")))

        let evaluate (f:Objective<'a>) (x:Point<'a>) = (f x, x)
        let valueOf (s:Solution<'a>) = fst s

        let replace (a:Amoeba) (s:Solution<float>) = 
            let last = a.Size - 1
            let a' = Array.copy a.Solutions
            a'.[last] <- s
            { a with Solutions = a' |> Array.sortBy fst }

        let centroid (a:Amoeba) = 
            [| for d in 0 .. (a.Dim - 1) -> 
                (a.Solutions.[0..a.Size - 2] |> Seq.averageBy(fun (_,x) -> x.[d])) |]

        let stretch ((X,Y):Point<float>*Point<float>) (s:float) =
            Array.map2 (fun x y -> x + s * (x - y)) X Y

        let reflected v s = stretch v s.Alpha

        let expanded v s = stretch v s.Gamma

        let contracted v s = stretch v s.Rho

        let shrink (a:Amoeba) (f:Objective<float>) s =
            let best = snd a.Best
            { a with Solutions =         
                        a.Solutions 
                        |> Array.map (fun p -> stretch (best,snd p) -s.Sigma)
                        |> Array.map (evaluate f) } 

        let update (a:Amoeba) (f:Objective<float>) (s:Settings) =
            let cen = centroid a
            let rv,r = reflected (cen, (snd a.Worst)) s |> evaluate f
            if ((valueOf (a.Best) <= rv) && (rv < (valueOf (a.Solutions.[a.Size - 2])))) then
                replace a (rv, r)
            else
                if (rv < valueOf (a.Best)) then
                    let ev,e = expanded (cen, r) s |> evaluate f
                    if (ev < rv) then
                        replace a (ev, e)
                    else
                        replace a (rv, r)
                else
                    let (cv,c) = contracted (cen, (snd a.Worst)) s |> evaluate f
                    if (cv < valueOf (a.Worst)) then
                        replace a (cv, c)
                    else
                        shrink a f s

        let initialize (d:Domain) (rng:System.Random) =
            [| for (min,max,_) in d -> min + (max-min) * rng.NextDouble() |]

        let solve settings logger (endWhen:EndCondition<float>) domain f =
            let dim = Array.length domain
            let rng = System.Random()
            let start =             
                [| for _ in 1 .. settings.Size -> initialize domain rng |]
                |> Array.map (evaluate f)
                |> Array.sortBy fst
            let amoeba = { Dim = dim; Solutions = start }

            let rec search i (a:Amoeba) =
                if i > 0 then search (i-1) (update a f settings) // TODO unify array vs list
                else 
                    logger <| GeneralEvent (sprintf "Solution: -L = %f" (fst a.Solutions.[0]))
                    a.Solutions |> Array.toList

            search 50000 amoeba

    /// Optimisation heuristic that creates a swarm of amoeba (Nelder-Mead) solvers.
    /// The swarm proceeds for `numberOfLevels` levels, constraining the starting bounds
    /// at each level to the 80th percentile of the current set of best likelihoods.
    let rec swarm logger numberOfLevels iterationsPerLevel numberOfAomeba (paramBounds:Domain) (f:Objective<float>) =

        let aomebaResults = 
            [|1 .. numberOfAomeba|]
            |> Array.collect (fun _ -> 
                try [|Solver.solve Solver.Default logger iterationsPerLevel paramBounds f|]
                with | e -> 
                    logger <| GeneralEvent (sprintf "Warning: Could not generate numercal solution for point (with EXN %s): %A" e.Message paramBounds)
                    [||] )

        let mostLikely = aomebaResults |> Array.map List.head |> Array.minBy fst

        // Drop worst 20% of likelihoods
        let percentile80thRank = int (System.Math.Floor (float (80. / 100. * (float aomebaResults.Length + 1.))))
        let percentile80thValue = fst aomebaResults.[0].[percentile80thRank - 1]
        logger <| GeneralEvent (sprintf "80th percentile = %f" percentile80thValue)
        let ranked = aomebaResults |> Array.map List.head |> Array.filter (fun x -> (fst x) <= percentile80thValue)

        let dims = Array.length paramBounds

        let boundsList = ranked |> Array.map snd

        let getBounds (dim:int) (points:Point<'a> array) =
            let max = points |> Array.maxBy (fun p -> p.[dim])
            let min = points |> Array.minBy (fun p -> p.[dim])
            logger <| GeneralEvent (sprintf "Min %A Max %A" min.[dim] max.[dim])
            (min.[dim], max.[dim], Bristlecone.Parameter.Constraint.Unconstrained)

        let bounds =
            [|0 .. dims - 1|]
            |> Array.map (fun dim -> (boundsList |> getBounds dim))

        let boundWidth =
            bounds
            |> Array.sumBy (fun (l,h,_) -> h - l)
        logger <| GeneralEvent (sprintf "Bound width: %f" boundWidth)
        
        if (numberOfLevels > 1 && boundWidth > 0.01) 
            then swarm logger (numberOfLevels-1) iterationsPerLevel numberOfAomeba bounds f
            else mostLikely
