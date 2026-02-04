namespace Bristlecone.Optimisation

open Bristlecone
open Bristlecone.Logging
open Bristlecone.EstimationEngine
open Bristlecone.Statistics

/// <summary>Composable end conditions to specify when optimisation
/// routines should end.</summary>
module EndConditions =

    let internal defaultTolerance = 1e-06<``-logL``>

    let internal onlyOnInterval interval i fn =
        if i % interval = 0<iteration> && i > 1<iteration> then
            fn ()
        else
            Continue

    let internal rollingWindow window results iteration fn =
        if iteration >= window then
            let n = Units.removeUnitFromInt window
            let slice = results |> List.take n
            fn slice
        else Continue

    /// Stop when any condition has a non-continue reason.
    let combineAny (conditions: EndCondition list) : EndCondition =
        fun results iter ->
            conditions
            |> Seq.map (fun cond -> cond results iter)
            |> Seq.tryFind (fun reason -> reason <> Continue)
            |> Option.defaultValue Continue

    /// Stop when all conditions have non-continue reasons.
    let combineAll (conditions: EndCondition list) : EndCondition =
        fun results iter ->
            let evaluations = conditions |> List.map (fun cond -> cond results iter)
            if evaluations |> List.exists ((=) Continue) then Continue
            else evaluations |> List.find (fun r -> r <> Continue)

    /// End on or after a minimum number of iterations.
    let atIteration iteration : EndCondition =
        fun _ currentIteration ->
            if currentIteration >= iteration then
                OptimStopReason.MaxIterations
            else
                OptimStopReason.Continue

    /// Given a list of solutions, which are ordered most recent first,
    /// returns `true` if there are at least `window` recent results, and
    /// the change within the recent results is no more than `tolerance`.
    let whenNoBestValueImprovement window : EndCondition =
        fun results iteration ->
            rollingWindow window results iteration <| fun window ->
                let improvement =
                    window
                    |> List.pairwise
                    |> List.sumBy (fun (n, old) -> max 0.<``-logL``> (fst old - fst n))
                if improvement <= defaultTolerance then NoImprovement else Continue

    /// Ends if the mean change in the objective's value is less than
    /// a tolerance of 1e-6.
    let whenObjectiveFlat interval : EndCondition =
        fun results iteration ->
            onlyOnInterval interval iteration <| fun () ->
                let improvement =
                    results
                    |> List.map fst
                    |> List.take (Units.removeUnitFromInt interval)
                    |> List.pairwise
                    |> List.averageBy (fun (n, old) -> if n > old then n - old else old - n)
                if improvement <= defaultTolerance then NoImprovement else Continue

    /// Ends when the overall number of improvements made is
    /// greater than `count`.
    let whenImprovementsMade count interval : EndCondition =
        fun results iteration ->
            onlyOnInterval interval iteration <| fun () ->
                let i =
                    results
                    |> List.pairwise
                    |> List.where (fun (x, y) -> fst x < fst y)
                    |> List.length

                if i >= count then
                    OptimStopReason.NoImprovement
                else
                    Continue

    /// End when exploration is not detecting any new
    /// worse results. For use in heating stages of
    /// simulated annealing.
    let whenWorstValuePlateaued window : EndCondition =
        fun results iteration ->
            onlyOnInterval (2 * window) iteration <| fun () ->
                let n = Units.removeUnitFromInt window

                let recentWorst =
                    results |> List.take n |> List.map fst |> List.max

                let olderWorst =
                    results |> List.skip n |> List.take n |> List.map fst |> List.max

                if abs (recentWorst - olderWorst) <= defaultTolerance then
                    Custom "Worst value plateau"
                else Continue

    /// Stops when there is a lack of movement, as indicated by squared jump
    /// distance below threshold across the last n steps.
    let whenStationary threshold nSteps : EndCondition =
        fun results iter ->
            if iter < nSteps then Continue else
            let jumps =
                results
                |> List.take (Units.removeUnitFromInt nSteps)
                |> List.pairwise
                |> List.map (fun ((_,θ1),(_,θ2)) -> Tensors.Typed.squaredLength (θ1 - θ2) |> Tensors.Typed.toFloatScalar)
            if List.forall (fun d -> d < threshold) jumps then
                Stationary
            else Continue

    /// An `EndCondition` that calculates that segregates the most recent n results into
    /// five bins, and runs a regression to detect a temporal variation in the mean
    /// squared jumping distance (MSJD). The significance of the slope coefficient of a linear
    /// regression is assessed to determine if the MSJD is increasing through time for every
    /// parameter sequentially: if all p-values are >0.1, then the `EndCondition` is true.
    let stationarySquaredJumpDistance' fixedBin pointsRequired slopeTol (log: LogEvent -> unit) : EndCondition =
        fun results i ->
            if
                i % (fixedBin * pointsRequired * 1<iteration>) = 0<iteration>
                && i > 1<iteration>
            then
                let slopesAndPs =
                    results
                    |> List.take (fixedBin * pointsRequired)
                    |> List.chunkBySize fixedBin
                    |> List.map (fun bin ->
                        bin
                        |> List.map (snd >> Tensors.Typed.toFloatArray >> Array.toList)
                        |> List.flip
                        |> List.map (fun p ->
                            p
                            |> List.map Units.removeUnitFromFloat
                            |> List.pairwise
                            |> List.averageBy (fun (a, b) -> (b - a) ** 2.)))
                    |> List.flip
                    |> List.map (fun msjds ->
                        let x = [| 1. .. float (List.length msjds) |]
                        Statistics.Regression.slopeAndPValue x (msjds |> List.toArray))

                let valid = slopesAndPs |> List.filter (fun (_, p) -> not (System.Double.IsNaN p))

                let decision =
                    if
                        valid.Length > 0
                        && valid |> List.forall (fun (slope, pVal) -> abs slope < slopeTol && pVal > 0.1)
                    then
                        Stationary
                    else
                        Continue
                decision
            else
                Continue

    /// True if there is no significant slope in mean squared jumping distances (MSJD),
    /// binned per 200 iterations and a regression of five bins.
    let stationarySquaredJumpDistance log : EndCondition =
        stationarySquaredJumpDistance' 200 5 1e-3 log

    /// Stops when acceptance rate is consistently within the
    /// defined range. Used to avoid stopping when not mixing.
    let whenStableAcceptanceRate min max interval intervalsRequired log : EndCondition =
        let intervalN = Units.removeUnitFromInt interval
        let required = Units.removeUnitFromInt intervalsRequired
        fun results iteration ->
            onlyOnInterval interval iteration <| fun () ->
                if iteration < interval * intervalsRequired then Continue else
                
                let bins =
                    results
                    |> List.take (intervalN * required)
                    |> List.chunkBySize intervalN

                let acceptanceRates =
                    bins
                    |> List.map (fun chunk ->
                        let diffs =
                            chunk
                            |> List.pairwise
                            |> List.map (fun (a, b) -> fst a - fst b)

                        let accepted =
                            diffs
                            |> List.where ((=) 0.<``-logL``> >> not)
                            |> List.length

                        float accepted / float diffs.Length
                    )

                let stable =
                    acceptanceRates
                    |> List.forall (fun ar -> ar >= min && ar <= max)

                let result =
                    if stable then
                        OptimStopReason.Custom "Stable"
                    else
                        Continue

                log (GeneralEvent(sprintf "[EndCondition] Acceptance rates = %A [%A]" acceptanceRates result))
                result

    let whenAcceptanceRateOutside min max interval intervalsRequired log : EndCondition =
        fun results iteration ->
            let within = whenStableAcceptanceRate min max interval intervalsRequired log results iteration
            if within = Continue then Custom "Outside AR bound" else Continue

    /// <summary>Stop when the rolling variance of the objective stabilises.</summary>
    /// <param name="window">number of recent samples to compare</param>
    /// <param name="relTol">relative change threshold (e.g. 0.05 = 5%)</param>
    /// <returns>A stop condition or Continue.</returns>
    let whenVarianceStabilised window relTol : EndCondition =
        fun results iteration ->
            rollingWindow (2 * window) results iteration <| fun _ ->
                let n = Units.removeUnitFromInt window

                let recent =
                    results |> List.take n |> List.map fst

                let older =
                    results |> List.skip n |> List.take n |> List.map fst

                let varRecent = Observations.variance recent
                let varOlder  = max (Observations.variance older) 1e-12<``-logL``^2>

                let relChange = abs (varRecent - varOlder) / varOlder

                if relChange < relTol then
                    Custom "Variance stabilised"
                else Continue

    let whenStuck movementFloor interval : EndCondition =
        fun results iteration ->
            onlyOnInterval interval iteration
            <| fun () ->
                let totalMovement =
                    results
                    |> List.truncate (Units.removeUnitFromInt interval)
                    |> List.pairwise
                    |> List.map (fun (a, b) -> abs (fst a - fst b))
                    |> List.sum

                if totalMovement <= movementFloor then
                    OptimStopReason.Stuck
                else
                    Continue


    module Profiles =

        let private regularity = 10<iteration>

        /// Ends the tuning stages of an MCMC algorithm when
        /// the chain appears to be well mixed, or a maximum
        /// iteration count is reached. Mixing is defined by
        /// not being stuck, and the acceptance rate has stabilised
        /// in the range 0.15 - 0.5.
        let mcmcTuningStep maxIter log : EndCondition =
            fun results iter ->
                if atIteration maxIter results iter <> Continue then
                    MaxIterations
                else if
                    whenStableAcceptanceRate 0.15 0.5 regularity 3 log results iter = Custom "Stable"
                    && whenStuck 1e-6<``-logL``> regularity results iter = Continue // not stuck
                then
                    log (GeneralEvent(sprintf "[EndCondition] Well mixed"))
                    Custom "Well mixed"
                else
                    Continue

        /// Ends an MCMC chain when the chain is well-behaved (i.e. acceptance
        /// rate 0.15 - 0.5, and not stuck) and it is stationary (as measured
        /// by windowed squared jump distance on recent 200-iteration windows).
        /// The chain will also be abandoned if `maxIter` iterations are reached.
        let mcmc maxIter log : EndCondition =
            fun results iter ->
                let tuneStatus = mcmcTuningStep maxIter log results iter

                if tuneStatus = Custom "Well mixed" then
                    stationarySquaredJumpDistance' 200 5 1e-3 log results iter
                else
                    tuneStatus


        module SimulatedAnnealing =

            let preTuning =
                combineAny [
                    whenVarianceStabilised 200<iteration> 0.10
                    atIteration 1000<iteration>
                ]

            let heating =
                combineAny [
                    combineAll [
                        whenVarianceStabilised 50<iteration> 0.05
                        whenWorstValuePlateaued 50<iteration>
                    ]
                    atIteration 500<iteration>
                ]

            /// Annealing will stop when either: there is little movement in parameter space;
            /// no improvements are being made; or acceptance rates are very low.
            let annealing =
                combineAny [
                    whenNoBestValueImprovement 100<iteration>
                    whenStationary 1e-6<``optim-space`` ^2> 50<iteration>
                    whenAcceptanceRateOutside 0.1 1.0 regularity 3 ignore
                    atIteration 2000<iteration>
                ]


    module Ensemble =

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
                        let thetaLength =
                            (chains |> Seq.head).Value |> Seq.head |> snd |> Tensors.Typed.length

                        [ 1..thetaLength ]
                        |> Seq.map (fun p ->
                            chainsEqualLength
                            |> Seq.map (fun chain ->
                                chain
                                |> Seq.map (fun v ->
                                    v |> snd |> Tensors.Typed.toFloatValueAt (p - 1) |> Units.removeUnitFromFloat)))

                    printfn "Param values by chain: %A" parameterValueByChain

                    parameterValueByChain
                    |> Seq.map Convergence.GelmanRubin.rHat
                    |> Seq.toList
                    |> Some

            let convergenceAgent =
                MailboxProcessor.Start(fun inbox ->
                    let rec messageLoop chainResults =
                        async {
                            let! (chainId, results, repl: AsyncReplyChannel<OptimStopReason>) = inbox.Receive()
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

                                repl.Reply OptimStopReason.Converged
                            | None -> repl.Reply OptimStopReason.Continue

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
                    Continue
