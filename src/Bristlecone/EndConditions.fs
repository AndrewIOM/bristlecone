namespace Bristlecone.Optimisation

open Bristlecone
open Bristlecone.Logging
open Bristlecone.EstimationEngine
open Bristlecone.Statistics

module EndConditions =

    /// End the optimisation procedure when a minimum number of iterations is exceeded.
    let afterIteration iteration : EndCondition =
        fun _ currentIteration ->
            if currentIteration >= iteration then OptimStopReason.MaxIterations
            else OptimStopReason.Continue

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

    let noImprovement : EndCondition =
        fun results iteration ->
        let imp =
            results
            |> List.map fst
            |> List.pairwise
            |> List.averageBy (fun (n, old) -> if n > old then n - old else old - n) // Mean change in objective value
        if imp <= defaultTolerance then NoImprovement
        else Continue

    let improvementCount count interval : EndCondition =
        fun results iteration ->
            if iteration % interval = 0<iteration> then
                let i = results |> List.pairwise |> List.where (fun (x, y) -> fst x < fst y) |> List.length
                if  i >= count
                then OptimStopReason.NoImprovement
                else Continue
            else
                Continue

    /// An `EndCondition` that calculates that segregates the most recent n results into
    /// five bins, and runs a regression to detect a temporal variation in the mean
    /// squared jumping distance (MSJD). The significance of the slope coefficient of a linear
    /// regression is assessed to determine if the MSJD is increasing through time for every
    /// parameter sequentially: if all p-values are >0.1, then the `EndCondition` is true.
    let stationarySquaredJumpDistance' fixedBin pointsRequired slopeTol (log: LogEvent -> unit) : EndCondition =
        fun results i ->
            if i % (fixedBin * pointsRequired * 1<iteration>) = 0<iteration> && i > 1<iteration> then
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

                slopesAndPs
                |> List.iteri (fun paramIdx (slope, p) ->
                    log (GeneralEvent (sprintf "[EndCondition] Param %d: slope=%.6g, p=%.4f"
                                            (paramIdx + 1) slope p)))

                let valid = slopesAndPs |> List.filter (fun (_, p) -> not (System.Double.IsNaN p))

                let decision =
                    if valid.Length > 0
                        && valid |> List.forall (fun (slope, p) -> abs slope < slopeTol && p > 0.1)
                    then Stationary
                    else Continue
            
                log (GeneralEvent (sprintf "[EndCondition] Decision at iter %d: %A (valid=%d/%d)"
                                        (int i) decision valid.Length slopesAndPs.Length))

                decision
            else Continue

    /// True if there is no significant slope in mean squared jumping distances (MSJD),
    /// binned per 200 iterations and a regression of five bins.
    let stationarySquaredJumpDistance log: EndCondition =
        stationarySquaredJumpDistance' 200 5 1e-3 log

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
                        |> Seq.map (fun chain -> chain |> Seq.map (fun v -> v |> snd |> Tensors.Typed.toFloatValueAt (p - 1) |> Units.removeUnitFromFloat)))

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

    module Profiles =

        /// Combine multiple EndConditions into one, returning the first non-Continue reason.
        let combineEndConditions (conditions: EndCondition list) : EndCondition =
            fun results iter ->
                conditions
                |> Seq.map (fun cond -> cond results iter)
                |> Seq.tryFind (fun reason -> reason <> Continue)
                |> Option.defaultValue Continue

        /// Composite end conditions well-suited to basic MCMC algorithms.
        let mcmcProfile log =
            combineEndConditions [
                stationarySquaredJumpDistance' 200 5 1e-3 log
                // acceptanceRateGate 0.15 0.5
                // movementFloorGate 1e-6 ]
            ]

        /// Composite end conditions well-suited to simulated annealing.
        let saProfile tol =
            combineEndConditions [
                noImprovement
                improvementCount 3 100<iteration>
            ]
