namespace Bristlecone.Optimisation.ConfidenceInterval

open Bristlecone.Logging

/// The 95% and 68% confidence interval around a point estimate.
type ConfidenceInterval = {
    ``95%``: Interval
    ``68%``: Interval
    Estimate: float
}

and Interval = {
    Lower: float
    Upper: float
}

/// Keeps a list of all optimisation events that occur for further analysis.
type OptimisationEventStash() =
    let mutable events = []
    with
        member __.SaveEvent(e) =
            match e with
            | OptimisationEvent o -> events <- (events |> List.append [o])
            | _ -> ()

        member __.GetAll() = events


/// Differences in likelihood for different confidence intervals
/// based on a chi squared distribution.
module Bounds =

    /// The difference in likelihood at 68% confidence
    let lowerBound = 0.49447324 // qchisq(0.68,1)/2

    /// The difference in likelihood at 95% confidence
    let upperBound = 1.92072941 // qchisq(0.95,1)/2


/// Given a maximum likelihood estimate (MLE), the profile likelihood method
/// runs a Monte Carlo algorithm that samples around the MLE. The range for
/// each parameter is discovered at 95% and 68% confidence based on a chi squared
/// distribution.
module ProfileLikelihood =

    open Bristlecone
    open Bristlecone.EstimationEngine
    open Bristlecone.ModelSystem
    open Bristlecone.Optimisation

    type EstimateFunction<'data,'time,'subject> =
        EstimationEngine<'data,'time> -> ModelSystem -> 'subject -> EstimationResult


    module CustomOptimisationMethod =

        type TuneSettings = {
            MinimumSampleSize: int
            InitialScale: float
            KMax: int
            TuneN: int
        } 
        with 
            static member Default = 
                { MinimumSampleSize = 60000
                  InitialScale = 0.001
                  KMax = 60000
                  TuneN = 50 }

        // This algorithm only differs from SA because it outputs all iterations
        // in the tunedSearch as optimisation events.
        let tunedSearch random settings machine (jump:System.Random->float->unit->float) writeOut domain f =

            // 1. Initial conditions
            let draw' = jump random
            let theta1 = Bristlecone.Optimisation.MonteCarlo.initialise domain random
            let l1 = f theta1

            // 2. Tune individual parameter scales (for many univariate distributions)
            let initialScale = 
                [| 1 .. theta1.Length |] |> Array.map (fun _ -> settings.InitialScale)

            let rec tune (p:(float*float[])[]) k results (l1, theta1) =
                let chance = (float k) / (float settings.KMax)
                let parameterToChange = random.Next(0, (p |> Array.length) - 1)
                let scalesToChange = p |> Array.mapi (fun i x -> (x, random.NextDouble() < chance || i = parameterToChange))
                let propose theta =
                    Array.zip3 theta scalesToChange domain
                    |> Array.map(fun (x,((ti,n),shouldChange),(_,_,con)) -> 
                        if shouldChange
                        then Bristlecone.Optimisation.MonteCarlo.constrainJump x (draw' ti ()) 1. con
                        else x )
                let result = Bristlecone.Optimisation.MonteCarlo.SimulatedAnnealing.tryMove propose (machine 1.) random f (l1, theta1)
                let newScaleInfo = 
                    scalesToChange 
                    |> Array.zip (result |> snd)
                    |> Array.map(fun (v, ((ti,previous),changed)) ->
                        let ti, previous =
                            if changed then (ti, (previous |> Array.append [|v|]))  // Append new parameter values to previous ones
                            else (ti, previous)
                        if previous |> Array.length = settings.TuneN
                        then
                            let changes = previous |> Array.pairwise |> Array.where(fun (a,b) -> a <> b) |> Array.length
                            match (float changes) / (float settings.TuneN) with
                            | ar when ar < 0.35 -> (ti * 0.80, Array.empty)
                            | ar when ar > 0.50 -> (ti * 1.20, Array.empty)
                            | _ -> (ti, Array.empty)
                        else (ti, previous) )

                if k % 1000 = 0 then
                    writeOut <| GeneralEvent (sprintf "Tuning is at %A (k=%i/%i)" (newScaleInfo |> Array.map fst) k settings.KMax)

                if k < settings.KMax
                then
                    writeOut <| OptimisationEvent { Iteration = k; Likelihood = result |> fst; Theta = result |> snd } 
                    tune newScaleInfo (k + 1) (results |> List.append [result]) result
                else newScaleInfo |> Array.map fst

            let result = tune (initialScale |> Array.map(fun t -> (t, Array.empty))) 1 [] (l1, theta1)
            writeOut <| GeneralEvent (sprintf "Tuned = %A" result)
            [(l1, theta1)]

        let classic settings : Optimise<float> =
            fun random writeOut n domain _ f ->
                let gaussian rnd scale = Bristlecone.Statistics.Distributions.Normal.draw rnd 0. scale
                tunedSearch random settings MonteCarlo.SimulatedAnnealing.Machines.boltzmann gaussian writeOut domain f

    let interval nParam mle limit trace =
        trace
        |> List.filter(fun (l,p) -> (l - mle) < limit)
        |> List.fold(fun best (l,p) -> best |> Array.zip p |> Array.map(fun (v,(mn,mx)) -> ((min v mn), (max v mx)))) 
            (Array.init nParam (fun _ -> (System.Double.MaxValue, System.Double.MinValue)))

    /// The profile likelihood method samples the likelihood space
    /// around the Maximum Likelihood Estimate 
    let profile fit engine subject (hypothesis:ModelSystem) n (result:EstimationResult) =

        // 1. Set estimation bounds to the MLE
        let hypothesisMle =  { hypothesis with Parameters = Parameter.Pool.fromEstimated result.Parameters }

        // 2. Generate a trace of at least n samples that deviate in L less than 2.0
        let results = OptimisationEventStash()
        let mle = result.Likelihood

        let customFit = fit { engine with OptimiseWith = CustomOptimisationMethod.classic CustomOptimisationMethod.TuneSettings.Default; LogTo = fun e -> engine.LogTo e; results.SaveEvent e }
        let rec fit' currentTrace =
            let a = customFit (EndConditions.afterIteration n) subject hypothesisMle
            let validTrace = a.Trace |> List.filter(fun (l,_) -> (l - mle) < 2.00 && (l - mle) > 0.00) |> List.distinct
            engine.LogTo <| GeneralEvent (sprintf "Profiling efficiency: %f/1.0." ((validTrace |> List.length |> float) / (float n)))
            currentTrace |> List.append validTrace
        fit' [] |> ignore

        let trace = 
            results.GetAll()
            |> List.map(fun s -> (s.Likelihood, s.Theta |> Seq.toArray))
        engine.LogTo <| GeneralEvent (sprintf "Actual trace was %A" trace)

        // 3. Calculate min and max at the specified limit for each parameter
        let lowerInterval = trace |> interval (Parameter.Pool.count hypothesisMle.Parameters) mle Bounds.lowerBound
        let upperInterval = trace |> interval (Parameter.Pool.count hypothesisMle.Parameters) mle Bounds.upperBound

        result.Parameters
        |> Parameter.Pool.toList
        |> Seq.zip3 lowerInterval upperInterval
        |> Seq.map(fun ((l1,l2),(u1,u2),(k,v)) -> 
            match v |> Parameter.getEstimate with
            | Error e -> failwith e
            | Ok v ->
                k, {
                    Estimate = v
                    ``68%`` = { Lower = l1; Upper = l2 }
                    ``95%`` = { Lower = u1; Upper = u2 }})
        |> Map.ofSeq