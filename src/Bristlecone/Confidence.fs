namespace Bristlecone.Confidence

open Bristlecone
open Bristlecone.Logging
open Bristlecone.ModelSystem

/// <namespacedoc>
///   <summary>Contains functions for calculating confidence intervals on model fits.</summary>
/// </namespacedoc>
///
/// The 95% and 68% confidence interval around a point estimate.
type ConfidenceInterval =
    { ``95%``: Interval
      ``68%``: Interval
      Estimate: float }

and Interval =
    { Lower: float<parameter>
      Upper: float<parameter> }

/// <summary>Keeps a list of all optimisation events that occur for further analysis.</summary>
type internal OptimisationEventStash() =
    let mutable events = []

    member __.SaveEvent(e) =
        match e with
        | OptimisationEvent o -> events <- (events |> List.append [ o ])
        | _ -> ()

    member __.GetAll() = events


/// <summary>Differences in likelihood for different confidence intervals
/// based on a chi squared distribution.</summary>
module Bounds =

    /// <summary>The difference in likelihood at 68% confidence</summary>
    let lowerBound = 0.49447324<``-logL``> // qchisq(0.68,1)/2

    /// <summary>The difference in likelihood at 95% confidence</summary>
    let upperBound = 1.92072941<``-logL``> // qchisq(0.95,1)/2


/// <summary>Given a maximum likelihood estimate (MLE), the profile likelihood method
/// runs a Monte Carlo algorithm that samples around the MLE.</summary>
/// <remarks>The range for each parameter is discovered at 95% and 68%
/// confidence based on a chi squared distribution.</remarks>
module ProfileLikelihood =

    open Bristlecone.EstimationEngine
    open Bristlecone.Optimisation

    type EstimateFunction<'S,'V,'M,[<Measure>] 'modelTimeUnit, [<Measure>] 'state, 'subject, 'date, 'timeunit, 'timespan> =
        EstimationEngine<'S,'V,'M, 'date, 'timespan, 'modelTimeUnit, 'state>
            -> ModelSystem<'S,'V,'modelTimeUnit>
            -> 'subject
            -> EstimationResult<'date, 'timeunit, 'timespan>

    let interval nParam mle limit (trace: (float<``-logL``> * float<parameter> array) list) =
        trace
        |> List.filter (fun (l, _) -> l - mle < limit)
        |> List.fold
            (fun best (_, p) -> best |> Array.zip p |> Array.map (fun (v, (mn, mx)) -> ((min v mn), (max v mx))))
            (Array.init nParam (fun _ ->
                (System.Double.MaxValue |> LanguagePrimitives.FloatWithMeasure<parameter>,
                 System.Double.MinValue |> LanguagePrimitives.FloatWithMeasure<parameter>)))

    /// The profile likelihood method samples the likelihood space
    /// around the Maximum Likelihood Estimate
    let profile fit engine subject (hypothesis: ModelSystem<'S,'V,'modelTimeUnit>) n result =

        // Start at the MLE
        let hypothesisMle =
            { hypothesis with
                Parameters = Parameter.Pool.fromEstimated result.Parameters }

        // Perturb and re‑fit locally around the MLE
        let mle = result.Likelihood
        let transforms = Parameter.Pool.compileTransformsBounded result.Parameters

        let customFit =
            fit
                { engine with
                    OptimiseWith =
                        MonteCarlo.SimulatedAnnealing.Tuning.perturb
                            0.01
                            { MonteCarlo.SimulatedAnnealing.Tuning.TuningSettings.Default with
                                MinTuneLength = n * 2<iteration> } }

        let rec fit' currentTrace =
            let a = customFit (EndConditions.atIteration 100<iteration>) subject hypothesisMle

            let validTrace =
                a.Trace
                |> List.map (fun t -> t.Results)
                |> List.concat
                |> List.filter (fun (l, _) -> l - mle < 2.00<``-logL``> && l - mle > 0.00<``-logL``>)
                |> List.distinct

            engine.LogTo
            <| GeneralEvent(sprintf "Profiling efficiency: %f/1.0." ((validTrace |> List.length |> float) / (float n)))

            let updatedTrace = List.append currentTrace validTrace

            if updatedTrace.Length > n then
                updatedTrace
            else
                fit' updatedTrace

        let trace = fit' []

        // 3. Calculate min and max at the specified limit for each parameter
        let lowerInterval =
            trace
            |> interval (Parameter.Pool.count hypothesisMle.Parameters) mle Bounds.lowerBound

        let upperInterval =
            trace
            |> interval (Parameter.Pool.count hypothesisMle.Parameters) mle Bounds.upperBound

        let paramKeys = Parameter.Pool.toArrayReal result.Parameters |> Seq.map fst

        paramKeys
        |> Seq.zip3 lowerInterval upperInterval
        |> Seq.map (fun ((l1, l2), (u1, u2), sc) ->
            match result.Parameters |> Parameter.Pool.tryGetRealValue sc.Value with
            | None -> failwithf "%A" sc
            | Some v ->
                sc,
                { Estimate = v
                  ``68%`` = { Lower = l1; Upper = l2 }
                  ``95%`` = { Lower = u1; Upper = u2 } })
        |> Map.ofSeq
