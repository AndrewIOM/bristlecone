namespace Bristlecone.ModelSelection

module Akaike =

    open Bristlecone.ModelSystem
    open Bristlecone.Time

    type AkaikeWeight = {
        Likelihood: float
        AIC: float
        AICc: float
        Weight: float
    }

    ///**Description**
    /// The Akaike information criterion, a standardised index of model fit quality for models that have different numbers of parameters.
    ///**Parameters**
    ///  * `k` - The number of parameters within the model in question.
    ///  * `logLikelihood` - a `float` representing the minimum log-likelihood achieved for the model in question.
    let aic (k:int) logLikelihood =
        2. * logLikelihood + 2. * (float k)


    ///**Description**
    /// The Akaike information criterion, corrected for small sample sizes. 
    /// It represents standardised index of model fit quality for models that have different numbers of parameters.
    ///**Assumptions**
    /// Your model must adhere to the following assumptions:
    /// - Univariate
    /// - Linear in parameters
    /// - Normally-distributed residuals
    ///**Parameters**
    ///  * `n` - The sample size 
    ///  * `k` - The number of parameters within the model in question.
    ///  * `logLikelihood` - a `float` representing the minimum log-likelihood achieved for the model in question.
    let aicc n k logLikelihood =
        let aic = aic k logLikelihood
        let correction = (2. * ((float k) ** 2.) + 2. * (float k)) / ((float n) - (float k) - 1.)
        aic + correction
    
    let akaikeWeights' n modelResults : seq<AkaikeWeight> =
        let aiccs = modelResults |> Seq.map(fun (l,p) -> aicc n p l)
        let relativeLikelihoods =
            aiccs
            |> Seq.map(fun aicc -> exp ( - (1. / 2.) * (aicc - (aiccs |> Seq.min))))
        let totalLikelihood = relativeLikelihoods |> Seq.sum
        relativeLikelihoods
        |> Seq.zip modelResults
        |> Seq.map (fun ((l,p),relative) -> 
            { Likelihood = l
              AIC = aic p l
              AICc = aicc n p l
              Weight = relative / totalLikelihood })

    /// **Description**
    /// Akaike weights for a set of model results.
    /// The weights can be directly interpreted as conditional probabilities for each model.
    /// 
    /// **Output Type**
    ///   * A `seq<EstimationResult * float>` of estimation results paired to their Akaike weights.
    ///
    /// **Exceptions**
    ///   * `ArgumentException` - occurs when there are no observations within an estimation result.
    let akaikeWeights (models:seq<EstimationResult>) =
        match models |> Seq.tryHead with
        | None -> seq[]
        | Some m ->
            let n = (m.Series |> Seq.head).Value.Length
            models
            |> Seq.map(fun m -> (m.Likelihood, m.Parameters.Count))
            |> akaikeWeights' n 
            |> Seq.zip models


module Select =

    open Bristlecone.ModelSystem

    type Result = {
        AnalysisId: System.Guid
        Subject: string
        ModelId: string
        Estimate: EstimationResult
    }

    /// Given a list of model predictions, find the best MLE for each
    /// model * subject combination, calculate the weights for this set.
    let calculate (results:seq<Result>) =
        results
        |> Seq.groupBy(fun r -> (r.Subject, r.ModelId))
        |> Seq.map(fun (_,r) -> r |> Seq.minBy(fun x -> x.Estimate.Likelihood))
        |> Seq.groupBy(fun r -> r.Subject)
        |> Seq.collect(fun (_,r) ->
            let weights = r |> Seq.map(fun r -> r.Estimate) |> Akaike.akaikeWeights |> Seq.map snd
            weights
            |> Seq.zip r )
