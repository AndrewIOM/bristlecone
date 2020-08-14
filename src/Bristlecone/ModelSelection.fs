module Bristlecone.ModelSelection

open Bristlecone
open Bristlecone.ModelSystem

/// Organises multiple hypotheses and multiple subjects into
/// distinct analysis groups.
[<RequireQualifiedAccess>]
module ResultSet =

    /// A representation of all results for a particular subject and hypothesis
    type ResultSet<'subject,'hypothesis> = ('subject * 'hypothesis * int * (EstimationResult seq * EstimationResult) option)

    /// Arrange estimation results into subject and hypothesis groups.
    let arrangeResultSets subjects hypotheses getResults : ResultSet<'a,'hypothesis> seq =
        Seq.allPairs subjects (hypotheses |> Seq.mapi(fun i v -> (i+1,v)))
        |> Seq.map (fun (s,(hi,h)) ->
            let r = getResults s h hi
            if r |> Seq.isEmpty
            then (s, h, hi, None )
            else 
                let r' = r |> Seq.filter(fun x -> not (System.Double.IsNaN(x.Likelihood)))
                if Seq.isEmpty r'
                then (s, h, hi, None )
                else (s, h, hi, (r', r' |> Seq.minBy(fun x -> x.Likelihood)) |> Some))

/// Functions for conducting Akaike Information Criterion (AIC).
module Akaike =

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
    
    let internal akaikeWeights' n modelResults : seq<AkaikeWeight> =
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
            |> Seq.map(fun m -> (m.Likelihood, Bristlecone.Parameter.Pool.count m.Parameters))
            |> akaikeWeights' n 
            |> Seq.zip models


/// A record of an individual maximum likelihood estimate for 
/// a particular subject and hypothesis.
type Result<'subject> = {
    AnalysisId: System.Guid
    Subject: 'subject
    ModelId: string
    Estimate: EstimationResult
}

/// Given a list of model predictions, find the best MLE for each
/// model * subject combination, calculate the weights for this set.
let calculate (results:seq<Result<'subject>>) =
    results
    |> Seq.groupBy(fun r -> (r.Subject, r.ModelId))
    |> Seq.map(fun (_,r) -> r |> Seq.minBy(fun x -> x.Estimate.Likelihood))
    |> Seq.groupBy(fun r -> r.Subject)
    |> Seq.collect(fun (_,r) ->
        let weights = r |> Seq.map(fun r -> r.Estimate) |> Akaike.akaikeWeights |> Seq.map snd
        weights
        |> Seq.zip r )

/// 
let weights (results:ResultSet.ResultSet<'a,'b> seq) = 
    results
    |> Seq.groupBy(fun (identifier,_,_,_) -> identifier)
    |> Seq.collect(fun (_,r) ->
        let weights =
            r 
            |> Seq.choose(fun (_,_,_,r) -> r)
            |> Seq.map(fun (_,mle) -> mle)
            |> Akaike.akaikeWeights 
        r
        |> Seq.zip weights
        |> Seq.map (fun (w,(s,_,hi,_)) -> s, hi, fst w, snd w)
        |> Seq.toList )
    |> Seq.toList