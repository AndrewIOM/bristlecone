namespace Bristlecone.ModelSelection

module Akaike =

    open Bristlecone.ModelSystem

    ///**Description**
    /// The Akaike information criterion, a standardised index of model fit quality for models that have different numbers of parameters.
    ///**Parameters**
    ///  * `k` - The number of parameters within the model in question.
    ///  * `logLikelihood` - a `float` representing the minimum log-likelihood achieved for the model in question.
    let aic (k:int) logLikelihood =
        2. * (float k) - 2. * logLikelihood


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

    
    
    /// **Description**
    /// Akaike weights for a set of models.
    /// The weights can be directly interpreted as conditional probabilities for each model.
    /// 
    /// **Output Type**
    ///   * A `seq<EstimationResult * float>` of estimation results paired to their Akaike weights.
    ///
    /// **Exceptions**
    ///   * `ArgumentException` - occurs when there are no observations within an estimation result.
    let akaikeWeights (models:EstimationResult seq) =
        let aiccs =
            models
            |> Seq.map(fun m ->
                let n = (m.Series |> Seq.head).Value.Observed.Length
                aicc n m.Parameters.Count m.Likelihood)
        let relativeLikelihoods =
            aiccs
            |> Seq.map(fun aicc -> exp ( - (1. / 2.) * (aicc - (aiccs |> Seq.min))))
        let totalLikelihood = relativeLikelihoods |> Seq.sum
        relativeLikelihoods
        |> Seq.map (fun relative -> relative / totalLikelihood)
        |> Seq.zip models