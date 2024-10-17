/// <summary>Contains tools for conducting Model Selection across
/// individual subjects and hypotheses.</summary>
module Bristlecone.ModelSelection

open Bristlecone
open Bristlecone.ModelSystem

/// <summary>Organises multiple hypotheses and multiple subjects into distinct analysis groups.</summary>
[<RequireQualifiedAccess>]
module ResultSet =

    /// <summary>A representation of all results for a particular subject and hypothesis</summary>
    type ResultSet<'subject, 'hypothesis, 'date, 'timeunit, 'timespan> =
        { Subject: 'subject
          Hypothesis: 'hypothesis
          BestResult: EstimationResult<'date, 'timeunit, 'timespan> option
          AllResults: EstimationResult<'date, 'timeunit, 'timespan> seq }

    /// <summary>Arrange estimation results into subject and hypothesis groups.</summary>
    /// <param name="subjects"></param>
    /// <param name="hypotheses"></param>
    /// <param name="getResults"></param>
    /// <typeparam name="'subject"></typeparam>
    /// <typeparam name="'hypothesis"></typeparam>
    /// <typeparam name="'a">An estimation result</typeparam>
    /// <returns></returns>
    let arrangeResultSets<'subject, 'hypothesis> (subjects: 'subject seq) (hypotheses: 'hypothesis seq) getResults =
        Seq.allPairs subjects hypotheses
        |> Seq.map (fun (s, h) ->
            let r = getResults s h

            if r |> Seq.isEmpty then
                { Subject = s
                  Hypothesis = h
                  BestResult = None
                  AllResults = [] }
            else
                let r' = r |> Seq.filter (fun x -> not (System.Double.IsNaN(x.Likelihood)))

                if Seq.isEmpty r' then
                    { Subject = s
                      Hypothesis = h
                      BestResult = None
                      AllResults = [] }
                else
                    { Subject = s
                      Hypothesis = h
                      BestResult = r' |> Seq.minBy (fun x -> x.Likelihood) |> Some
                      AllResults = r' })

/// <summary>Runs a model comparison statistic across a sequence of
/// `ResultSet`s. Uses the best MLE for each subject * hypothesis group
/// an runs `comparisonFn` across these results.</summary>
/// <returns>The subject, hypothesis code, the original result, and the statistic.</returns>
let internal comparisonStatistic
    comparisonFn
    getRefCode
    (results: ResultSet.ResultSet<'subject, 'hypothesis, 'date, 'timeunit, 'timespan> seq)
    =
    results
    |> Seq.groupBy (fun resultSet -> getRefCode resultSet.Hypothesis)
    |> Seq.collect (fun (_, r) ->
        let weights =
            r |> Seq.choose (fun resultSet -> resultSet.BestResult) |> comparisonFn

        r
        |> Seq.zip weights
        |> Seq.map (fun (w, resultSet) -> resultSet.Subject, getRefCode resultSet.Hypothesis, fst w, snd w)
        |> Seq.toList)
    |> Seq.toList


/// <summary>Functions for conducting Akaike Information Criterion (AIC).</summary>
module Akaike =

    type AkaikeWeight =
        { Likelihood: float
          AIC: float
          AICc: float
          Weight: float }

    /// <summary>The Akaike information criterion, a standardised index of model fit quality for models that have different numbers of parameters.</summary>
    /// <param name="k">The number of parameters within the model in question.</param>
    /// <param name="logLikelihood">a `float` representing the minimum log-likelihood achieved for the model in question.</param>
    /// <returns></returns>
    let aic (k: int) logLikelihood = 2. * logLikelihood + 2. * (float k)

    /// <summary>The Akaike information criterion, corrected for small sample sizes.
    /// It represents standardised index of model fit quality for models that have different numbers of parameters.</summary>
    /// <remarks>Your model must adhere to the following assumptions:
    /// - Univariate
    /// - Linear in parameters
    /// - Normally-distributed residuals
    /// </remarks>
    /// <param name="n">The sample size</param>
    /// <param name="k">The number of parameters within the model in question</param>
    /// <param name="logLikelihood">A `float` representing the minimum log-likelihood achieved for the model in question.</param>
    /// <returns></returns>
    let aicc n k logLikelihood =
        let aic = aic k logLikelihood

        let correction =
            (2. * ((float k) ** 2.) + 2. * (float k)) / ((float n) - (float k) - 1.)

        aic + correction

    let internal akaikeWeights' n modelResults : seq<AkaikeWeight> =
        let aiccs = modelResults |> Seq.map (fun (l, p) -> aicc n p l)

        let relativeLikelihoods =
            aiccs |> Seq.map (fun aicc -> exp (-(1. / 2.) * (aicc - (aiccs |> Seq.min))))

        let totalLikelihood = relativeLikelihoods |> Seq.sum

        relativeLikelihoods
        |> Seq.zip modelResults
        |> Seq.map (fun ((l, p), relative) ->
            { Likelihood = l
              AIC = aic p l
              AICc = aicc n p l
              Weight = relative / totalLikelihood })

    /// <summary>Akaike weights for a sequence of `EstimationResult`s.</summary>
    /// <param name="models">The input model results</param>
    /// <returns>An (EstimationResult * float) sequence of estimation results paired to their Akaike weights.</returns>
    /// <exception name="ArgumentException">Occurs when there are no observations within an estimation result.</exception>
    let akaikeWeights (models: seq<EstimationResult<'date, 'timeunit, 'timespan>>) =
        match models |> Seq.tryHead with
        | None -> seq []
        | Some m ->
            let n = (m.Series |> Seq.head).Value.Length

            models
            |> Seq.map (fun m -> (m.Likelihood, Bristlecone.Parameter.Pool.count m.Parameters))
            |> akaikeWeights' n
            |> Seq.zip models

    /// <summary>Akaike weights for a result set.</summary>
    /// <param name="getRefCode">A function that gets a short reference code from a hypothesis.</param>
    /// <param name="set">A sequence of `ResultSet`s, within each the 1 .. many results of a particular subject * hypothesis combination.</param>
    /// <returns>An `(EstimationResult * float) seq` of estimation results paired to their Akaike weights.</returns>
    /// <exception cref="ArgumentException">Occurs when there are no observations within an estimation result.</exception>
    let akaikeWeightsForSet getRefCode set =
        comparisonStatistic akaikeWeights getRefCode set
