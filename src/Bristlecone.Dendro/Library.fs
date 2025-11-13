namespace Bristlecone

open Bristlecone.Time
open Bristlecone.Dendro

module Bristlecone =

    type FittingMethod =
        | AbsoluteGrowth
        | CumulativeGrowth

    /// <summary>Fit a model to plant growth time-series. The plant individual's growth data is always labelled as `x`.</summary>
    /// <param name="engine">A configured estimation engine</param>
    /// <param name="endCondition">The end condition to apply to optimisation</param>
    /// <param name="system">The compiled model system for analysis</param>
    /// <param name="plant">A plant individual</param>
    /// <returns>An estimation result for the given plant, model and fitting method (engine)</returns>
    let fitDendro engine endCondition system fitMethod growthSeriesId plant =
        let data =
            match fitMethod with
            | AbsoluteGrowth -> failwith "absolute not implemented"
            | CumulativeGrowth -> PlantIndividual.forFittingCumulative growthSeriesId plant

        Bristlecone.fit engine endCondition data system

    let tryFitDendro engine endCondition system fitMethod growthSeriesId plant =
        let data =
            match fitMethod with
            | AbsoluteGrowth -> failwith "absolute not implemented"
            | CumulativeGrowth -> PlantIndividual.forFittingCumulative growthSeriesId plant

        Bristlecone.tryFit engine endCondition data system

    /// <summary> Perform n-step-ahead computation on the hypothesis and plant. The plant individual's growth data is always labelled as `x`.</summary>
    /// <param name="engine"></param>
    /// <param name="system"></param>
    /// <param name="plant"></param>
    /// <param name="preTransform"></param>
    /// <param name="estimate"></param>
    /// <returns></returns>
    let oneStepAheadDendro engine system fitMethod growthSeriesId plant preTransform estimate =
        let predictors =
            match fitMethod with
            | AbsoluteGrowth -> failwith "absolute not implemented"
            | CumulativeGrowth -> PlantIndividual.forFittingCumulative growthSeriesId plant

        Bristlecone.oneStepAhead engine system preTransform predictors estimate
