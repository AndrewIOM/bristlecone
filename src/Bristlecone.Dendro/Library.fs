namespace Bristlecone

open Bristlecone.Time
open Bristlecone.Dendro

module Bristlecone =

    /// <summary>Lower time-series data from a plant individual type into
    /// basic time-series that may be used for model-fitting</summary>
    /// <param name="plant">A plant individual</param>
    /// <returns>A coded map of time-series data for model-fitting</returns>
    let fromDendro (plant:PlantIndividual.PlantIndividual) =
        let g =
            match plant.Growth |> PlantIndividual.growthSeries with
            | GrowthSeries.Absolute g -> g
            | GrowthSeries.Cumulative g -> g
            | GrowthSeries.Relative g -> g
        plant.Environment 
        |> Map.add (ShortCode.create "x" |> Option.get) g

    /// <summary>Fit a model to plant growth time-series. The plant individual's growth data is always labelled as `x`.</summary>
    /// <param name="engine">A configured estimation engine</param>
    /// <param name="endCondition">The end condition to apply to optimisation</param>
    /// <param name="system">The compiled model system for analysis</param>
    /// <param name="plant">A plant individual</param>
    /// <returns>An estimation result for the given plant, model and fitting method (engine)</returns>
    let fitDendro engine endCondition system (plant: PlantIndividual.PlantIndividual) =
        Bristlecone.fit engine endCondition (fromDendro plant) system

    /// <summary> Perform n-step-ahead computation on the hypothesis and plant. The plant individual's growth data is always labelled as `x`.</summary>
    /// <param name="engine"></param>
    /// <param name="system"></param>
    /// <param name="plant"></param>
    /// <param name="preTransform"></param>
    /// <param name="estimate"></param>
    /// <returns></returns>
    let oneStepAheadDendro
        (engine: EstimationEngine.EstimationEngine<float, float>)
        system
        (plant: PlantIndividual.PlantIndividual)
        preTransform
        estimate
        =
        let predictors = fromDendro plant
        Bristlecone.oneStepAhead engine system preTransform predictors estimate
