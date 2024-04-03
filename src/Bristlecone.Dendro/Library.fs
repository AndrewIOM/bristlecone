namespace Bristlecone

open Bristlecone.Time
open Bristlecone.Dendro

module Bristlecone =

    /// Fit a model to plant growth time-series. The plant individual's
    /// growth data is always labelled as `x`.
    let fitDendro engine iterations system (plant: PlantIndividual.PlantIndividual) =
        let g =
            match plant.Growth |> PlantIndividual.growthSeries with
            | GrowthSeries.Absolute g -> g
            | GrowthSeries.Cumulative g -> g
            | GrowthSeries.Relative g -> g

        let predictors = plant.Environment |> Map.add (ShortCode.create "x" |> Option.get) g
        Bristlecone.fit engine iterations predictors system

    /// Perform n-step-ahead computation on the hypothesis and plant.
    /// The plant individual's growth data is always labelled as `x`.
    let predictAheadDendro
        (engine: EstimationEngine.EstimationEngine<float, float>)
        system
        (plant: PlantIndividual.PlantIndividual)
        preTransform
        estimate
        =
        let g =
            match plant.Growth |> PlantIndividual.growthSeries with
            | GrowthSeries.Absolute g -> g
            | GrowthSeries.Cumulative g -> g
            | GrowthSeries.Relative g -> g

        let predictors = plant.Environment |> Map.add (ShortCode.create "x" |> Option.get) g
        Bristlecone.oneStepAhead engine system preTransform predictors estimate

//
//module Test =
//
//    // How to process plants?
//    // 1. Where time-series have variable start and end dates, bound to common time.
//    // 2. Determine conditioning based on real data at new t-1, or can be calculated by allometrics.
//    // 3.
//
//    type GrowthModellingBasis =
//        | Biomass
//        | AbsoluteGrowthRate
//        | RelativeGrowthRate
//
//    let startValues (startDate:System.DateTime) (plant:PlantIndividual.PlantIndividual) =
//        let initialRadius =
//            match plant.Growth with
//            | PlantIndividual.PlantGrowth.RingWidth s ->
//                match s with
//                | GrowthSeries.Absolute c -> c.Head |> fst |> removeUnit
//                | GrowthSeries.Cumulative c ->
//                    let start = (c |> TimeSeries.trimStart (startDate - System.TimeSpan.FromDays(366.))).Values |> Seq.head |> removeUnit
//                    printfn "Start cumulative growth = %f" start
//                    start
//                | GrowthSeries.Relative _ -> invalidOp "Not implemented"
//            | _ -> invalidOp "Not implemented 2"
//        let initialMass = initialRadius |> removeUnit |> ModelComponents.Proxies.toBiomassMM
//        let initialNitrogen = plant.Environment.[ShortCode.create "N"].Head |> fst
//        let initialT = plant.Environment.[code "T[max]"] |> TimeSeries.findExact startDate |> fst
//        [ (ShortCode.create "x", initialRadius)
//          (ShortCode.create "N", initialNitrogen)
//          (ShortCode.create "T[max]", initialT)
//          (ShortCode.create "bs", initialMass) ] |> Map.ofList
//
//    let preparePlantAnalysis growthBasis localEnvironment plant =
//
//        // 1. Add environment to plant
//        let plant2 =
//            localEnvironment
//            |> List.fold(fun plant (c,ts) -> plant |> PlantIndividual.zipEnv c ts) plant
//
//        // 2. Transform growth into required basis
//        let plant3 =
//            match growthBasis with
//            | Biomass -> plant |> PlantIndividual.toCumulativeGrowth
//            | _ -> failwith "Not finished"
//
//        // 3.
//
//        let shrub = s |> PlantIndividual.toCumulativeGrowth |> PlantIndividual.zipEnv (code "T[max]" |> Option.get) (TemperatureData.monthly)
//        let common = shrub |> PlantIndividual.keepCommonYears |> PlantIndividual.zipEnv (code "T[max]" |> Option.get) (TemperatureData.monthly)
//        let startDate = (common.Environment.[code "N" |> Option.get]).StartDate |> snd
//        let startConditions = getStartValues startDate shrub
//        let e = engine |> Bristlecone.withConditioning (Conditioning.Custom startConditions)
//
//
//        failwith "Cool"
//
//
//    type StartStrategy =
//        | CommonTime
//        | FirstGrowthYear
