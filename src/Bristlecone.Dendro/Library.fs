namespace Bristlecone

open Time
open Dendro.PlantIndividual

module Bristlecone =

    module PlantIndividual =

        let fit engine iterations system (plant:PlantIndividual) =
            let g =
                match plant.Growth |> growthSeries with
                | Absolute g -> g
                | Cumulative g -> g
                | Relative g -> g
            let predictors = plant.Environment |> Map.add (ShortCode.create "x") g
            Bristlecone.fit engine iterations predictors system

        /// Perform n-step-ahead computation on the hypothesis and plant.
        let predictAhead (engine:EstimationEngine.EstimationEngine<float,float>) system (plant:PlantIndividual) preTransform estimate =
            let g =
                match plant.Growth |> growthSeries with
                | Absolute g -> g
                | Cumulative g -> g
                | Relative g -> g
            let predictors = plant.Environment |> Map.add (ShortCode.create "x") g
            Bristlecone.oneStepAhead engine system preTransform predictors estimate


// module Test =

//     type StartStrategy =
//         | CommonTime
//         | FirstGrowthYear
//         | 

//     let startValues (startDate:System.DateTime) (plant:PlantIndividual) =
//         let initialRadius =
//             match plant.Growth with
//             | PlantIndividual.PlantGrowth.RingWidth s -> 
//                 match s with
//                 | GrowthSeries.Absolute c -> c.Head |> fst |> removeUnit
//                 | GrowthSeries.Cumulative c -> 
//                     let start = (c |> TimeSeries.trimStart (startDate - System.TimeSpan.FromDays(366.))).Values |> Seq.head |> removeUnit
//                     printfn "Start cumulative growth = %f" start
//                     start
//                 | GrowthSeries.Relative _ -> invalidOp "Not implemented"
//             | _ -> invalidOp "Not implemented 2"
//         let initialMass = initialRadius |> removeUnit |> ModelComponents.Proxies.toBiomassMM
//         let initialNitrogen = plant.Environment.[ShortCode.create "N"].Head |> fst
//         let initialT = plant.Environment.[code "T[max]"] |> TimeSeries.findExact startDate |> fst
//         [ (ShortCode.create "x", initialRadius)
//           (ShortCode.create "N", initialNitrogen)
//           (ShortCode.create "T[max]", initialT)
//           (ShortCode.create "bs", initialMass) ] |> Map.ofList
