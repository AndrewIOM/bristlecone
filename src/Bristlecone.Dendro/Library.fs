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
