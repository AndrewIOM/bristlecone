namespace Bristlecone.Data

open Bristlecone
open Bristlecone.Time
open Bristlecone.Dendro

module PlantIndividual =

    open FSharp.Data

    type RingWidthData = CsvProvider<"data-types/ring-width.csv">
    type EnvironmentVariableData = CsvProvider<"data-types/env-variable.csv">

    let loadRingWidths (fileName:string) =
        let data = RingWidthData.Load fileName
        data.Rows 
        |> Seq.groupBy(fun row -> row.``Plant Code``)
        |> Seq.map (fun (code,rows) ->
            let growth = 
                rows
                |> Seq.sortBy (fun i -> i.Date)
                |> Seq.map (fun i -> (float i.``Increment (mm)`` * 1.<mm>, i.Date))
                |> TimeSeries.fromObservations
                |> GrowthSeries.Absolute
                |> PlantIndividual.RingWidth
            { Identifier = code |> ShortCode.create |> Option.get
              Growth = growth
              InternalControls = [] |> Map.ofList
              Environment = [] |> Map.ofList } : PlantIndividual.PlantIndividual)
        |> Seq.toList

    let loadLocalEnvironmentVariable (fileName: string) =
        let data = EnvironmentVariableData.Load fileName
        data.Rows
        |> Seq.groupBy(fun row -> row.``Plant Code``)
        |> Seq.map (fun (code,rows) ->
            code, rows 
              |> Seq.map (fun i -> float i.Predictor, i.Date)
              |> TimeSeries.fromObservations )
        |> Seq.toList