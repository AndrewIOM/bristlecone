namespace Bristlecone.Data

open Bristlecone

module PlantIndividual =

    open FSharp.Data
    open Bristlecone.PlantIndividual

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
                |> Absolute
                |> RingWidth
            { Identifier = code |> ShortCode.create
              Growth = growth
              InternalControls = [] |> Map.ofList
              Environment = [] |> Map.ofList })
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