module DataAccess

open Types
open Time

module Shrub =

    open FSharp.Data
    open Types.PlantIndividual

    type RingWidthData = CsvProvider<"data-types/plant-growth.csv">
    type EnvironmentVariableData = CsvProvider<"data-types/env-variable.csv">

    let loadRingWidths (fileName:string) =
        let data = RingWidthData.Load fileName
        data.Rows 
        |> Seq.groupBy(fun row -> row.``Plant Code``)
        |> Seq.map (fun (code,rows) ->
            let growth = 
                rows
                |> Seq.sortBy (fun i -> i.Date)
                |> Seq.map (fun i -> i.Date, float i.``Increment (mm)`` * 1.<mm>)
                |> TimeSeries.createVarying
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
              |> Seq.map (fun i -> i.Date, float i.Predictor)
              |> TimeSeries.createVarying )
        |> Seq.toList