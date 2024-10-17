namespace Bristlecone.Data

module PlantIndividual =

    open Bristlecone
    open Bristlecone.Time
    open Bristlecone.Dendro
    open Bristlecone.Dendro.Units

    /// Load plant-specific time-series from CSV files.
    module Csv =

        open FSharp.Data

        type RingWidthData = CsvProvider<"data-types/ring-width.csv">
        type EnvironmentVariableData = CsvProvider<"data-types/env-variable.csv">

        /// <summary>Load ring widths from a CSV file that contains 'Year', 'Plant Code',
        /// and 'Increment (mm)' columns.</summary>
        /// <param name="csvFileName">The file to load</param>
        /// <returns>A plant individual containing the growth series.</returns>
        let loadRingWidths (csvFileName: string) =
            let data = RingWidthData.Load csvFileName

            data.Rows
            |> Seq.groupBy (fun row -> row.``Plant Code``)
            |> Seq.map (fun (code, rows) ->
                let growth =
                    rows
                    |> Seq.sortBy (fun i -> i.Year)
                    |> Seq.map (fun i ->
                        (float i.``Increment (mm)`` * 1.<millimetre / year>, i.Year * 1<year> |> DatingMethods.Annual))
                    |> TimeSeries.fromObservations DateMode.annualDateMode
                    |> GrowthSeries.Absolute
                    |> PlantIndividual.PlantGrowth.RingWidth

                { Identifier = code |> ShortCode.create |> Option.get
                  Growth = growth
                  InternalControls = [] |> Map.ofList
                  Environment = [] |> Map.ofList }
                : PlantIndividual.PlantIndividual<DatingMethods.Annual, int<year>, int<year>>)
            |> Seq.toList

        /// <summary>Load plant-specific environmental time-series from
        /// a CSV file, where all time-series are specified in a single CSV.</summary>
        /// <param name="fileName">The CSV file to load from.</param>
        /// <returns>A map of time-series by their plant individual.</returns>
        let loadPlantSpecificEnvironments (fileName: string) =
            let data = EnvironmentVariableData.Load fileName

            data.Rows
            |> Seq.groupBy (fun row -> row.``Plant Code``)
            |> Seq.map (fun (code, rows) ->
                code,
                rows
                |> Seq.map (fun i -> float i.Predictor, i.Year * 1<year> |> DatingMethods.Annual)
                |> TimeSeries.fromObservations DateMode.annualDateMode)
            |> Seq.toList
