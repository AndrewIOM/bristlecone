namespace Bristlecone.Data

open Bristlecone
open Bristlecone.Time
open Bristlecone.Dendro

/// <summary>Access the NCEI Palaeo Data Service</summary>
module PalaeoDataService =

    open FSharp.Data

    let internal baseUrl =
        "https://www.ncei.noaa.gov/access/paleo-search/study/search.xml"

    type NCEISearch = XmlProvider<"data-types/ncei-example.xml">

    type PalaeoDataType =
        | Borehole = 1
        | ``Climate forcing`` = 2
        | ``Climate reconstructions`` = 3
        | ``Corals and sclerosponges`` = 4
        | Fauna = 5
        | ``Fire history`` = 12
        | Historical = 6
        | ``Ice cores`` = 7
        | Insect = 8
        | Instrumental = 20
        | ``Lake levels`` = 9
        | ``Loess and palaeosol`` = 10
        | ``Other collections`` = 19
        | ``Palaeoclimatic modelling`` = 11
        | Palaeolimnology = 13
        | ``Plant macrofossils`` = 15
        | Pollen = 16
        | Respository = 60
        | Software = 59
        | Speleothems = 17
        | ``Tree ring`` = 18

    type Continent =
        | Africa
        | Antarctica
        | Asia
        | Australia
        | Europe
        | ``North America``
        | ``South America``

    type GeographicRegion =
        | Arctic
        | Global
        | ``Global Ocean``
        | ``Northern Hemisphere``
        | ``Southern Hemisphere``
        | ``Tropics``
        | ``Western Hemisphere``

    type Location =
        | Continent of Continent //* (Region option) * (Country option)
        | Ocean
        | ``Geographic Region`` of GeographicRegion

    let search (dataType: PalaeoDataType) (treeSpecies: string option) (location: Location option) =
        let url: string = sprintf "%s?dataType=%i" baseUrl (int dataType)
        let result = NCEISearch.Load url

        result.Studies.[0]










module PlantIndividual =

    open FSharp.Data

    type RingWidthData = CsvProvider<"data-types/ring-width.csv">
    type EnvironmentVariableData = CsvProvider<"data-types/env-variable.csv">

    let loadRingWidths (fileName: string) =
        let data = RingWidthData.Load fileName

        data.Rows
        |> Seq.groupBy (fun row -> row.``Plant Code``)
        |> Seq.map (fun (code, rows) ->
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
              Environment = [] |> Map.ofList }
            : PlantIndividual.PlantIndividual)
        |> Seq.toList

    let loadLocalEnvironmentVariable (fileName: string) =
        let data = EnvironmentVariableData.Load fileName

        data.Rows
        |> Seq.groupBy (fun row -> row.``Plant Code``)
        |> Seq.map (fun (code, rows) ->
            code,
            rows
            |> Seq.map (fun i -> float i.Predictor, i.Date)
            |> TimeSeries.fromObservations)
        |> Seq.toList
