#r "nuget: FSharp.Data"

open FSharp.Data

let baseUrl = "https://www.ncei.noaa.gov/access/paleo-search/study/search.xml"

type NCEISearch =
    XmlProvider<"/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Dendro/data-types/ncei-example.xml">

/// Defines the possible parameter values for some fields, e.g. species available on a per-proxy basis
type NCEIParams = JsonProvider<"https://www.ncei.noaa.gov/access/paleo-search/study/params.json">

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

let withLocation location query =
    match location with
    | None -> query
    | Some l ->
        match l with
        | Continent c -> sprintf "%s&locations=Continent>%A" query c
        | Ocean -> "Ocean"
        | ``Geographic Region`` region -> sprintf "%s&locations=Region>%A" query region

/// Defines available tree ring species identities based on those available in NCEI API.
let availableWoodRingSpecies =
    lazy
        (let data =
            NCEIParams.Load "https://www.ncei.noaa.gov/access/paleo-search/study/params.json"

         data.Species.Noaa.``18``
         |> Array.map (fun s -> s.Split(':').[0], s.Split(':').[1])
         |> Map.ofArray)

let isAvailableSpecies (taxon: string) =
    let lowered = taxon.ToLower()

    availableWoodRingSpecies.Value
    |> Map.filter (fun k v -> k.ToLower().Contains(lowered))
    |> Map.keys

let withTaxon taxon query =
    match taxon with
    | None -> Ok query
    | Some t ->
        let available = availableWoodRingSpecies.Value |> Map.tryFind t

        match available with
        | Some av -> Ok <| sprintf "%s&species=%s" query av
        | None -> Error "The taxon %s is not listed as an available taxon from the ITRDB."

let getWoodRingData (taxon: string option) (location: Location option) =
    sprintf "%s?dataType=%i" baseUrl 18
    |> withLocation location
    |> withTaxon taxon
    |> Result.map NCEISearch.Load


let search (dataType: PalaeoDataType) (treeSpecies: string option) (location: Location option) =
    let url: string =
        sprintf "%s?dataType=%i" baseUrl (int dataType) |> withLocation location

    let result = NCEISearch.Load url

    result.Studies.[0]

let result =
    getWoodRingData (Some "Salix lanata L.") (Some <| Location.Continent Continent.Europe)



module DataSearch =

    let baseUrl = "https://www.ncei.noaa.gov/access/paleo-search/data/search.xml"

    // Data search
    type NCEIDataSearch =
        XmlProvider<
            "/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Dendro/data-types/ncei-data-example.xml"
         >

    let searchItrdb (taxon: string option) (location: Location option) =
        sprintf "%s?preview=true&matchLevel=file&dataType=%i" baseUrl 18
        |> withLocation location
        |> withTaxon taxon
        |> Result.map NCEIDataSearch.Load


let result2 = DataSearch.searchItrdb (Some "Salix lanata L.") None

type TreeRingData =
    | ``Correlation statistics``
    | ``Raw measurements`` // rwl files
    | Chronology // crn files


result2
|> Result.map (fun r ->
    r.Studies.[0].DataFiles
    |> Array.map (fun df ->
        match df.UrlDescription with
        | "Correlation Stats" -> ``Correlation statistics``
        | "Chronology - NOAA Template File" -> Chronology))


let crn = FSharp.Data.Http.Request("")



// Parse data types

let txt =
    System.IO.File.ReadAllLines
        "/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Dendro/data-types/noaa-template-4.txt"

let meta =
    txt
    |> Array.filter (fun s -> System.Text.RegularExpressions.Regex.IsMatch(s, "#[ \t]*([^:]*):[ \t]*(.*)"))
    |> Array.map (fun s ->
        let m = System.Text.RegularExpressions.Regex.Match(s, "#[ \t]*([^:]*):[ \t]*(.*)")
        m.Groups.[1].Value, m.Groups.[2].Value)

let dataRows =
    txt
    |> Array.filter (fun s -> System.Text.RegularExpressions.Regex.IsMatch(s, "^[^#](.*)"))
    |> Array.map (fun row -> row.Split("\t"))

// #r "nuget: Microsoft.data.Analysis"
// open Microsoft.Data.Analysis

// Connect units

type Column =
    { What: string
      Material: string
      Error: string
      Units: string
      Seasonality: string
      DataType: string
      Detail: string
      Method: string
      DataFormat: string
      AdditionalInformation: string }

#r "nuget: Bristlecone"

open Bristlecone
open Bristlecone.Time

let variableDefinitions =
    txt
    |> Array.filter (fun s -> s.StartsWith("##"))
    |> Array.map (fun row ->
        let rowData =
            System.Text.RegularExpressions.Regex.Match(
                row,
                "^## ([^\t]*)\t*([^,]*),([^,]*),([^,]*),([^,]*),([^,]*),([^,]*),([^,]*),([^,]*),([^,]*),([^,]*)$"
            )

        rowData.Groups.[1].Value
        |> ShortCode.create
        |> Option.defaultValue (ShortCode.create "Unknown").Value,
        { What = rowData.Groups.[2].Value
          Material = rowData.Groups.[3].Value
          Error = rowData.Groups.[4].Value
          Units = rowData.Groups.[5].Value
          Seasonality = rowData.Groups.[6].Value
          DataType = rowData.Groups.[7].Value
          Detail = rowData.Groups.[8].Value
          Method = rowData.Groups.[9].Value
          DataFormat = rowData.Groups.[10].Value
          AdditionalInformation = rowData.Groups.[11].Value })

// Data Collection -> Time_Unit: year CE, cal yr BP or radiocarbon year BP

type TimeUnit =
    | CalYrBP
    | RadiocarbonYrBP
    | CommonEra

let isTimeColumn what (units: string) =
    match units with
    | "calendar year before present" -> Some CalYrBP
    | _ -> None

let colLabels =
    dataRows.[0]
    |> Array.map ShortCode.create
    |> Array.map (Option.defaultValue (ShortCode.create "Unknown").Value)

if colLabels.Length <> dataRows.[0].Length then
    failwith "Could not parse columns"

let df =
    colLabels
    |> Array.mapi (fun i col ->
        let code, col = variableDefinitions |> Seq.find (fun (l, _) -> l = col)
        code, (col, dataRows |> Array.skip 1 |> Array.map (fun row -> row.[i])))
    |> Map.ofArray

let timeIndexes =
    df
    |> Map.filter (fun s (col, t) -> isTimeColumn col.What col.Units |> Option.isSome)

let dataIndexes =
    df |> Map.filter (fun k _ -> timeIndexes |> Map.containsKey k |> not)

// Missing_Values : set in data conversion

let timeFrame =
    timeIndexes
    |> Map.map (fun k (col, v) ->

        // TODO Set time basis in date time format in timeframe / timeseries (requires old date support)
        let timeBasis = isTimeColumn col.What col.Units |> Option.get
        let timePoints = v |> Array.map int // TODO is this float or int?
        let times = timePoints |> Array.map (fun year -> System.DateTime(year, 12, 31))

        let timeSeries =
            dataIndexes
            |> Seq.map (fun kv -> kv.Key, Array.zip (snd kv.Value) times |> TimeSeries.fromObservations)
            |> Map.ofSeq

        timeSeries |> TimeFrame.tryCreate

    )

let x = timeFrame.[colLabels.[1]].Value.StartDate


// Timeframe: store inside as strings, but convert on pulling of column to float or int with units.


let x = meta |> Seq.map fst |> Seq.toList



type Link =
    { NOAA_Landing_Page: System.Uri
      Landing_Page_Description: string
      Study_Level_JSON_Metadata: System.Uri
      Study_Level_JSON_Description: string }

type ResourceLinks =
    { Data_Download_Resource: string
      Data_Download_Description: string
      Supplemental_Download_Resource: string
      Supplemental_Description: string
      Related_Online_Resource: string
      Related_Online_Description: string
      Original_Source_URL: string
      Original_Source_Description: string }

type ShortText = ShortText of string
type DOI = DOI of string

type PublicationDetails =
    { Authors: string
      Published_Date_or_Year: string
      Published_Title: string
      Journal_Name: string
      Volume: string
      Edition: string
      Issue: string
      Pages: string
      Report_Number: string
      DOI: string
      Publisher: string
      ISBN: string
      Online_Resource: string
      Other_Reference_Details: string
      Full_Citation: string
      Abstract: string }

type FundingAgency =
    { Funding_Agency_Name: string
      Grant: string }

type SiteInformation =
    { Site_Name: string
      Location: string
      Northernmost_Latitude: string
      Southernmost_Latitude: string
      Easternmost_Longitude: string
      Westernmost_Longitude: string
      Elevation_m: string }

type DataCollection =
    { Collection_Name: string
      Earliest_Year: string
      Most_Recent_Year: string
      Time_Unit: string
      Core_Length_m: string
      Parameter_Keywords: string
      Notes: string }

type PalaeoDataItem =
    {
      // Encoding: string
      Dataset_DOI: DOI
      Data_Type: PalaeoDataType
      Science_Keywords: ShortText list

      ResourceLinks: ResourceLinks

      ContributionDate: System.DateTime
      LastModifiedDate: System.DateTime

      Study_Name: string
      Investigators: string // todo parse author list
      Description: string

      Publication: PublicationDetails option
      FundingAgency: FundingAgency option
      SiteInformation: SiteInformation option
      DataCollection: DataCollection

      Species_Note: string
      Tree_Species_Download_Resource: string
      Tree_Species_Download_Description: string
      Species_Name: string
      Common_Name: string
      Tree_Species_Code: string
      Chronology: string
      PaST_Thesaurus_Download_Resource: string
      PaST_Thesaurus_Download_Description: string
      ``Variables are defined using these elements``: string
      Note: string
      Data: string
      Missing_Values: string }

// Navigate database by properties..
// Browse indexes by types
