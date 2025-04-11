namespace Bristlecone.Data.PalaeoProvider.Runtime

open System
open System.Text.RegularExpressions
open Bristlecone
open Bristlecone.Time
open System.IO

// Core types:

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

/// Metadata for a data column in the template.
type ColumnMetadata =
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

type TimeUnit =
    | CalYrBP
    | RadiocarbonYrBP
    | CommonEra

type ShortText = ShortText of string
type DOI = DOI of string

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
      Note: string
      Data: string
      Missing_Values: string }

and FundingAgency =
    { Funding_Agency_Name: string
      Grant: string }

and SiteInformation =
    { Site_Name: string
      Location: string
      Northernmost_Latitude: string
      Southernmost_Latitude: string
      Easternmost_Longitude: string
      Westernmost_Longitude: string
      Elevation_m: string }

and DataCollection =
    { Collection_Name: string
      Earliest_Year: string
      Most_Recent_Year: string
      Time_Unit: string
      Core_Length_m: string
      Parameter_Keywords: string
      Notes: string }

and PublicationDetails =
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

and Link =
    { NOAA_Landing_Page: System.Uri
      Landing_Page_Description: string
      Study_Level_JSON_Metadata: System.Uri
      Study_Level_JSON_Description: string }

and ResourceLinks =
    { Data_Download_Resource: string
      Data_Download_Description: string
      Supplemental_Download_Resource: string
      Supplemental_Description: string
      Related_Online_Resource: string
      Related_Online_Description: string
      Original_Source_URL: string
      Original_Source_Description: string }

// Put any utilities here
[<AutoOpen>]
module internal Utilities =

    open System.IO
    open System.Text

    let readNOAATemplateFile (reader: TextReader) = reader.ReadToEnd().Split("\n")

    let isTimeColumn what (units: string) =
        match units with
        | "calendar year before present" -> Some CalYrBP
        | _ -> None

    let findMeta name (meta: (string * string) array) =
        meta |> Array.find (fun (n, _) -> n = name) |> snd

    let parseMetadata (meta: (string * string) array) = "Cool metdata"
// {
//     Study_Name = ""
//     Dataset_DOI = DOI ""
//     Data_Type = PalaeoDataType.Borehole
//     Science_Keywords = []
//     ResourceLinks = ""
//     ContributionDate = ""
//     LastModifiedDate = ""
//     Investigators = ""
//     Description = ""
//     Publication = ""
//     FundingAgency = ""
//     SiteInformation = ""
//     DataCollection = ""
//     Species_Note = ""
//     Tree_Species_Download_Resource = ""
//     Tree_Species_Download_Description = ""
//     Species_Name = ""
//     Common_Name = ""
//     Tree_Species_Code = ""
//     Chronology = ""
//     PaST_Thesaurus_Download_Resource = ""
//     PaST_Thesaurus_Download_Description = ""
//     Note = ""
//     Data = ""
//     Missing_Values = ""
// }

// Put any runtime constructs here
type NoaaFile private (metadata, timelines, series, datasets) =

    member _.Timelines = timelines

    member _.Series = series

    member _.Metadata = metadata

    new(readerFunc: Func<IO.TextReader>) =

        let reader = readerFunc.Invoke()
        let txt = Utilities.readNOAATemplateFile reader

        let metaDict =
            txt
            |> Array.filter (fun s -> Regex.IsMatch(s, "#[ \t]*([^:]*):[ \t]*(.*)"))
            |> Array.map (fun s ->
                let m = Regex.Match(s, "#[ \t]*([^:]*):[ \t]*(.*)")
                m.Groups.[1].Value, m.Groups.[2].Value)

        let meta = Utilities.parseMetadata metaDict

        let dataRows =
            txt
            |> Array.filter (fun s -> System.Text.RegularExpressions.Regex.IsMatch(s, "^[^#](.*)"))
            |> Array.map (fun row -> row.Split("\t"))

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

        if dataRows |> Array.isEmpty then
            failwith "There were no data rows in the NOAA file"

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

                code,
                (col,
                 dataRows
                 |> Array.skip 1
                 |> Array.map (fun row ->
                     try
                         row.[i]
                     with e ->
                         failwithf "help! %i" i)))
            |> Map.ofArray

        let timeIndexes =
            df
            |> Map.filter (fun s (col, t) -> isTimeColumn col.What col.Units |> Option.isSome)

        let dataIndexes =
            df |> Map.filter (fun k _ -> timeIndexes |> Map.containsKey k |> not)


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
            |> Map.filter (fun k v -> v.IsSome)
            |> Map.map (fun k v -> v.Value)

        NoaaFile(meta, timeIndexes, dataIndexes, timeFrame)

    /// Parses the specified NOAA template content
    static member Parse(text) =
        let readerFunc = Func<_>(fun () -> new StringReader(text) :> TextReader)
        new NoaaFile(readerFunc)

    /// Read column definitions
    static member ColumnDefinitions(lines: string seq) =
        lines
        |> Seq.takeWhile (fun s -> s.StartsWith("#"))
        |> Seq.filter (fun s -> s.StartsWith("##"))
        |> Seq.map (fun row ->
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
        |> Seq.toList

    /// Loads NOAA template from the specified stream
    static member Load(stream: Stream) =
        let firstTime = ref true

        let readerFunc =
            Func<_>(fun () ->
                if firstTime.Value then
                    firstTime := false
                else
                    stream.Position <- 0L

                new StreamReader(stream) :> TextReader)

        new NoaaFile(readerFunc)


#if !IS_DESIGNTIME
// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly: CompilerServices.TypeProviderAssembly("Bristlecone.Data.PalaeoProvider.DesignTime.dll")>]
do ()
#endif