#r "nuget: FSharp.Data"

open FSharp.Data

let baseUrl = "https://www.ncei.noaa.gov/access/paleo-search/study/search.xml"

type NCEISearch = XmlProvider<"/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Dendro/data-types/ncei-example.xml">

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
let availableWoodRingSpecies = lazy (
    let data = NCEIParams.Load "https://www.ncei.noaa.gov/access/paleo-search/study/params.json"
    data.Species.Noaa.``18``
    |> Array.map(fun s -> s.Split(':').[0], s.Split(':').[1])
    |> Map.ofArray
)

let isAvailableSpecies (taxon:string) =
    let lowered = taxon.ToLower()
    availableWoodRingSpecies.Value
    |> Map.filter(fun k v -> k.ToLower().Contains(lowered))
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


let search (dataType:PalaeoDataType) (treeSpecies:string option) (location:Location option) =
    let url: string =
        sprintf "%s?dataType=%i" baseUrl (int dataType)
        |> withLocation location
    let result = NCEISearch.Load url

    result.Studies.[0]

let result =
    getWoodRingData (Some "Salix lanata L.") (Some <| Location.Continent Continent.Europe)



module DataSearch =

    let baseUrl = "https://www.ncei.noaa.gov/access/paleo-search/data/search.xml"

    // Data search
    type NCEIDataSearch = XmlProvider<"/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Dendro/data-types/ncei-data-example.xml">

    let searchItrdb (taxon: string option) (location: Location option) =
        sprintf "%s?preview=true&matchLevel=file&dataType=%i" baseUrl 18
        |> withLocation location
        |> withTaxon taxon
        |> Result.map NCEIDataSearch.Load


let result2 =
    DataSearch.searchItrdb (Some "Salix lanata L.") None

type TreeRingData =
    | ``Correlation statistics``
    | ``Raw measurements`` // rwl files
    | Chronology // crn files


result2
|> Result.map(fun r ->
    r.Studies.[0].DataFiles |> Array.map(fun df -> df)
    )


let crn = FSharp.Data.Http.Request("")