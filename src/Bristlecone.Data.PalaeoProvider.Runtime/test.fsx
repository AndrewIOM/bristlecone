#r "/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Data.PalaeoProvider.Runtime/bin/Debug/netstandard2.1/Bristlecone.dll"
#r "/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Data.PalaeoProvider.Runtime/bin/Debug/netstandard2.1/Bristlecone.Data.PalaeoProvider.Runtime.dll"


open Bristlecone
open Bristlecone.Data.Palaeo
open System

type X =
    NoaaProvider<"/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Dendro/data-types/noaa-template-4.txt">

let y = X()


// Data -> IndexedBy X

y.Data |> Seq.map(fun r ->
    r.``air temperature``
    )

let x = X()

let y = X.Parse "/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Dendro/data-types/noaa-template-4.txt"

y.Timelines

y

let ts : CodedMap<Time.TimeSeries<float>> =
    [
        ShortCode.create "test" |> Option.get,
            Time.TimeSeries.fromSeq DateTime.Now (Time.Resolution.FixedTemporalResolution.Months (PositiveInt.create 1 |> Option.get)) [1. ; 432.]
    ] |> Map.ofList



// Static:
// - Types only.

// Non-Static:
// - Columns