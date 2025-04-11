#r "/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Data.PalaeoProvider.Runtime/bin/Debug/netstandard2.1/Bristlecone.dll"
#r "/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Data.PalaeoProvider.Runtime/bin/Debug/netstandard2.1/Bristlecone.Data.PalaeoProvider.Runtime.dll"

open Bristlecone
open Bristlecone.Data.Palaeo

type X =
    NoaaProvider<"/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Dendro/data-types/noaa-template-4.txt">

let y = X.Parse "/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Dendro/data-types/noaa-template-4.txt"

y.Metadata