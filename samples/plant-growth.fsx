#r "../packages/NETStandard.Library.NETFramework/build/net461/lib/netstandard.dll"
#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
#r "../src/DendroFit/bin/Debug/netstandard2.0/DendroFit.dll"

///////////////////////////////////
/// Shrub Modelling with DendroFit
///////////////////////////////////

(* Some description of the modelling procedure. *)

open System
open DendroFit
open Types.ParameterEstimation
open Types
open Time

// Test 1.
let eq (p:CodedMap<Parameter>) t x environment =
    (p |> ParameterPool.value "r") * x * ( 1. - (x / (p |> ParameterPool.value "k")))

let l (parameters:ParameterPool) (predictions: CodedMap<PredictedSeries>) =
    let predicted = predictions.Item (ShortCode.create "x")
    ModelLibrary.Likelihood.sumOfSquares parameters predicted.Expected.Values predicted.Observed.Values

let p = [ ShortCode.create "r", NotEstimated (0.01,0.5)
          ShortCode.create "k", NotEstimated (1., 100.) ] |> Map.ofList

let system = {
    Equation = eq
    Likelihood = l
    Parameters = p
}

let series = 
    [1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.;  ] 
    |> TimeSeries.create DateTime.Now (TimeSpan.FromDays 1.)
    |> GrowthSeries.Absolute

let result = DendroFit.estimate 100000 system series

let p2 = [ ShortCode.create "r", Estimated 1.
           ShortCode.create "k", Estimated 50. ] |> Map.ofList

eq p2 1. 0.01 []