(*** condition: prepare ***)
#r "nuget: DiffSharp-cpu, v=1.0.7"
#r "nuget: MathNet.Numerics.FSharp,5.0.0"
#r "nuget: FSharp.Data,6.6"
#r "../../src/Bristlecone/bin/Debug/net10.0/Bristlecone.dll"
#r "../../src/Bristlecone.Dendro/bin/Debug/net10.0/Bristlecone.Dendro.dll"
#r "nuget: Plotly.NET, 4.2.0"

open Bristlecone
open Bristlecone.Language
open Bristlecone.Time

[<Measure>] type ton // imperial ton
[<Measure>] type hour

(**
Example model from Chapter 10 of 'The Ecological Detective'.
Comparison of models for Namibian hake.
*)

/// The Schaefer model of fishing 
module Schaefer =

    let B = state<ton> "biomass"
    let I = measure<1> "CPUE" // Catch per unit effort
    let C = environment<ton> "catch"

    let r = parameter "r" NoConstraints 0.01</year> 1.0</year> // Intrinsic growth rate
    let K = parameter "K" NoConstraints 0.01<ton> 1.0<ton> // Carrying capacity
    let q = parameter "q" NoConstraints 1e-6<1/ton> 1e-2<1/ton> // Catchability coefficient
    let B0 = P K // Biomass at time zero. Assumes stock unfished at start time

    let dt = Constant 1.<year>
    let ``B_est[t+1]`` =
        State B + P r * State B * (Constant 1. - State B / P K) * dt - Environment C
    let It = P q * State B

    let sigma = parameter "sigma_v" NoConstraints 0.01 1.0
    let NLL = ModelLibrary.NegLogLikelihood.LogNormal (Require.measure I) sigma

    let model =
        Model.discrete<year>
        |> Model.addDiscreteEquation B ``B_est[t+1]``
        |> Model.addMeasure I It
        |> Model.initialiseHiddenStateWith B B0
        |> Model.useLikelihoodFunction NLL
        |> Model.compile


let cpueData, catchData =
    [
        1965, 1.78, 94
        1966, 1.31, 212
        1967, 0.91, 195
        1968, 0.96, 383
        1969, 0.88, 320
        1970, 0.90, 402
        1971, 0.87, 366
        1972, 0.72, 606
        1973, 0.57, 378
        1974, 0.45, 319
        1975, 0.42, 309
        1976, 0.42, 389
        1977, 0.49, 277
        1978, 0.43, 254
        1979, 0.40, 170
        1980, 0.45, 97
        1981, 0.55, 91
        1982, 0.53, 177
        1983, 0.58, 216
        1984, 0.64, 229
        1985, 0.66, 211
        1986, 0.65, 231
        1987, 0.63, 223
    ]
    |> List.map(fun (y, cpue, catch) ->
        let time = DatingMethods.Annual (y * 1<year>)
        (cpue, time), (float catch, time))
    |> List.unzip

let ts = 
    [
        Schaefer.I.Code, cpueData |> TimeSeries.fromObservations DateMode.annualDateMode
        Schaefer.C.Code, catchData |> TimeSeries.fromObservations DateMode.annualDateMode
    ] |> Map.ofList

let engine =
    Bristlecone.mkDiscrete ()
    |> Bristlecone.withTimeConversion DateMode.Conversion.Annual.toYears

(*** do-not-eval ***)
let r =
    Bristlecone.fit engine (Optimisation.EndConditions.atIteration 1000<iteration>) ts Schaefer.model

