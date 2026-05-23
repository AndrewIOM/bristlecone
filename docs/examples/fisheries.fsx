(**
---
title: Fisheries - Namibian hake
category: Examples
categoryindex: 3
index: 5
---

[![Script]({{root}}/img/badge-script.svg)]({{root}}/{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}/img/badge-notebook.svg)]({{root}}/{{fsdocs-source-basename}}.ipynb)
*)

(*** condition: prepare ***)
#r "nuget: DiffSharp-cpu, v=1.0.7"
#r "nuget: MathNet.Numerics.FSharp,5.0.0"
#r "nuget: FSharp.Data,6.6"
#r "../../src/Bristlecone/bin/Debug/net10.0/Bristlecone.dll"
#r "nuget: Plotly.NET, 4.2.0"

open Bristlecone
open Bristlecone.Language
open Bristlecone.Time

[<Measure>] type kton // thousand imperial ton
[<Measure>] type hour

(**
Example model from Chapter 10 of 'The Ecological Detective'.
Models for Namibian hake.
*)

/// The Schaefer model of fishing 
module Schaefer =

    let B = state<kton> "biomass"
    let I = measure<1> "CPUE" // Catch per unit effort
    let C = environment<kton> "catch"

    let r = parameter "r" Positive 0.01</year> 1.0</year> // Intrinsic growth rate
    let K = parameter "K" Positive 100.<kton> 2000.<kton> // Carrying capacity
    let q = parameter "q" Positive 1e-6<1/kton> 1e-2<1/kton> // Catchability coefficient
    let B0 = P K // Biomass at time zero. Assumes stock unfished at start time

    let dt = Constant 1.<year>
    let ``B_est[t+1]`` =
        let n = State B + P r * State B * (Constant 1. - State B / P K) * dt - Environment C
        Conditional (n .> Constant 0.<kton>) n (Constant 0.<kton>)

    let It = P q * State B

    let sigma = parameter "sigma_v" NoConstraints 0.01 1.0
    let NLL = ModelLibrary.NegLogLikelihood.LogNormal (Require.measure I) sigma

    let model =
        Model.discrete<year>
        |> Model.addDiscreteEquation B ``B_est[t+1]``
        |> Model.addMeasure I It
        |> Model.initialiseHiddenStateWith B B0
        |> Model.estimateParameter r
        |> Model.estimateParameter K
        |> Model.estimateParameter q
        |> Model.useLikelihoodFunction NLL
        |> Model.compile

(**
Here, we input the data from the table in the book directly
into F# code.

The units are:
* CPUE: tons per standardised trawler hour
* Catch (thousands of tons)
*)

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

(**
We then convert the raw time-series data into Bristlecone time-series
by using the `fromObservations` function with the annual date mode.
*)

let ts = 
    [
        Schaefer.I.Code, cpueData |> TimeSeries.fromObservations DateMode.annualDateMode
        Schaefer.C.Code, catchData |> TimeSeries.fromObservations DateMode.annualDateMode
    ] |> Map.ofList

(**
Next, we create a standard estimation engine with no data conditioning,
and run a model fit.
*)

let engine: EstimationEngine.EstimationEngine<DatingMethods.Annual,int<year>,year,1> =
    Bristlecone.mkDiscrete ()
    |> Bristlecone.withTimeConversion DateMode.Conversion.Annual.toYears
    |> Bristlecone.withConditioning Conditioning.NoConditioning
    |> Bristlecone.withOutput (Logging.Console.logger 10<iteration>)
    |> Bristlecone.withCustomOptimisation (Optimisation.MonteCarlo.``Automatic (Adaptive Diagnostics)``)

(*** do-not-eval ***)
let r =
    Bristlecone.fit engine (Optimisation.EndConditions.atIteration 1000<iteration>) ts Schaefer.model

(**
Assuming observation uncertainty as above, the book gives parameter
estimates of r=0.39, K=2709, q=0.00045, and sigmaV=0.12.

The above fit yielded similar values, with
estimates of r=0.35, K=2915, q=0.00042, and sigmaV=0.08.
*)

(*** do-not-eval ***)
let save =
    Data.EstimationResult.saveAll (fun (d:DatingMethods.Annual) -> d.Value.ToString()) (__SOURCE_DIRECTORY__ + "cached/") "fisheries" "schaefer" (Some 200) r

(**
We can estimate confidence intervals using a profile likelihood method.
First, we load the cached result:
*)

let resultCached =
    Data.EstimationResult.loadAll
        (fun s -> s |> Seq.map(fun s -> fst s, int (snd s) * 1<year> |> DatingMethods.Annual) |> TimeSeries.fromObservations DateMode.annualDateMode)
        (__SOURCE_DIRECTORY__ + "cached/") "fisheries" Schaefer.model "schaefer"
    |> Seq.head

(**
Then, run the profiler:
*)

(*** do-not-eval ***)
let ci =
    Bristlecone.Confidence.ProfileLikelihood.profile
        Bristlecone.fit
        engine
        ts
        Schaefer.model
        10000
        resultCached

(**
The profile likelihood results indicate the following 95% confidence intervals:

* K: 2544 - 3255 (estimate = 2915)
* q: 0.00034 - 0.00053 (estimate = 0.00042)
* r: 0.30 - 0.42 (estimate = 0.35)
* sigmaV: 0.07 - 0.15 (estimate 0.08)
*)
