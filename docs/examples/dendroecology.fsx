(**
---
title: Investigating plant-environment interactions using wood rings
category: Examples
categoryindex: 3
index: 2
---
*)

(*** condition: prepare ***)
#nowarn "211"
#r "nuget: DiffSharp-cpu, v=1.0.7"
#r "nuget: MathNet.Numerics.FSharp,5.0.0"
#r "nuget: FSharp.Data,6.6"
#r "../../src/Bristlecone/bin/Debug/net10.0/Bristlecone.dll"
#r "../../src/Bristlecone.Dendro/bin/Debug/net10.0/Bristlecone.Dendro.dll"

#r "nuget: Plotly.NET, 4.2.0"


(**
# Growth of tree rings in response to temperature
This Bristlecone example demonstrates model-fitting
using high-resolution monthly temperature data with
annual ring width increments.

First, we must reference Bristlecone and open it:
*)
open Bristlecone // Opens Bristlecone core library and estimation engine
open Bristlecone.Language // Open the language for writing Bristlecone models
open Bristlecone.Time
open FSharp.Data.UnitSystems.SI.UnitSymbols

(**
### Step 1. Defining the ecological model

*)

// 0. Configure Options
// ----------------------------

module Settings =

    /// Bristlecone can use latitude and longitude to compute day length.
    let latitude, longitude = 68.39<Dendro.latitude>, 58.22<Dendro.longitude>
    let timeZone = "Asia/Yekaterinburg"
    let endWhen = Optimisation.EndConditions.atIteration 1000<iteration>


// 1. Define a model hypothesis
// ----------------------------

[<Measure>] type mm

// States and forcings:
let SR      = measure<mm> "stem-radius"
let GR      = state<mm> "stem-growth"
let Tmean   = environment<K> "temperature"
let L       = environment<1> "day-fraction"

let hypothesis =

    /// The universal gas constant in J mol−1 K−1
    let gasConstant = Constant 8.314<(J/mol)/K>

    let Ea = parameter "Ea" noConstraints 40.0<J/mol> 50.0<J/mol>
    let γB = parameter "γB" noConstraints 1e-5</day> 1e-2</day>
    let r  = parameter "r" notNegative 1e-5</day> 1e-2</day>

    /// An Arrhenius function to represent temperature limitation on growth.
    let temperatureLimitation =
        Constant System.Math.E
        ** ((Constant 1000. * P Ea * (Environment Tmean - Constant 298.<K>))
            / (Constant 298.<K> * gasConstant * Environment Tmean))

    let lightLimitation = Environment L

    /// Plant growth is a function of net photosynthesis minus environmental losses.
    /// Photosynthesis is limited by light and temperature.
    let ``db/dt`` = This<mm> * P r * temperatureLimitation * lightLimitation - P γB * This<mm>

    /// Wood output is laid down and cannot decrease. A cold year will
    /// simply make a missing ring. No allometric relations are used here.
    let woodOutput : ModelExpression<mm> =
        let oldCumulativeMass = StateAt (-1<``time index``>, GR)
        let newCumulativeMass = StateAt (0<``time index``>, GR)
        let diff = newCumulativeMass - oldCumulativeMass
        Conditional (diff .> Constant 0.<mm>) diff This

    Model.empty
    |> Model.addRateEquation GR ``db/dt``
    |> Model.addMeasure SR woodOutput
    |> Model.estimateParameter Ea
    |> Model.estimateParameter γB
    |> Model.estimateParameter r
    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ Require.measure SR ])
    |> Model.compile

    
// 2. Setup Bristlecone Engine
// ----------------------------
// A bristlecone engine provides a fixed setup for estimating parameters from data.
// Use the same engine for all model fits within a single study.

let engine: EstimationEngine.EstimationEngine<System.DateTime,System.TimeSpan,day,1> =
    Bristlecone.mkContinuous ()
    |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint
    |> Bristlecone.withCustomOptimisation ( Optimisation.MonteCarlo.Filzbach.filzbach
           { Optimisation.MonteCarlo.Filzbach.FilzbachSettings.Default with BurnLength = Optimisation.EndConditions.atIteration 10000<iteration> })
    |> Bristlecone.withTimeConversion DateMode.Conversion.toDays

(**
As we have daily temperature data and can compute daily light availability, we
have a choice as to whether to run the models at daily or monthly resolution.
The obvious drawback for daily resolution is a far greater computational load.

Bristlecone contains functions in the `TimeSeries` module to lower the resolution
of time-series. The `TimeSeries.generalise` function requires a fixed resolution
to lower the data to (i.e. monthly), and a generalisation function; this may simply
be the monthly mean, but it would be more appropriate to generalise on more meaningful
measures. For example:

* For temperature, growing degree days.
* For light, the total photoperiod within the month.

Here, we will use daily data to drive the models.
In the GHCN weather station data, the variables are defined
in [this document](https://www.ncei.noaa.gov/pub/data/ghcn/daily/readme.txt).
Temperatures are in tenths of degrees C, precipitation in tenths of mm, and
snow depth in mm.
*)

open FSharp.Data

type WeatherStationData = CsvProvider<"data/dendro/hoseda-hard-obs.csv">

let weatherObs = new WeatherStationData()

let tMean = 
    weatherObs.Rows
    |> Seq.filter(fun r -> r.Variable = "TAVG")
    |> Seq.map(fun r -> r.ValueCleaned |> Option.map(fun v -> float v / 10. * 1.<Dendro.Units.celsius> |> Dendro.Units.celsiusToKelvin), r.Date)
    |> TimeSeries.fromNeoObservations
    // Fills in gaps with simple linear interpolation:
    |> TimeSeries.interpolateFloats (Resolution.FixedTemporalResolution.Days <| PositiveInt.create(1<day>).Value)

tMean |> TimeSeries.resolution

(*** hide ***)
open Plotly.NET

(*** hide ***)
Chart.Line(tMean |> TimeSeries.toObservations |> Seq.map(fun (x,y) -> y,x))
|> Chart.withTemplate ChartTemplates.light
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
The temperature data has some gaps. Specifically,
the maximum temperature data is missing during 1942-45.
*)

/// Light limitation effect (linear between 0 and 1).
/// Light is cached in an object to avoid unnecessary computation.
let lightFn = Dendro.Sunrise.DayLengthCache(Settings.latitude, Settings.longitude, Settings.timeZone)

let lightFraction =
    tMean 
    |> TimeSeries.toObservations
    |> Seq.map snd
    |> Seq.map(fun dt -> dt |> lightFn.GetLight |> Dendro.Sunrise.dayFraction, dt )
    |> TimeSeries.fromNeoObservations

(*** hide ***)
Chart.Line(lightFraction |> TimeSeries.toObservations |> Seq.map(fun (x,y) -> y,x))
|> Chart.withTemplate ChartTemplates.light
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
Here, we are investigating willow shrub ring widths from Varandei, Nenets Autonomous Okrug, Russia.
A [chronology was previously built](https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/correlation-stats/russ208.txt)
using COFECHA; the published chronology for 1921 - 2005 was standardised using a 32-year moving spline.

We cannot use the existing chronology directly, as it has been subject to
detrending. Here, we simply begin by appling the model to some representive
individuals. For example, S8N0125A, S8N0113A, and S8N0110A had the greatest
correlation with all segments of the chronology, so we will start with those.
*)


// 4. Load Real Data
// ----------------------------
// Here, we are using the FSharp.Data type provider to read in .csv datasets.
// We prepare the monthly temperature dataset for the analysis by:
// a) Loading the daily dataset using the FSharp.Data library;
// b) Replacing NaN values with the F# 'None' option type;
// c) Converting the dataset into a Bristlecone time series type;
// d) Interpolate the 'None' values by linear interpolation to fill in missing measurements;
// e) Generalise the time series from daily to monthly by applying an average function.

let ringWidths = Data.PlantIndividual.Csv.loadRingWidths (__SOURCE_DIRECTORY__ + "/data/dendro/varandei-rw.csv")
let exampleShrub =
    ringWidths
    |> Seq.find(fun rw -> rw.Identifier.Value = "S8N0125A")
    |> Dendro.PlantIndividual.toSubannual
    |> Dendro.PlantIndividual.zipEnvironment Tmean tMean
    |> Dendro.PlantIndividual.zipEnvironment L lightFraction



// let startValues =
//     [ ShortCode.create "lynx", 30.09; ShortCode.create "hare", 19.58 ] |> Map.ofList

// // TODO Test settings new format

// hypothesis
// |> Bristlecone.testModel engine Options.testSeriesLength startValues Options.iterations []

// // 4. Fit Model to Real Data
// // -----------------------------------
let result =
    let e = engine |> Bristlecone.withConditioning(Conditioning.Custom (Map.ofList [ GR.Code, 0.36; SR.Code, 0.36 ]))
    Bristlecone.fitDendro e Settings.endWhen hypothesis Bristlecone.FittingMethod.CumulativeGrowth SR.Code exampleShrub


// -------------------------------------------
// -------------------------------------------
// -------------------------------------------

// 3. Test Engine and Model
// ----------------------------
// Running a full test is strongly recommended. The test will demonstrate if the current
// configuration can find known parameters for a model. If this step fails, there is an
// issue with either your model, or the Bristlecone configuration.



// module Test =

//     open Bristlecone.Test

//     let settings = TestSettings.Default

//     let testSettings =
//         { Resolution = Years 1
//           TimeSeriesLength = 30
//           StartValues = [ code "b", 5.; code "t", 255. ] |> Map.ofList
//           EndCondition = Settings.endWhen
//           GenerationRules =
//             [ "b" |> GenerationRules.alwaysLessThan 1000000.
//               "b" |> GenerationRules.alwaysMoreThan 0.
//               code "b", (fun data -> (data |> Seq.max) - (data |> Seq.min) > 100.) ]
//           NoiseGeneration = fun p data -> data
//           EnvironmentalData = [ code "t", TemperatureData.monthly ] |> Map.ofList
//           Random = MathNet.Numerics.Random.MersenneTwister()
//           StartDate = System.DateTime(1970, 01, 01)
//           Attempts = 50000 }

//     let run () =
//         hypothesis |> Bristlecone.testModel Settings.engine testSettings


// let testResult = Test.run ()
