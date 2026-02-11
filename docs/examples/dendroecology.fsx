(**
---
title: Wood rings: investigating plant-environment interactions
category: Examples
categoryindex: 3
index: 3
---

[![Script]({{root}}/img/badge-script.svg)]({{root}}/{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}/img/badge-notebook.svg)]({{root}}/{{fsdocs-source-basename}}.ipynb)
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
### Step 1. Defining the ecological model (hypothesis)

*)

// 0. Configure Options
// ----------------------------

module Settings =

    /// Bristlecone can use latitude and longitude to compute day length.
    let latitude, longitude = 68.39<Dendro.latitude>, 58.22<Dendro.longitude>
    let timeZone = "Asia/Yekaterinburg"
    let endWhen = Optimisation.EndConditions.atIteration 1000<iteration>
    let logger = Logging.Console.logger 10<iteration>


[<Measure>] type mm

// States and forcings:
let SR      = state<mm> "stem-radius"
let SP      = state<mm> "stem-production"
let Tmean   = environment<K> "temperature"
let L       = environment<1> "day-fraction"

let hypothesis =

    /// The universal gas constant in J mol−1 K−1
    let gasConstant = Constant 8.314<(J/mol)/K>

    let Ea    = parameter "Ea" NoConstraints 30.0<J/mol> 80.0<J/mol>
    let γB    = parameter "γB" NoConstraints 1e-5</day> 1e-1</day>
    let r     = parameter "r" Positive 1e-5</day> 1e-1</day>
    let Rstar = parameter "r*" Positive 2.<mm> 10.<mm>
    let q     = parameter "q"  NoConstraints 0.3<1> 0.9<1>
    let σx    = parameter "σ[x]" Positive 0.05<mm> 0.3<mm>

    /// An Arrhenius function to represent temperature limitation on growth.
    let temperatureLimitation =
        Constant System.Math.E
        ** ((Constant 1000. * P Ea * (Environment Tmean - Constant 298.<K>))
            / (Constant 298.<K> * gasConstant * Environment Tmean))

    let shadingEffect = P q + (Constant 1.<1> - P q) * (State SR / (State SR + P Rstar))
    let lightLimitation = Environment L * shadingEffect

    /// Plant growth is a function of net photosynthesis minus environmental losses.
    /// Photosynthesis is limited by light and temperature.
    let ``db/dt`` = This<mm> * P r * temperatureLimitation * lightLimitation - P γB * This<mm>

    /// Wood output is laid down and cannot decrease. A cold year will
    /// simply make a missing ring. No allometric relations are used here.
    let ``dW/dt`` = Conditional (``db/dt`` .> Constant 0.<mm/day>) (``db/dt``) (Constant 0.<mm/day>)

    Model.empty
    |> Model.addRateEquation SP ``db/dt``
    |> Model.addRateEquation SR ``dW/dt``
    |> Model.estimateParameter Ea
    |> Model.estimateParameter γB
    |> Model.estimateParameter r
    |> Model.estimateParameter Rstar
    |> Model.estimateParameter q
    |> Model.useLikelihoodFunction (ModelLibrary.NegLogLikelihood.Normal (Require.state SR) σx)
    |> Model.compile

(**
### Setup Bristlecone Engine

A bristlecone engine provides a fixed setup for estimating parameters from data.
Use the same engine for all model fits within a single study.

You may start with the `Bristlecone.mkContinuous` or `Bristlecone.mkDiscrete` functions
depending on the model you want to apply (continuous or discrete time). Here, we are using
differential equation models, so we setup a continuous-time engine.

Within this engine below, we configure that the first observation is repeated as a data conditioning
step. We also choose to use a custom optimisation method - Filzbach - which is included in the core
Bristlecone library, with a burn-in length of 1000 iterations. In Filzbach, the burn-in period is that
where tuning of the parameter scales occurs. We then configure the engine to write out to a simple console
logger, which is setup in the Settings at the start of this script.

Finally, we must apply a time conversion so that the engine understands how to convert from the
timespan representation used for the datasets to the temporal resolution the model is defined in.
Here, the data is defined using a contemporary Gregorian calendar (using .NET DateTime / TimeSpan types),
and the model is defined on a daily timescale (i.e. the model's parameters that are rates are per day).
Some built in time conversions (of the type `ResolutionToModelUnits`) are included in the
`DateMode.Conversion` module within the `Bristlecone.Time` module.
*)

let engine: EstimationEngine.EstimationEngine<System.DateTime,System.TimeSpan,day,1> =
    Bristlecone.mkContinuous ()
    |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint
    |> Bristlecone.withBristleconeOptimiser
    |> Bristlecone.withOutput Settings.logger
    |> Bristlecone.withTimeConversion DateMode.Conversion.CalendarDates.toDays

(**
### Load in datasets

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

The temperature data has some gaps, spanning usually one to ten days at a time.
Here, we use the built-in `TimeSeries.interpolateFloats` to fill in the gaps.
*)

open FSharp.Data

type WeatherStationData = CsvProvider<"data/dendro/hoseda-hard-obs.csv">

let weatherObs = new WeatherStationData()

let tMean = 
    weatherObs.Rows
    |> Seq.filter(fun r -> r.Variable = "TAVG")
    |> Seq.map(fun r -> r.ValueCleaned |> Option.map(fun v -> float v / 10. * 1.<Dendro.Units.celsius> |> Dendro.Units.celsiusToKelvin), r.Date)
    |> TimeSeries.fromNeoObservations
    // Fills in gaps with simple linear interpolation between known values:
    |> TimeSeries.interpolateFloats (Resolution.FixedTemporalResolution.Days <| PositiveInt.create(1<day>).Value)

open Plotly.NET

Chart.Line(tMean |> TimeSeries.toObservations |> Seq.map(fun (x,y) -> y,x))
|> Chart.withTemplate ChartTemplates.light
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
#### Seasonal light availability

In the above model, we naively applied a light limiting effect within the model
that a simple 0 - 1 multiplier to growth.

To obtain daily light availability values as an exogeneous environmental input to
the model, the `Bristlecone.Dendro` package contains sunlight calculations we can use.

A `DayLengthCache` provides a wrapper for accessing day length values at a particular
latitude and longitude; a time-zone is also required, which must be a time-zone within
your computer's built in time-zone list, which differs between macos/linux/windows.
*)

let lightFn = Dendro.Sunrise.DayLengthCache(Settings.latitude, Settings.longitude, Settings.timeZone)

let lightFraction =
    tMean 
    |> TimeSeries.toObservations
    |> Seq.map snd // collect the list of daily dates to match the temperature data
    |> Seq.map(fun dt -> dt |> lightFn.GetLight |> Dendro.Sunrise.dayFraction, dt )
    |> TimeSeries.fromNeoObservations

Chart.Line(lightFraction |> TimeSeries.toObservations |> Seq.map(fun (x,y) -> y,x))
|> Chart.withTemplate ChartTemplates.light
|> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
#### Ring width data

Here, we are investigating willow shrub ring widths from Varandei, Nenets Autonomous Okrug, Russia.
A [chronology was previously built](https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/correlation-stats/russ208.txt)
using COFECHA; the published chronology for 1921 - 2005 was standardised using a 32-year moving spline.

We cannot use the existing chronology directly, as it has been subject to
detrending. Here, we simply begin by appling the model to some representive
individuals. For example, S8N0125A, S8N0113A, and S8N0110A had the greatest
correlation with all segments of the chronology, so we will start with those.

To setup the data for Bristlecone, we use functions within `Bristlecone.Dendro` to:

* load all of the ring width data from a CSV file;
* find the specifc shrub for this example (S8N0125A);
* change the date of each ring from a year (e.g. 1980) to a date that - when using a daily-resolution model and environment data - will compare the observation and prediction at the **end of the growing season** (e.g. 1980-12-31);
* attach the temperature and light environment data for analysis.

Bristlecone dendro represents a plant and its environment as a `PlantIndividual` type
as a convenience wrapper.
*)

let exampleShrub =
    Data.PlantIndividual.Csv.loadRingWidths (__SOURCE_DIRECTORY__ + "/data/dendro/varandei-rw.csv")
    |> Seq.find(fun rw -> rw.Identifier.Value = "S8N0125A")
    |> Dendro.PlantIndividual.toSubannual
    |> Dendro.PlantIndividual.zipEnvironment Tmean tMean
    |> Dendro.PlantIndividual.zipEnvironment L lightFraction

(**
### Fit the model to the real data

We use `Bristlecone.fitDendro` as a convenience function to work with `PlantIndividual` types.

One complexity is that the conditioning requires specifying the value for the internal stem
production dynamic equation, which is effectively a hidden state
(TODO Replace with the hidden states initialiser functions on the model system itself).
*)

(*** do-not-eval ***)
let result =
    let e = engine |> Bristlecone.withConditioning(Conditioning.Custom (Map.ofList [ SP.Code, 0.36; SR.Code, 0.36 ]))
    Bristlecone.fitDendro e Settings.endWhen hypothesis Bristlecone.FittingMethod.CumulativeGrowth SR.Code exampleShrub

let outputDir = __SOURCE_DIRECTORY__ + "/cached/"

(*** do-not-eval ***)
Bristlecone.Data.Series.save (fun (d:System.DateTime) -> d.ToShortDateString()) outputDir exampleShrub.Identifier.Value "test-model" result

// Note: Here, we are loading a result calculated earlier.
let cachedResultSeries =
    Bristlecone.Data.Series.load id outputDir exampleShrub.Identifier.Value "test-model"
    |> Seq.toList
    |> Seq.head
    |> snd

(**
The fit versus observed series appear as follows:
*)

cachedResultSeries |> Seq.map(fun v ->
    let fit = v.Value |> Seq.map(fun (x,y) -> System.DateTime.Parse y, x.Fit)
    let obs = v.Value |> Seq.map(fun (x,y) -> System.DateTime.Parse y, x.Obs)
    [ Chart.Line fit; Chart.Line obs ]
    |> Chart.combine
)
|> Chart.Grid(1,2)
|> Chart.withTemplate ChartTemplates.light
|> GenericChart.toChartHTML
(*** include-it-raw ***)