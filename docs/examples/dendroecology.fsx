(**
---
title: Dendroecology: investigating plant-environment interactions
category: Examples
categoryindex: 2
index: 1
---
*)

(*** condition: prepare ***)
#nowarn "211"
#r "../../src/Bristlecone/bin/Debug/netstandard2.0/Bristlecone.dll"
#r "nuget: MathNet.Numerics.FSharp,5.0.0"
(*** condition: fsx ***)
#if FSX
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // FSX
(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // IPYNB

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

(**
### Step 1. Defining the ecological model

Here, we 

*)


// 1. Model System
// ----------------------------

let hypothesis =

    /// The universal gas constant in J mol−1 K−1
    let gasConstant = Constant 8.314

    /// An Arrhenius function to represent temperature limitation on growth.
    /// Form of equation: https://pubag.nal.usda.gov/download/13565/PDF
    let temperatureLimitation =
        Constant System.Math.E
        ** ((Constant 1000. * Parameter "Ea" * (Environment "temperature" - Constant 298.))
            / (Constant 298. * gasConstant * Environment "temperature"))

    /// Plant growth is a function of net photosynthesis minus environmental losses.
    /// Photosynthesis is limited by light and temperature.
    let ``db/dt`` = This * Parameter "r" * temperatureLimitation - Parameter "γB" * This

    Model.empty
    |> Model.addEquation "stem radius" ``db/dt``
    |> Model.estimateParameter "Ea" noConstraints 40.0 50.0
    |> Model.estimateParameter "γB" noConstraints 0.01 0.40
    |> Model.estimateParameter "r" notNegative 0.01 1.00
    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ "stem radius" ])
    |> Model.compile


// 2. Setup Bristlecone Engine
// ----------------------------
// A bristlecone engine provides a fixed setup for estimating parameters from data.
// Use the same engine for all model fits within a single study.

let engine =
    Bristlecone.mkContinuous
    |> Bristlecone.withContinuousTime Integration.MathNet.integrate
    |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint
    |> Bristlecone.withTunedMCMC
        [ Optimisation.MonteCarlo.TuneMethod.CovarianceWithScale 0.200,
          250,
          Optimisation.EndConditions.afterIteration 20000 ]


// 3. Test Engine and Model
// ----------------------------
// Running a full test is strongly recommended. The test will demonstrate if the current
// configuration can find known parameters for a model. If this step fails, there is an
// issue with either your model, or the Bristlecone configuration.

// 0. Configure Options
// ----------------------------

module Settings =

    /// Bristlecone can use latitude and longitude to compute day length.
    let latitude, longitude = 63.2, 15.2
    let endWhen = Optimisation.EndConditions.afterIteration 1


// let startValues =
//     [ ShortCode.create "lynx", 30.09; ShortCode.create "hare", 19.58 ] |> Map.ofList

// // TODO Test settings new format

// hypothesis
// |> Bristlecone.testModel engine Options.testSeriesLength startValues Options.iterations []


// // 4. Load Real Data
// // ----------------------------
// // Here, we are using the FSharp.Data type provider to read in .csv datasets.
// // We prepare the monthly temperature dataset for the analysis by:
// // a) Loading the daily dataset using the FSharp.Data library;
// // b) Replacing NaN values with the F# 'None' option type;
// // c) Converting the dataset into a Bristlecone time series type;
// // d) Interpolate the 'None' values by linear interpolation to fill in missing measurements;
// // e) Generalise the time series from daily to monthly by applying an average function.

// open FSharp.Data

// // TODO Is there a way to streamline this?
// [<Literal>]
// let DailyTemperatureUrl = __SOURCE_DIRECTORY__ + "/data/mean-temperature-daily.csv"

// let meanTemperatureMonthly =
//     let maxTemperatures = CsvProvider<DailyTemperatureUrl>.Load DailyTemperatureUrl

//     maxTemperatures.Rows
//     |> Seq.map (fun r ->
//         ((if System.Double.IsNaN r.``T[avg]`` then
//               None
//           else
//               Some(r.``T[avg]`` + 273.15)),
//          r.Date))
//     |> TimeSeries.fromObservations
//     |> TimeSeries.interpolate
//     |> TimeSeries.generalise (FixedTemporalResolution.Months(PositiveInt.create 1)) (fun x -> x |> Seq.averageBy fst)


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

// // 4. Fit Model to Real Data
// // -----------------------------------
// let result =
//     hypothesis
//     |> Bristlecone.fit engine (Optimisation.EndConditions.afterIteration Options.iterations) data
