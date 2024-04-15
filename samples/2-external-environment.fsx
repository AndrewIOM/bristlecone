#load "bristlecone.fsx"

////////////////////////////////////////////////////
/// Growth of tree rings in response to temperature
////////////////////////////////////////////////////

// This Bristlecone example demonstrates model-fitting
// using high-resolution monthly temperature data with
// annual ring width increments.

open Bristlecone            // Opens Bristlecone core library and estimation engine
open Bristlecone.Language   // Open the language for writing Bristlecone models
open Bristlecone.Time

// 0. Configure Options
// ----------------------------

module Settings =

    /// Bristlecone uses latitude and longitude to compute day length.
    let latitude, longitude = 63.2, 15.2
    let endWhen = Optimisation.EndConditions.afterIteration 1


// 1. Model System
// ----------------------------

let hypothesis =

    /// The universal gas constant in J mol−1 K−1
    let gasConstant = Constant 8.314

    /// An Arrhenius function to represent temperature limitation on growth.
    /// Form of equation: https://pubag.nal.usda.gov/download/13565/PDF
    let temperatureLimitation =
        Constant System.Math.E ** (
            (Constant 1000. * Parameter "Ea" * (Environment "temperature" - Constant 298.))
            / (Constant 298. * gasConstant * Environment "temperature"))
    
    /// Plant growth is a function of net photosynthesis minus environmental losses.
    /// Photosynthesis is limited by light and temperature.
    let ``db/dt`` = This * Parameter "r" * temperatureLimitation - Parameter "γB" * This
    
    Model.empty
    |> Model.addEquation        "stem radius"   ``db/dt``
    |> Model.estimateParameter  "Ea"            noConstraints 40.0 50.0
    |> Model.estimateParameter  "γB"            noConstraints 0.01 0.40
    |> Model.estimateParameter  "r"             notNegative 0.01 1.00
    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ "stem radius" ])
    |> Model.compile


// 2. Setup Bristlecone Engine
// ----------------------------
// A bristlecone engine provides a fixed setup for estimating parameters from data.
// Use the same engine for all model fits within a single study.

let engine = 
    Bristlecone.mkContinuous
    |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint
    |> Bristlecone.withTunedMCMC [ Optimisation.MonteCarlo.TuneMethod.CovarianceWithScale 0.200, 250, Optimisation.EndConditions.afterIteration 20000 ]


// 3. Test Engine and Model
// ----------------------------
// Running a full test is strongly recommended. The test will demonstrate if the current
// configuration can find known parameters for a model. If this step fails, there is an
// issue with either your model, or the Bristlecone configuration.

let testSettings =
    Test.create
    |> Test.withTimeSeriesLength 30
    |> Test.addStartValues [ "stem radius", 2.3 ]
    |> Test.addGenerationRules [ 
        Test.GenerationRules.alwaysLessThan 1000. "stem radius"
        Test.GenerationRules.alwaysMoreThan 0. "stem radius"
        Test.GenerationRules.monotonicallyIncreasing "x" ] // There must be at least 10mm of wood production
    |> Test.endWhen (Optimisation.EndConditions.afterIteration 1000)

let testResult = Bristlecone.testModel engine testSettings hypothesis


// 4. Load Real Data
// ----------------------------
// Here, we are using the FSharp.Data type provider to read in .csv datasets.
// We prepare the monthly temperature dataset for the analysis by:
// a) Loading the daily dataset using the FSharp.Data library;
// b) Replacing NaN values with the F# 'None' option type;
// c) Converting the dataset into a Bristlecone time series type;
// d) Interpolate the 'None' values by linear interpolation to fill in missing measurements;
// e) Generalise the time series from daily to monthly by applying an average function.

open FSharp.Data

[<Literal>] 
let DailyTemperatureUrl = __SOURCE_DIRECTORY__ + "/data/mean-temperature-daily.csv"

let meanTemperatureMonthly =
    let maxTemperatures = CsvProvider<DailyTemperatureUrl>.Load DailyTemperatureUrl
    maxTemperatures.Rows
    |> Seq.map(fun r -> ( (if System.Double.IsNaN r.``T[avg]`` then None else Some (r.``T[avg]`` + 273.15)), r.Date))
    |> TimeSeries.fromObservations
    |> TimeSeries.interpolate
    |> TimeSeries.generalise (FixedTemporalResolution.Months (PositiveInt.create 1)) (fun x -> x |> Seq.averageBy fst)

// TODO read in stem radius sample dataset

// 4. Fit Model to Real Data
// -----------------------------------
let result =
    hypothesis 
    |> Bristlecone.fit engine Settings.endWhen data
