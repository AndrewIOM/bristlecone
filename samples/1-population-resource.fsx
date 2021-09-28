#load "bristlecone.fsx"
// #nuget "Bristlecone"

////////////////////////////////////////////////////
/// Snowshoe Hare and Lynx Predator-Prey Dynamics
////////////////////////////////////////////////////

(* Testing out Bristlecone, using the 90-year data set 
   of snowshoe hare and lynx pelts purchased by the 
   Hudson's Bay Company of Canada. Data is in 1000s. *)

open Bristlecone            // Opens Bristlecone core library and estimation engine
open Bristlecone.Language   // Open the language for writing Bristlecone models
open Bristlecone.Time

// 0. Configure Options
// ----------------------------

module Options =
    let iterations = 100000
    let testSeriesLength = 50


// 1. Model System
// ----------------------------

let ``predator-prey`` =

    let ``dh/dt`` = Parameter "α" * This - Parameter "β" * This * Environment "lynx"
    let ``dl/dt`` = - Parameter "γ" * This + Parameter "Δ" * Environment "hare" * This

    Model.empty
    |> Model.addEquation       "hare"   ``dh/dt``
    |> Model.addEquation       "lynx"   ``dl/dt``
    |> Model.estimateParameter "α"      noConstraints 0.10 0.60    // Natural growth rate of hares in absence of predation
    |> Model.estimateParameter "β"      noConstraints 0.10 0.60    // Death rate per encounter of hares due to predation
    |> Model.estimateParameter "Δ"      noConstraints 0.10 0.60    // Efficiency of turning predated hares into lynx
    |> Model.estimateParameter "γ"      noConstraints 0.10 0.60    // Natural death rate of lynx in the absence of food
    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ "hare"; "lynx" ])
    |> Model.compile


// 2. Setup Bristlecone Engine
// ----------------------------
// A bristlecone engine provides a fixed setup for estimating parameters from data.
// Use the same engine for all model fits within a single study.
// This engine uses a gradident descent method (Nelder Mead simplex), and a basic
// Runge-Kutta 4 integration method provided by MathNet Numerics.

let engine = 
    Bristlecone.mkContinuous
    |> Bristlecone.withGradientDescent
    |> Bristlecone.withContinuousTime Integration.MathNet.integrate
    |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint


// 3. Test Engine and Model
// ----------------------------
// Running a full test is strongly recommended. The test will demonstrate if the current
// configuration can find known parameters for a model. If this step fails, there is an
// issue with either your model, or the Bristlecone configuration.

let startValues = [ ShortCode.create "lynx", 30.09; ShortCode.create "hare", 19.58 ] |> Map.ofList

// TODO Test settings new format
//``predator-prey`` |> Bristlecone.testModel engine Options.testSeriesLength startValues Options.iterations []


// 3. Load in Real Data
// ----------------------------
// Here, we are using the FSharp.Data type provider to read in a CSV file.

type PopulationData = FSharp.Data.CsvProvider<"../samples/data/lynx-hare.csv">
let data = 
    let csv = PopulationData.Load "../samples/data/lynx-hare.csv"
    [ ShortCode.create "hare", TimeSeries.fromObservations (csv.Rows |> Seq.map(fun r -> float r.Hare, r.Year))
      ShortCode.create "lynx", TimeSeries.fromObservations (csv.Rows |> Seq.map(fun r -> float r.Lynx, r.Year)) ] |> Map.ofList


// 4. Fit Model to Real Data
// -----------------------------------
let result = ``predator-prey`` |> Bristlecone.fit engine (Optimisation.EndConditions.afterIteration Options.iterations) data
