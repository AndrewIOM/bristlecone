#load "bristlecone.fsx"

////////////////////////////////////////////////////
/// Snowshoe Hare and Lynx Predator-Prey Dynamics
////////////////////////////////////////////////////

(* Testing out DendroFit, using the 90-year data set 
   of snowshoe hare and lynx pelts purchased by the 
   Hudson's Bay Company of Canada. Data is in 1000s. *)

open Bristlecone
open Bristlecone.ModelSystem

// 0. Configure Options
// ----------------------------

module Options =
    let iterations = 100000
    let testSeriesLength = 50


// 1. Model System
// ----------------------------

let ``predator-prey`` =

    /// Number of snowshoe hares
    let dhdt' hare lynx alpha beta =
        alpha * hare - beta * hare * lynx

    /// Number of lynx
    let dldt' lynx hare delta gamma =
        - gamma * lynx + delta * hare * lynx

    let dhdt p _ x (e:Environment) =
        dhdt' x (lookup e "lynx") (p |> Pool.getEstimate "alpha") (p |> Pool.getEstimate "beta")
    
    let dldt p _ y (e:Environment) =
        dldt' y (lookup e "hare") (p |> Pool.getEstimate "delta") (p |> Pool.getEstimate "gamma")

    { Equations =  [ code "hare",       dhdt
                     code "lynx",       dldt ] |> Map.ofList
      Parameters = [ code "alpha",      parameter Unconstrained 0.10 0.60        // Natural growth rate of hares in absence of predation
                     code "beta",       parameter Unconstrained 0.001 0.0135     // Death rate per encounter of hares due to predation
                     code "delta",      parameter Unconstrained 0.001 0.0135     // Efficiency of turning predated hares into lynx
                     code "gamma",      parameter Unconstrained 0.10 0.60        // Natural death rate of lynx in the absence of food
                     code "sigmax",     parameter Unconstrained -0.2 0.2
                     code "sigmay",     parameter Unconstrained -0.2 0.2 
                     code "rho",        parameter Unconstrained -0.2 0.2 
                   ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquares ["hare"; "lynx"] }


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
    |> Bristlecone.withConditioning RepeatFirstDataPoint


// 3. Test Engine and Model
// ----------------------------
// Running a full test is strongly recommended. The test will demonstrate if the current
// configuration can find known parameters for a model. If this step fails, there is an
// issue with either your model, or the Bristlecone configuration.

let startValues = [ ShortCode.create "lynx", 30.09; ShortCode.create "hare", 19.58 ] |> Map.ofList

``predator-prey`` |> Bristlecone.testModel engine Options.testSeriesLength startValues Options.iterations []


// 3. Load in Real Data
// ----------------------------
// Here, we are using the FSharp.Data type provider to read in a CSV file.

type PopulationData = FSharp.Data.CsvProvider<"data/lynx-hare.csv">
let data = 
    let csv = PopulationData.Load "data/lynx-hare.csv"
    [ ShortCode.create "hare", TimeSeries.createVarying (csv.Rows |> Seq.map(fun r -> r.Year, float r.Hare))
      ShortCode.create "lynx", TimeSeries.createVarying (csv.Rows |> Seq.map(fun r -> r.Year, float r.Lynx)) ] |> Map.ofList


// 4. Fit Model to Real Data
// -----------------------------------
let result = ``predator-prey`` |> Bristlecone.fit engine Options.iterations data

