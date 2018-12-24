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
    let iterations = 10
    let traceGraph = Bristlecone.Logging.RealTimeTrace.TraceGraph(Logging.Device.X11)
    let logger event = 
        let consolePost = Bristlecone.Logging.Console.logger()
        consolePost event; traceGraph.Log event


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
        dhdt' x (e.[ShortCode.create "lynx"]) (p |> Pool.getEstimate "alpha") (p |> Pool.getEstimate "beta")
    
    let dldt p _ y (e:Environment) =
        dldt' y (e.[ShortCode.create "hare"]) (p |> Pool.getEstimate "delta") (p |> Pool.getEstimate "gamma")

    { Equations =  [ ShortCode.create "hare", dhdt
                     ShortCode.create "lynx", dldt ] |> Map.ofList
      Parameters = [ ShortCode.create "alpha",  Parameter.create Unconstrained 0.10 0.60        // Natural growth rate of hares in absence of predation
                     ShortCode.create "beta",   Parameter.create Unconstrained 0.001 0.0135     // Death rate per encounter of hares due to predation
                     ShortCode.create "delta",  Parameter.create Unconstrained 0.001 0.0135     // Efficiency of turning predated hares into lynx
                     ShortCode.create "gamma",  Parameter.create Unconstrained 0.10 0.60        // Natural death rate of lynx in the absence of food
                     ShortCode.create "sigmax", Parameter.create Unconstrained -0.2 0.2
                     ShortCode.create "sigmay", Parameter.create Unconstrained -0.2 0.2 
                     ShortCode.create "rho",    Parameter.create Unconstrained -0.2 0.2 
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
    |> Bristlecone.withContinuousTime Integration.MathNet.integrate
    |> Bristlecone.withOutput Options.logger
    |> Bristlecone.withTunedMCMC [ Optimisation.MonteCarlo.TuneMethod.Scale, 2000, 10000
                                   Optimisation.MonteCarlo.TuneMethod.CovarianceWithScale 0.250, 500, 30000 ]


// 3. Load in Real Data
// ----------------------------
// Here, we are using the FSharp.Data type provider to read in a CSV file.

type PopulationData = FSharp.Data.CsvProvider<"../samples/data/lynx-hare.csv">
let data = 
    let csv = PopulationData.Load "../samples/data/lynx-hare.csv"
    [ ShortCode.create "hare", TimeSeries.createVarying (csv.Rows |> Seq.map(fun r -> r.Year, float r.Hare))
      ShortCode.create "lynx", TimeSeries.createVarying (csv.Rows |> Seq.map(fun r -> r.Year, float r.Lynx)) ] |> Map.ofList


// 4. Fit Model to Real Data
// -----------------------------------
let result = ``predator-prey`` |> Bristlecone.fit engine Options.iterations data
