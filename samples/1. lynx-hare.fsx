#load "../src/Bristlecone/bristlecone.fsx"

////////////////////////////////////////////////////
/// Snowshoe Hare and Lynx Predator-Prey Dynamics
////////////////////////////////////////////////////

(* Testing out DendroFit, using the 90-year data set 
   of snowshoe hare and lynx pelts purchased by the 
   Hudson's Bay Company of Canada. Data is in 1000s. *)

open Bristlecone
open Types.ParameterEstimation
open Types
open Time

// 0. Configure Options
// ----------------------------

module Options =
    let resolution = Annual
    let iterations = 100000
    let testSeriesLength = 50


// 1. Model System
// ----------------------------

let ``predator-prey`` =

    /// Number of snowshoe hares
    let dHdt' hare lynx alpha beta =
        alpha * hare - beta * hare * lynx

    /// Number of lynx
    let dLdt' lynx hare delta gamma =
        - gamma * lynx + delta * hare * lynx

    let dHdt p _ x (e:Environment) =
        dHdt' x (e.[ShortCode.create "lynx"]) (p |> Pool.getEstimate "alpha") (p |> Pool.getEstimate "beta")
    
    let dLdt p _ y (e:Environment) =
        dLdt' y (e.[ShortCode.create "hare"]) (p |> Pool.getEstimate "delta") (p |> Pool.getEstimate "gamma")

    { Equations =  [ ShortCode.create "hare", dHdt
                     ShortCode.create "lynx", dLdt ] |> Map.ofList
      Parameters = [ ShortCode.create "alpha",  Parameter.create Unconstrained 0.10 0.60        // Natural growth rate of hares in absence of predation
                     ShortCode.create "beta",   Parameter.create Unconstrained 0.001 0.0135     // Death rate per encounter of hares due to predation
                     ShortCode.create "delta",  Parameter.create Unconstrained 0.001 0.0135     // Efficiency of turning predated hares into lynx
                     ShortCode.create "gamma",  Parameter.create Unconstrained 0.10 0.60        // Natural death rate of lynx in the absence of food
                    //  ShortCode.create "sigmax", Parameter.create Unconstrained -0.5 0.5
                    //  ShortCode.create "sigmay", Parameter.create Unconstrained -0.5 0.5 
                    //  ShortCode.create "rho",    Parameter.create Unconstrained -0.5 0.5 
                   ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquares ["lynx"; "hare"] }


// 2. Test Hypotheses Work
// ----------------------------

let startValues = [ ShortCode.create "lynx", 30.09; ShortCode.create "hare", 19.58 ] |> Map.ofList

let test = ``predator-prey`` |> Bristlecone.test' Options.resolution Options.iterations Options.testSeriesLength startValues


// 3. Load in Real Data
// ----------------------------

type PopulationData = FSharp.Data.CsvProvider<"data/lynx-hare.csv">

let data = 
    let csv = PopulationData.Load "data/lynx-hare.csv"
    [ ShortCode.create "hare", TimeSeries.createVarying (csv.Rows |> Seq.map(fun r -> r.Year, float r.Hare))
      ShortCode.create "lynx", TimeSeries.createVarying (csv.Rows |> Seq.map(fun r -> r.Year, float r.Lynx)) ] |> Map.ofList

let result = Bristlecone.estimate' Options.resolution Options.iterations ``predator-prey`` StartingValues.FirstDataItem data


// 4. Plot with ggplot
// ----------------------------

#load "plotting.fsx"
open RProvider
open RProvider.graphics
open RProvider.ggplot2

// i. Plot likelihood by iteration (for MCMC)
R.plot (namedParams [ "x", box (test.Trace |> fst |> List.rev |> List.skip 1000) ; "type", box "l"; "xlab", box "Iteration"; "ylab", box "-log likelihood" ]) |> ignore

// i. Comparison to Observed Series
R.par  (namedParams [ "mfrow", [2;2]] ) |> ignore
R.plot (namedParams [ "x",box test.Series.[ShortCode.create "hare"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "X (Observed)" ]) |> ignore
R.plot (namedParams [ "x",box test.Series.[ShortCode.create "hare"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "X (Model)" ]) |> ignore
R.plot (namedParams [ "x",box test.Series.[ShortCode.create "lynx"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "Y (Observed)" ]) |> ignore
R.plot (namedParams [ "x",box test.Series.[ShortCode.create "lynx"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "Y (Model)" ]) |> ignore

