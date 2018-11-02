#load "../src/bristlecone.fsx"

////////////////////////////////////////////////////
/// Population growth with a single limiting resource
////////////////////////////////////////////////////

(* Replication of Jeffers et al 2011 analysis. *)

open Bristlecone
open Bristlecone.ModelSystem

// 0. Configure Options
// ----------------------------

module Options =
    let resolution = Annual
    let iterations = 100000
    let testSeriesLength = 50


// 1. Model System
// ----------------------------

let ``population with single resource limitation`` =

    let dxdt' x y alpha beta =
        alpha * x - beta * x * y

    let dydt' y x beta delta gamma =
        delta * beta * x * y - gamma * y

    let dxdt p _ x (e:Environment) =
        dxdt' x (e.[ShortCode.create "y"]) (p |> Pool.getEstimate "alpha") (p |> Pool.getEstimate "beta")
    
    let dydt p _ y (e:Environment) =
        dydt' y (e.[ShortCode.create "x"]) (p |> Pool.getEstimate "beta") (p |> Pool.getEstimate "delta") (p |> Pool.getEstimate "gamma")

    { Equations =  [ ShortCode.create "x", dxdt
                     ShortCode.create "y", dydt ] |> Map.ofList
      Parameters = [ ShortCode.create "alpha",  Parameter.create Unconstrained 1.00 1.001
                     ShortCode.create "beta",   Parameter.create Unconstrained 0.20 0.201
                     ShortCode.create "delta",  Parameter.create Unconstrained 0.50 0.501
                     ShortCode.create "gamma",  Parameter.create Unconstrained 0.20 0.201
                     ShortCode.create "sigmax", Parameter.create Unconstrained -0.50 0.50
                     ShortCode.create "sigmay", Parameter.create Unconstrained -0.50 0.50 
                     ShortCode.create "rho",    Parameter.create Unconstrained -0.25 0.25 ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquares [ "x"; "y" ] }


// 2. Test Hypotheses Work
// ----------------------------

let engine =
    Bristlecone.mkContinuous
    |> Bristlecone.withConditioning RepeatFirstDataPoint

let startValues = [ ShortCode.create "x", 1.00; ShortCode.create "y", 2.00 ] |> Map.ofList

let test =
    ``population with single resource limitation``
    |> Bristlecone.testModel engine Options.testSeriesLength startValues Options.iterations []


// 3. Plot test (using R)
// ----------------------------

// #load "plotting.fsx"
// open RProvider
// open RProvider.graphics
// open RProvider.ggplot2

// // i. Plot likelihood by iteration (for MCMC)
// R.plot (namedParams [ "x", box (test.Trace |> fst |> List.rev |> List.skip 10) ; "type", box "l"; "xlab", box "Iteration"; "ylab", box "-log likelihood" ]) |> ignore

// // i. testsus Observed Series
// R.par  (namedParams [ "mfrow", [2;2]] ) |> ignore
// R.plot (namedParams [ "x",box test.Series.[ShortCode.create "x"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "X (Observed)" ]) |> ignore
// R.plot (namedParams [ "x",box test.Series.[ShortCode.create "x"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "X (Model)" ]) |> ignore
// R.plot (namedParams [ "x",box test.Series.[ShortCode.create "y"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "Y (Observed)" ]) |> ignore
// R.plot (namedParams [ "x",box test.Series.[ShortCode.create "y"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "Y (Model)" ]) |> ignore
