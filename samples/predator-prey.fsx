#load "dendrofit.fsx"

////////////////////////////////////////////////////
/// Test
////////////////////////////////////////////////////

(* Testing out the various optimisation functions. *)

open DendroFit
open Types.ParameterEstimation
open Types
open Time

// 0. Configure Options
// ----------------------------

module Options =
    let resolution = Annual
    let iterations = 10000
    let testSeriesLength = 50
    let bootstrapTimes = 20


// 1. Model System
// ----------------------------

let ``predator-prey`` =

    let dxdt' x y alpha beta =
        alpha * x - beta * x * y

    let dydt' x y delta gamma =
        delta * x * y - gamma * y

    let dxdt p _ x (e:Environment) =
        dxdt' x (e.[ShortCode.create "y"]) (p |> Pool.getEstimate "alpha") (p |> Pool.getEstimate "beta")
    
    let dydt p _ y (e:Environment) =
        dydt' (e.[ShortCode.create "x"]) y (p |> Pool.getEstimate "delta") (p |> Pool.getEstimate "gamma")

    { Equations =  [ ShortCode.create "x", dxdt
                     ShortCode.create "y", dydt ] |> Map.ofList
      Parameters = [ ShortCode.create "alpha",  Parameter.create Unconstrained 0.01 2.00
                     ShortCode.create "beta",   Parameter.create Unconstrained 0.01 2.00
                     ShortCode.create "delta",  Parameter.create Unconstrained 0.01 2.00
                     ShortCode.create "gamma",  Parameter.create Unconstrained 0.01 2.00 ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquares [ "x"; "y" ] }


// 2. Test Hypotheses Work
// ----------------------------

let startValues = [ ShortCode.create "x", 20.0; ShortCode.create "y", 4.64 ] |> Map.ofList

``predator-prey``
|> DendroFit.test' Options.resolution Options.iterations Options.testSeriesLength startValues

// TODO
// ====================
//
// A. Test returns NaN. Is this a divide by zero error?
