(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#I "../../bin/Bristlecone/net47"

(**
Tutorial
========================

* [Quick Start: Predator-Prey Dynamics](#quick-start-predator-prey-dynamics)
    * [Defining the model](#defining-the-model)
    * [The likelihood function](#The-likelihood-function)
    * [Putting it all together](#Putting-it-all-together)
    * [Does my model and method work?](#Does-my-model-and-method-work?)
    * [Fitting to observation data](#Fitting-to-observation-data)
* [Time Modes (+ Integration)](#time-modes)
* [Optimisation](#optimisation)
* Working with Time Series
* Setting a Likelihood Function
* Measurement Variables
* Environmental Forcing

## Quick Start: Predator-Prey Dynamics

Here we use the classic example of snowshoe hare and lynx predator-prey dynamics, to demonstrate the basic functions of Bristlecone. The full example is in ``/samples/1. lynx-hare.fsx``.

In an F# script or interactive session, first load and open the Bristlecone library.

*)

#load "bristlecone.fsx"
open Bristlecone
open Bristlecone.ModelSystem

(**### Defining the model

A model system is defined as a series of differential equations, parameters, and a likelihood function. The model system is here defined as a pair of ordinary differential equations. 

*)
/// Number of snowshoe hares
let dhdt' hare lynx alpha beta =
    alpha * hare - beta * hare * lynx

/// Number of lynx
let dldt' lynx hare delta gamma =
    - gamma * lynx + delta * hare * lynx


(**The parameters required for the model are defined as a ``CodedMap<Parameter>``. For the predator-prey model, they are defined as:*)

[ // Natural growth rate of hares in absence of predation
  ShortCode.create "alpha",  Parameter.create  Unconstrained 0.10 0.60
  // Death rate per encounter of hares due to predation
  ShortCode.create "beta",   Parameter.create  Unconstrained 0.001 0.0135
  // Efficiency of turning predated hares into lynx
  ShortCode.create "delta",  Parameter.create  Unconstrained 0.001 0.0135 
  // Natural death rate of lynx in the absence of food
  ShortCode.create "gamma",  Parameter.create  Unconstrained 0.10 0.60
  ] |> Map.ofList


(**### The likelihood function

For this example, we are simply using sum of squares as our measure of goodness of fit. Bristlecone includes sum of squares, and some neagtive log likelihood functions within the ``Bristlecone.Likelihood`` module.
*)

ModelLibrary.Likelihood.sumOfSquares ["hare"; "lynx"]

(**### Putting it all together

The above models, parameters, and likelihood function must be tied together in a ``ModelSystem`` type. A complete model system for this problem is defined here:
*)

let ``predator-prey`` =

    let dhdt p _ x (e:Environment) =
        dhdt' x 
            (e.[ShortCode.create "lynx"]) 
            (p |> Pool.getEstimate "alpha") 
            (p |> Pool.getEstimate "beta")
    
    let dldt p _ y (e:Environment) =
        dldt' y 
            (e.[ShortCode.create "hare"]) 
            (p |> Pool.getEstimate "delta") 
            (p |> Pool.getEstimate "gamma")

    { Equations =  [ code "hare", dhdt
                     code "lynx", dldt ] |> Map.ofList
      Measures  =  [] |> Map.ofList
      Parameters = [ code "alpha",  parameter PositiveOnly 0.10 0.60
                     code "beta",   parameter PositiveOnly 0.001 0.0135
                     code "delta",  parameter PositiveOnly 0.001 0.0135
                     code "gamma",  parameter PositiveOnly 0.10 0.60
                   ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquares ["hare"; "lynx"] }
(**

The intermediate functions ``dhdt`` and ``dldt`` connect the raw ODEs to the functional signature required by Bristlecone. This feeds the current parameter estimates and variable values into the model. 

The modelling protocol is defined by an ``EstimationEngine``, which can be reused for any number of analyses. 
*)

let engine = 
    Bristlecone.mkContinuous
    |> Bristlecone.withConditioning RepeatFirstDataPoint


(**### Does my model and method work?

It is recommended that any new model and method combination is tested, to verify that - given a known set of parameters - the same parameters are estimated. Bristlecone can draw many sets of random parameters and test that these can be correctly estimated:
*)

let startValues = 
    [ code "lynx", 30.09
      code "hare", 19.58 ] |> Map.ofList

//``predator-prey`` |> Bristlecone.testModel engine Options.testSeriesLength startValues Options.iterations Options.burn

(**

### Fitting to observation data

In the example script, we load data using FSharp.Data, from a CSV file. You can choose to load data using any method. The raw data is parsed into a Map of TimeSeries using the ``TimeSeries.createVarying`` function.
*)

type PopulationData = FSharp.Data.CsvProvider<"../samples/data/lynx-hare.csv">

let data = 
    let csv = PopulationData.Load "../samples/data/lynx-hare.csv"
    [ code "hare", 
        TimeSeries.fromObservations (csv.Rows |> Seq.map(fun r -> r.Year, float r.Hare))
      code "lynx", 
        TimeSeries.fromObservations (csv.Rows |> Seq.map(fun r -> r.Year, float r.Lynx)) 
    ] |> Map.ofList


(**To get model fitting results:*)

let result = 
    ``predator-prey``
    //|> Bristlecone.fit engine Options.iterations Options.burn data


(**## Time Modes

Bristlecone can run models in either discrete time, or continuous time. When running models in continuous time, an integration function is required:
*)

type TimeMode<'data, 'time> =
| Discrete
| Continuous of Integrate<'data, 'time>

and Integrate<'data,'time> = 'time -> 'time -> 'time -> CodedMap<'data> -> CodedMap<ODE> -> CodedMap<'data[]>

(**
Currently only fixed timesteps are supported, but variable timestep support (e.g. for sediment core data) is planned. 

Two integration functions are included:

| Solver | Function | Description |
| - | - | - |
| Runge-Kutta 4 (MathNet Numerics) | ``Integration.MathNet.integrate`` | A fourth-order Runge Kutta method to provide approximate solutions to ODE systems. |
| Runge-Kutta 547M (Open Solving Library for ODEs - Microsoft Research) | ``Integration.MsftOslo.integrate`` | A method based on classic Runge-Kutta, but with automatic error and step size control. [See the documentation](https://www.microsoft.com/en-us/research/wp-content/uploads/2014/07/osloUserGuide.pdf).|

## Optimisation

Bristlecone supports optimisation functions that have the following type signature:

```fsharp
type Optimise<'data> = int -> int -> Domain -> ('data[] -> 'data) -> ('data * 'data []) list
```

There are two optimsation techniques currently built-in:

| Method | Function | Description |
| - | - | - |
| Amoeba (Nelder-Mead) | ``Optimisation.Amoeba.solve`` | A gradient-descent method. |
| MCMC Random Walk | ``Optimisation.MonteCarlo.randomWalk`` | A method based on classic Runge-Kutta, but with automatic error and step size control. [See the documentation](https://www.microsoft.com/en-us/research/wp-content/uploads/2014/07/osloUserGuide.pdf).|


## Estimation Engines

To use Bristlecone functions requires a configured ``EstimationEngine``. The easiest way is with the helper functions within the ``Bristlecone`` module:

| Function | Type | Description |
| - | - | - |
| ``mkContinuous`` | EstimationEngine | Default continuous engine |
| ``mkDiscrete`` | EstimationEngine | Default discrete model engine |
| ``withContinuousTime`` | t : Integrate<'a,'b> -> engine: EstimationEngine<'a,'b> -> EstimationEngine<'a,'b> | Transforms engine to continuous time mode, using the given integrator function. |
| ``withConditioning`` | c: Conditioning -> engine: EstimationEngine<'a,'b> -> EstimationEngine<'a,'b> | Choose how the start point is chosen when solving the model system. |

**)