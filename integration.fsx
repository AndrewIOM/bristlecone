(**
[![Script](https://acm.im/bristlecone//img/badge-script.svg)](https://acm.im/bristlecone//integration.fsx)&emsp;
[![Notebook](https://acm.im/bristlecone//img/badge-notebook.svg)](https://acm.im/bristlecone//integration.ipynb)

*)
#r "nuget: Bristlecone,3.1.0"
(**
# Integration Methods

Bristlecone supports model systems defined in discrete or continuous time.
For continuous-time model systems - defined as ordinary differential
equation(s) - Bristlecone applies numerical integration to calculate
changes in the modelled properties through time.

### Included integration methods

The following integration functions are included within the `Bristlecone.Integration` namespace:

#### Runge-Kutta 4

*)
Bristlecone.Integration.RungeKutta.rk4
(**
A fourth-order Runge Kutta method to provide approximate solutions to ODE systems.

### Adding a custom integration method

You can add a custom integration routine by wrapping your integration
function to match the `EstimationEngine.Integration.IntegrationRoutine` type signature as follows:

*)
open Bristlecone

let myCustomIntegrator: EstimationEngine.Integration.IntegrationRoutine =
    fun tInitial tEnd tStep initialConditions (rhs:EstimationEngine.ParameterisedRHS) ->
        invalidOp "Doesn't actually do anything!"

let engine () =
    Bristlecone.mkContinuous () |> Bristlecone.withContinuousTime myCustomIntegrator
(**
A custom integration routine is parameterised with fixed time-stepping between
`tInitial` and `tEnd`, with `tStep` step size. The 'rhs' function to be integrated
is a function compiled by Bristlecone that takes two arguments: first, a scalar
of the current time **t**; and second a vector of the current state values.

Internally, if variable time-steps are required Bristlecone runs multiple
fixed integration routines between each pair of time-points.

*)

