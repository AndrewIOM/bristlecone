(**
---
title: Integration
category: Components
categoryindex: 4
index: 3
---

[![Script]({{root}}/img/badge-script.svg)]({{root}}/{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}/img/badge-notebook.svg)]({{root}}/{{fsdocs-source-basename}}.ipynb)
*)

(*** condition: prepare ***)
#nowarn "211"
#r "../src/Bristlecone/bin/Debug/net5.0/Bristlecone.dll"
(*** condition: fsx ***)
#if FSX
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // FSX
(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // IPYNB

(**
Integration Methods
========================

Bristlecone supports model systems defined in discrete or continuous time.
For continuous-time model systems - defined as ordinary differential
equation(s) - Bristlecone applies numerical integration to calculate
changes in the modelled properties through time.

### Included integration methods

The following integration functions are included within the `Bristlecone.Integration` namespace:

| Solver | Function | Description |
| - | - | - |
| Runge-Kutta 4 | ``Integration.RungeKutta.rk4`` | A fourth-order Runge Kutta method to provide approximate solutions to ODE systems. |

### Adding a custom integration method

You can add a custom integration routine by wrapping your integration
function to match the `EstimationEngine.Integration.IntegrationRoutine` type signature as follows:

*)

open Bristlecone

let myCustomIntegrator: EstimationEngine.Integration.IntegrationRoutine =
    fun tInitial tEnd tStep initialConditions rhs -> invalidOp "Doesn't actually do anything!"

let engine () =
    Bristlecone.mkContinuous () |> Bristlecone.withContinuousTime myCustomIntegrator

(**

When defining a custom integration routine, the argument rhs is a compiled
internally using `Base.makeCompiledFunctionForIntegration` function from
the `Bristlecone.Integration` namespace. This function is used to 
compile a 'right-hand side' equation that 'bakes in' the environment
(external forcing) data.
*)
