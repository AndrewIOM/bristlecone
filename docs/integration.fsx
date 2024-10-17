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
#r "../src/Bristlecone/bin/Debug/netstandard2.0/Bristlecone.dll"
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

*Note. Currently only fixed timesteps are supported, but variable timestep support 
(e.g. for sediment core data) is planned for a future release.*

### Included integration methods

The following integration functions are included within the `Bristlecone.Integration` namespace:

| Solver | Function | Description |
| - | - | - |
| Runge-Kutta 4 (MathNet Numerics) | ``Integration.MathNet.integrate`` | A fourth-order Runge Kutta method to provide approximate solutions to ODE systems. |
| Runge-Kutta 547M (Open Solving Library for ODEs - Microsoft Research) | ``Integration.MsftOslo.integrate`` | A method based on classic Runge-Kutta, but with automatic error and step size control. [See the documentation](https://www.microsoft.com/en-us/research/wp-content/uploads/2014/07/osloUserGuide.pdf).|


### Adding a custom integration method

You can add a custom integration routine by wrapping your integration
function to match the `EstimationEngine.Integration` type signature as follows:

*)

open Bristlecone

let myCustomIntegrator: EstimationEngine.Integrate<'data, 'time, _, _> =
    fun writeOut tInitial tEnd tStep initialConditions externalEnvironment model ->
        invalidOp "Doesn't actually do anything!"


Bristlecone.mkContinuous |> Bristlecone.withContinuousTime myCustomIntegrator

(**

When defining a custom integration routine, you may wish to use the
`Base.solve` function from the `Bristlecone.Integration` namespace.
This is used to arrange the equations into a form suitable for a 
simple integration routine.
*)
