(**
---
title: Index
category: Getting Started
categoryindex: 1
index: 1
---

[![Script]({{root}}/img/badge-script.svg)]({{root}}/{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}/img/badge-notebook.svg)]({{root}}/{{fsdocs-source-basename}}.ipynb)
*)

(*** condition: prepare ***)
#nowarn "211"
#r "nuget: DiffSharp-cpu, v=1.0.7"
#r "nuget: MathNet.Numerics.FSharp,5.0.0"
#r "nuget: FSharp.Data,6.6"
#r "../src/Bristlecone/bin/Debug/net10.0/Bristlecone.dll"

(**
Bristlecone
======================

Bristlecone is a library for easily *composing* theoretical models into larger systems, and
subsequently conducting model-fitting and model-selection analyses on time-series data.
Although originally designed for the investigation of non-linear dynamics within ecological 
and environmental sciences, the library can be used across finance, econometrics and 
other fields that apply non-linear modelling techniques.

## Quick Start

Bristlecone is an F# .NET library. You can easily get started by installing 
[the latest .NET SDK](https://dot.net). You may then simply use the
Bristlecone package in a script, application, or library. 
The nuget package [is available here](https://nuget.org/packages/Bristlecone).

### To use in an F# script

![img/example.png](img/example.png)

## Example

This example demonstrates the layout of a model when defined in Bristlecone.

*)
open Bristlecone // Opens Bristlecone core library and estimation engine
open Bristlecone.Language // Open the language for writing Bristlecone models
open Bristlecone.Time // Opens time units and time manipulation

[<Measure>] type millimetre
[<Measure>] type celsius

let hypothesis =

    let radius = state<millimetre> "radius"
    let T = state<celsius> "temperature"

    // Parameters with units
    let r_max = parameter "η" NoConstraints 0.01<millimetre/day> 1.0<millimetre/day>
    let T_opt = parameter "η" NoConstraints 5.0<celsius> 20.0<celsius>
    let σ = parameter "σ_radius" Positive 0.1<millimetre> 0.2<millimetre>

    let ``dR/dt`` =  
      let tempDiff = Environment T - P T_opt
      let tempResponse = Exponential ( - (tempDiff * tempDiff) / Constant 20.0<celsius ^ 2> )
      P r_max * tempResponse

    let NLL = ModelLibrary.NegLogLikelihood.Normal (Require.state radius) σ

    Model.empty
    |> Model.addRateEquation radius ``dR/dt``
    |> Model.estimateParameter r_max
    |> Model.estimateParameter T_opt
    |> Model.estimateParameter σ
    |> Model.useLikelihoodFunction NLL
    |> Model.compile

// Given some data (loaded using Bristlecone functions, FSharp.Data, etc.)...
fun data ->
  let engine = Bristlecone.mkContinuous () |> Bristlecone.forDailyModel
  let endCond = Optimisation.EndConditions.atIteration 10000<iteration>
  Bristlecone.fit engine endCond data hypothesis

(**
In this example, we define a simple temperature‑driven growth model using Bristlecone’s
modelling language. The state variable `radius` represents stem radius in millimetres,
and the environmental driver `T` represents temperature in degrees Celsius. All quantities
carry F# units of measure, and Bristlecone ensures that every expression in the
model is dimensionally consistent.

The growth rate is defined as a Gaussian response to temperature. Because `radius` has
units of millimetres and time is measured in days, the rate equation must produce a
value in mm/day. This is enforced by the compiler, and cannot be run without being
correct.

To complete the model system, we add a likelihood function for observed stem radius
that assumes Gaussian observation error (with standard deviation `σ_radius`).

To confront the model with data, we create an `EstimationEngine`. An engine is a formal description
of the method that will be used for model-fitting. Engines are built using the F# forward‑pipe (`|>`)
operator, allowing composition of numerical integration methods, time‑conversion rules,
conditioning behaviour, and optimisation strategies in a readable, declarative style.

To apply the model to data, the `Bristlecone.fit` function can be called with an end condition
for the optimisation routine chosen in the given engine.

This pattern - define a model, setup an engine, and call fit - is the core
workflow in Bristlecone, whether you are fitting simple growth curves or
complex multi‑state ecological models.

Samples & documentation
-----------------------

An API reference is automatically generated from XML comments in the library implementation.

In addition, this documentation includes step-by-step example analyses. Each analysis may be downloaded
as an F# script or Jupyter notebook using the buttons at the top of each example page.

 * For basic functions, a comparison of [plant growth models](examples/plant-growth.html) and a classic [predator-prey example](examples/predator-prey.html)
   cover model-fitting with Bristlecone.

 * For dendroecology, a [shrub-resource example](examples/shrub-resource.html) demonstrates
   model-fitting and model-selection (MFMS) with Bristlecone.

 * For palaeoecology, a [sediment core example](examples/sedimentary.html) shows similar methods
   applied to sedimentary isotope and pollen time-series with uneven time-steps.

 * A real research project using Bristlecone [is viewable here](https://github.com/AndrewIOM/shrub-nutrient-modelling/tree/master/src/regional).
   The research concerns the drivers of Arctic shrubification.

 * The [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests.

The library is available under an MIT license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [gh]: https://github.com/AndrewIOM/Bristlecone
  [issues]: https://github.com/AndrewIOM/Bristlecone/issues
  [license]: https://github.com/AndrewIOM/Bristlecone/blob/master/LICENSE
*)
