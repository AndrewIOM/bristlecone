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

let mass = state<1> "mass"

let hypothesis =

    // Model parameters:
    let η = parameter "η" noConstraints 0.50 1.50
    let β = parameter "β" noConstraints 0.01 1.00 // m
    let κ = parameter "κ" noConstraints 0.01 1.00

    // Likelihood parameters:
    let sigma = parameter "σ[x]" notNegative 0.01 0.10

    let vonBertalanffy = P η * This ** P β - P κ * This

    Model.empty
    |> Model.addRateEquation mass vonBertalanffy
    |> Model.estimateParameter η
    |> Model.estimateParameter β
    |> Model.estimateParameter κ
    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.gaussian (Require.state mass) )
    |> Model.estimateParameter sigma
    |> Model.compile

// Given some data (loaded using Bristlecone functions or others)...
fun data ->
  let engine = Bristlecone.mkContinuous ()
  let endCond = Optimisation.EndConditions.atIteration 10000<iteration>
  Bristlecone.fit engine endCond data hypothesis

(**
In the above snippet, a von Bertalanffy growth model is defined as a model hypothesis.
We then create an `EstimationEngine`, which defines the methodology for model-fitting.
In Bristlecone, an `EstimationEngine` is created and customised using the F# forward pipe
operator (for R users this may be familiar; this concept was adapted into the dplyr %>% operator).

To apply the model to data, the `Bristlecone.fit` function can be called with an end condition
for the optimisation routine chosen in the given engine.

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

 * The [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into documentation (using fsdocs
literal scripting).

The library is available under an MIT license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/AndrewIOM/Bristlecone/tree/master/docs/examples
  [gh]: https://github.com/AndrewIOM/Bristlecone
  [issues]: https://github.com/AndrewIOM/Bristlecone/issues
  [license]: https://github.com/AndrewIOM/Bristlecone/blob/master/LICENSE
*)
