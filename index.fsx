(**
[![Script](https://acm.im/bristlecone//img/badge-script.svg)](https://acm.im/bristlecone//index.fsx)&emsp;
[![Notebook](https://acm.im/bristlecone//img/badge-notebook.svg)](https://acm.im/bristlecone//index.ipynb)

*)
#r "nuget: Bristlecone,{{package-version}}"
(**
# Bristlecone

Bristlecone is a library for easily **composing** theoretical models into larger systems, and
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
open FSharp.Data.UnitSystems.SI.UnitSymbols

let hypothesis =

    let mass = state "mass"

    let η = parameter "η" noConstraints 0.50 1.50
    let β = parameter "β" noConstraints 0.01 1.00 // m
    let κ = parameter "κ" noConstraints 0.01 1.00

    let vonBertalanffy = P η * This ** P β - P κ * This

    Model.empty
    |> Model.addRateEquation mass vonBertalanffy
    |> Model.estimateParameter η
    |> Model.estimateParameter β
    |> Model.estimateParameter κ
    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ Require.state mass ])
    |> Model.compile

let engine =
    Bristlecone.mkContinuous ()
    |> Bristlecone.withCustomOptimisation (Optimisation.Amoeba.swarm 5 20 Optimisation.Amoeba.Solver.Default)

let testSettings = Bristlecone.Test.defaultSettings
let testResult = Bristlecone.testModel engine testSettings hypothesis(* output: 
*)
(**
In the above snippet, a von Bertalanffy growth model is defined as a hypothesis to test. We then create an `EstimationEngine`, which defines the methodology for model-fitting. In Bristlecone, an `EstimationEngine` is created and customised using the F# forward pipe operator (for R users this may be familiar; this concept was adapted into the dplyr %&gt;% operator). The call to `testModel` generates random test data, and assesses whether the model-fitting method can accurately estimate known parameters.

## Samples &amp; documentation

An API reference is automatically generated from XML comments in the library implementation.

In addition, this documentation includes step-by-step example analyses. Each analysis may be downloaded
as an F# script or Jupyter notebook using the buttons at the top of each example page.

* [The predator-prey example](examples/predator-prey.html) covers basic model-fitting with Bristlecone.
  

* [The shrub-resource example](examples/shrub-resource.html) is a more
comprehensive example that covers model-fitting and model-selection (MFMS) with Bristlecone.
  

* The [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
and functions in the library. This includes additional brief samples on using most of the
functions.
  

## Contributing and copyright

The project is hosted on [GitHub](https://github.com/AndrewIOM/Bristlecone) where you can [report issues](https://github.com/AndrewIOM/Bristlecone/issues), fork
the project and submit pull requests. If you're adding a new public API, please also
consider adding [samples](https://github.com/AndrewIOM/Bristlecone/tree/master/docs/examples) that can be turned into documentation (using fsdocs
literal scripting).

The library is available under an MIT license, which allows modification and
redistribution for both commercial and non-commercial purposes. For more information see the
[License file](https://github.com/AndrewIOM/Bristlecone/blob/master/LICENSE) in the GitHub repository.

*)

