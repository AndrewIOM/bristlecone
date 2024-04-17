(**
---
title: Index
category: Getting Started
categoryindex: 1
index: 1
---
*)

(*** condition: prepare ***)
#nowarn "211"
#r "../src/Bristlecone/bin/Release/netstandard2.0/Bristlecone.dll"
#r "nuget: MathNet.Numerics.FSharp,5.0.0"
(*** condition: fsx ***)
#if FSX
#r "nuget: Bristlecone,{{package-version}}"
#endif // FSX
(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Bristlecone,{{package-version}}"
#endif // IPYNB

(**
Bristlecone
======================

Bristlecone is a library for conducting model-fitting and model-selection analyses on time-series data.
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
open Bristlecone            // Opens Bristlecone core library and estimation engine
open Bristlecone.Language   // Open the language for writing Bristlecone models

let hypothesis =

    let vonBertalanffy = 
        Parameter "η" * This ** Parameter "β" - Parameter "κ" * This

    Model.empty
    |> Model.addEquation       "mass"   vonBertalanffy
    |> Model.estimateParameter "η"      noConstraints 0.50 1.50 
    |> Model.estimateParameter "β"      noConstraints 0.01 1.00
    |> Model.estimateParameter "κ"      noConstraints 0.01 1.00 
    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ "mass" ])
    |> Model.compile

let engine = 
    Bristlecone.mkContinuous
    |> Bristlecone.withCustomOptimisation (Optimisation.Amoeba.swarm 5 20 Optimisation.Amoeba.Solver.Default)

let testSettings = Bristlecone.Test.TestSettings<float>.Default
let testResult = Bristlecone.testModel engine testSettings hypothesis

(*** include-fsi-output ***)


(**
In the above snippet, a von Bertalanffy growth model is defined as a hypothesis to test. We then create an `EstimationEngine`, which defines the methodology for model-fitting. In Bristlecone, an `EstimationEngine` is created and customised using the F# forward pipe operator (for R users this may be familiar; this concept was adapted into the dplyr %>% operator). The call to `testModel` generates random test data, and assesses whether the model-fitting method can accurately estimate known parameters.

Samples & documentation
-----------------------

An API reference is automatically generated from XML comments in the library implementation.

In addition, this documentation includes step-by-step example analyses. Each analysis may be downloaded
as an F# script or Jupyter notebook using the buttons at the top of each example page.

 * [The predator-prey example](examples/predator-prey.html) covers basic model-fitting with Bristlecone.

 * [The shrub-resource example](example/shrub-resource.html) is a more 
   comprehensive example that covers model-fitting and model-selection (MFMS) with Bristlecone.

 * The [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under an MIT license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/AndrewIOM/Bristlecone/tree/master/docs/content
  [gh]: https://github.com/AndrewIOM/Bristlecone
  [issues]: https://github.com/AndrewIOM/Bristlecone/issues
  [readme]: https://github.com/AndrewIOM/Bristlecone/blob/master/README.md
  [license]: https://github.com/AndrewIOM/Bristlecone/blob/master/LICENSE
*)
