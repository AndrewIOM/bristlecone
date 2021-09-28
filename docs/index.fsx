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

Bristlecone is a library for conducting model-fitting and model-selection analyses on time-series data. The library was designed for models in ecology / biology.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The Bristlecone library can be <a href="https://nuget.org/packages/Bristlecone">installed from NuGet</a>:
      <pre>$ dotnet add package Bristlecone</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

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
    |> Bristlecone.withTunedMCMC [ ]//Optimisation.MonteCarlo.TuneMethod.CovarianceWithScale 0.750, 200, Optimisation.EndConditions.afterIteration 25000  ]

let testSettings = Bristlecone.Test.TestSettings<float>.Default
Bristlecone.testModel engine testSettings hypothesis

(**
In the above snippet, a von Bertalanffy growth model is defined as a hypothesis to test. We then create an `EstimationEngine`, which defines the methodology for model-fitting. In Bristlecone, an `EstimationEngine` is created and customised using the F# forward pipe operator (for R users this may be familiar; this concept was adapted into the dplyr %>% operator). The call to `testModel` generates random test data, and assesses whether the model-fitting method can accurately estimate known parameters.

Samples & documentation
-----------------------

The API reference is automatically generated from Markdown comments in the library implementation.

 * [Tutorial](tutorial.html) covers the basic components and workflow of Bristlecone.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
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
