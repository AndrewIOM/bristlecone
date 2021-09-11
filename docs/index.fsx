(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#I "../../bin/Bristlecone/net47"

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
#r "Bristlecone.dll"
open Bristlecone
open Bristlecone.ModelSystem

let hypothesis =

  let vonBertalanffy' eta beta kappa mass =
      eta * mass ** beta - kappa * mass

  let vonBertalanffy p t x environment =
      vonBertalanffy' 
        (p |> Pool.getEstimate "eta") 
        (p |> Pool.getEstimate "beta") 
        (p |> Pool.getEstimate "kappa") x

  // B. Define model system
  { Equations  = [ code "x", vonBertalanffy ] |> Map.ofList
    Likelihood = ModelLibrary.Likelihood.sumOfSquares ["x"]
    Measures   = [] |> Map.ofList
    Parameters = [ code "eta",    parameter Unconstrained   0.001 1.00
                   code "beta",   parameter Unconstrained   0.001 1.00
                   code "kappa",  parameter Unconstrained   0.001 1.00 ] |> Map.ofList }

let engine =
  Bristlecone.mkContinuous
  |> Bristlecone.withTunedMCMC []
  |> Bristlecone.testModel hypothesis

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
