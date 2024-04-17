(**
---
title: Diagnostics
category: Techniques
categoryindex: 2
index: 5
---
*)

(*** condition: prepare ***)
#nowarn "211"
#r "../src/Bristlecone/bin/Release/netstandard2.0/Bristlecone.dll"
(*** condition: fsx ***)
#if FSX
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // FSX
(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // IPYNB

(**
Diagnostics
========================

Diagnostic statistics are essential to ensure that the model-fitting
and model-selection results are valid and complete. 

### Convergence Statistics

For Monte-Carlo based optimisation techniques (such as the random walk
MCMC or the Filzbach-style MCMC) convergence statistics are useful to
assess whether multiple independent *chains* have identified the same
minimum -log likelihood in parameter space, which may be the global minimum.

Bristlecone includes the Gelman-Rubin convergence statistic (Rhat), which
can be calculated on a per-parameter basis. 

*)

open Bristlecone
open Bristlecone.Diagnostics
open Bristlecone.Data

let saveDirectory = "/some/save/dir"

fun listOfResults ->

    // Calculate Gelman-Rubin statistic for all parameters 
    // across multiple model runs
    let stat = Convergence.gelmanRubinAll 1000 listOfResults

    // Save results to a single file
    Convergence.save saveDirectory stat

(**
### Logging individual model components

Sometimes it is helpful for diagnostic or visualisation purposes to
see the values of individual parts of a mathematical model during
computation, or at the minimum -log likelihood.

Bristlecone includes the ability to log out the values of individual
model components through time. An worked example is given below of how to
obtain computed values across the time-series for components within
a model system. First, you must set up a model that has a parameter
of the `IComponentLogger<float>)` type:

[ NB TODO: This function must be refactored to work with the new Bristlecone Language ]
*)

// open Bristlecone
// open Bristlecone.Language
// open Bristlecone.Diagnostics.ModelComponents

// let hypothesis (cLog:IComponentLogger<ModelExpression>) =

//     let ``dN/dt`` = 
//         Parameter "r" * This * cLog.StoreValue "log this thing" (Constant 1. - (This / Parameter "K"))

//     Model.empty
//     |> Model.addEquation       "N"      ``dN/dt``
//     |> Model.estimateParameter "r"      noConstraints 0.10 5.00 
//     |> Model.estimateParameter "K"      noConstraints 50.0 150.
//     |> Model.useLikelihoodFunction      (ModelLibrary.Likelihood.sumOfSquares [ "N" ])
//     |> Model.compile


// fun engine singleResult ->
//     // A wrapper around the Bristlecone.fit function
//     let fitFn ts m e = Bristlecone.fit e (Optimisation.EndConditions.afterIteration 1) ts m
//     let logs = ModelComponents.calculateComponents fitFn engine singleResult
//     ()