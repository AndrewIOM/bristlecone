(**
---
title: Model Selection
category: Techniques
categoryindex: 1
index: 3
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
Model Selection
========================

Through *Model Selection*, alternative model hypothesis results are
competed to determine which hypothesis is best explained by the underlying
data. Before conducting model selection, you should be familiar with the benefits
and limitations of alternative model selection statistics.

### Akaike Weights

To calculate Akaike weights for a set of hypotheses, you must first obtain
your results by either loading in saved result files, or running models directly.
Once you have obtained your results, weights can be saved after calculation
by using the functions within the `Bristlecone.Data` namespace as below:
*)

open Bristlecone

fun results ->

    let resultsDirectory = "some/results/directory/"

    let weights =
        results
        |> ModelSelection.weights

    // Save the weights into a csv file
    weights
    |> Data.ModelSelection.save resultsDirectory
