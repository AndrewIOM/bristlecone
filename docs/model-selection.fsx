(**
---
title: Model Selection
category: Techniques
categoryindex: 1
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

    let weights = results |> ModelSelection.Akaike.akaikeWeights

    let resultsDirectory = "some/results/directory/"

    // Save the weights into a csv file
    weights |> Bristlecone.Data.ModelSelection.save resultsDirectory
