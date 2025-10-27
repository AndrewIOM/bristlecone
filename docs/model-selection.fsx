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
#r "../src/Bristlecone/bin/Debug/net5.0/Bristlecone.dll"
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
by using the functions within the `Bristlecone.Data` namespace.

The `ModelSelection.save` function takes a sequence of tuples of four values: the subject's ID,
the model or hypothesis ID, the estimated result, and the Akaike weight for that result. In the
below example, we do some wrangling to get the weights into the correct format for saving:
*)

open Bristlecone

fun modelResults subjectIds hypothesisIds ->
    modelResults
    |> ModelSelection.Akaike.akaikeWeights
    |> Seq.zip3 subjectIds hypothesisIds
    |> Seq.map (fun (s, h, (a, b)) -> (s, h, a, b))
    |> Bristlecone.Data.ModelSelection.save "/some/dir"

(**
If you are working with `ResultSet`s, calculating and saving the weights is easier, and can
be completed as below:
*)

fun (hypothesisResults: ModelSelection.ResultSet.ResultSet<string, Language.Hypotheses.Hypothesis<'u>, _, _, _> seq) ->
    hypothesisResults
    |> ModelSelection.Akaike.akaikeWeightsForSet (fun h -> h.ReferenceCode)
    |> Bristlecone.Data.ModelSelection.save "/some/dir"
