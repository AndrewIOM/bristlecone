(**
---
title: Loading and Saving Data
category: Techniques
categoryindex: 2
index: 6
---

[![Script]({{root}}/img/badge-script.svg)]({{root}}/{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}/img/badge-notebook.svg)]({{root}}/{{fsdocs-source-basename}}.ipynb)
*)

(*** condition: prepare ***)
#nowarn "211"
#r "../src/Bristlecone/bin/Debug/net10.0/Bristlecone.dll"
(*** condition: fsx ***)
#if FSX
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // FSX
(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // IPYNB

(**
Loading and Saving Data
========================

The `Bristlecone.Data` namespace includes methods for saving Bristlecone
results and diagnostics, and loading saved results for further analysis
at a later date.

### Individual estimations results

#### The estimation result itself

Saving and loading estimation results is conducted as follows:
*)

open Bristlecone
open Bristlecone.Data
open Bristlecone.Language

let resultsDirectory = "/some/data/dir"
let thinTraceBy = Some 1000 // Only trace every n iterations to save disk space
let subject = "Mosquito population" // The name of the 'subject' of analysis
let modelName = "Logistic growth"
let dateToString = fun (s: Time.DatingMethods.Annual) -> sprintf "%i" s.Value

fun result -> EstimationResult.saveAll dateToString resultsDirectory subject modelName thinTraceBy result

(**
This save function outputs three files: one each for the maximum likelihood estimate,
a trace of the optimisation process, and the estimated vs
observed time series.

#### Other statistics and estimates

Some other Bristlecone functions have functions to load and save their outputs to CSV files.

Save confidence intervals:
*)
Bristlecone.Data.Confidence.save

(**
Save n-step ahead predictions made using `Bristlecone.oneStepAhead` and similar functions:
*)

Bristlecone.Data.NStepAhead.save

(**
### Ensemble-level results

Some processes, for example model-selection, work across many model results. Bristlecone
includes loading and saving functions (to and from CSV files) for many of these procedures.

#### Model-selection

For model selection, you may calculate Akaike weights either from a sequence of `EstimationResult` (for simpler tasks)
or from a sequence of `ResultSet` (for working with many hypotheses):
*)

fun modelResults subjectIds hypothesisIds ->
    modelResults
    |> ModelSelection.Akaike.akaikeWeights
    |> Seq.zip3 subjectIds hypothesisIds
    |> Seq.map (fun (s, h, (a, b)) -> (s, h, a, b))
    |> Bristlecone.Data.ModelSelection.save "/some/dir"

fun (hypothesisResults: ModelSelection.ResultSet.ResultSet<string, Hypotheses.Hypothesis<'u>, _, _, _> seq) ->
    hypothesisResults
    |> ModelSelection.Akaike.akaikeWeightsForSet (fun h -> h.ReferenceCode)
    |> Bristlecone.Data.ModelSelection.save "/some/dir"

(**
#### Convergence of Monte-Carlo based optimisation approaches

You can assess the convergence of multiple 'chains' between MCMC or Monte-Carlo based
optimisation methods using the functions in the `Bristlecone.Diagnostics` namespace, such as
the per-parameter R-hat values based on the optimisation trace.
*)

// Given a set of results on a per-subject and per-hypothesis basis:
fun resultSet ->
    resultSet
    |> Bristlecone.Diagnostics.Convergence.gelmanRubin 10000 "some subject" "some hypothesis"
    |> Option.iter (Bristlecone.Data.Convergence.save "/some/dir")

(**
#### Summarise Root Mean Squared Error for many hypotheses / subjects

Given individual n-step ahead predictions made using `Bristlecone.oneStepAhead`
and similar functions, you can save the resultant RMSE statistic (an indication
of model performance) in a summary table across all hypotheses and subjects:
*)

fun nStepFits -> Bristlecone.Data.NStepAhead.saveAllRMSE "/some/dir" nStepFits
