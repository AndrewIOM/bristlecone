(**
// can't yet format YamlFrontmatter (["title: Loading and Saving Data"; "category: Techniques"; "categoryindex: 2"; "index: 1"], Some { StartLine = 2 StartColumn = 0 EndLine = 6 EndColumn = 8 }) to pynb markdown

*)
#r "nuget: Bristlecone,{{package-version}}"
(**
Loading and Saving Data
========================

The `Bristlecone.Data` namespace includes methods for saving Bristlecone
results and diagnostics, and loading saved results for further analysis
at a later date.

### Estimation Results

Saving and loading estimation results is conducted as follows:
*)
open Bristlecone
open Bristlecone.Data

let resultsDirectory = "/some/data/dir"
let thinTraceBy = 1000 // Only trace every n iterations to save disk space
let subject = "Mosquito population" // The name of the 'subject' of analysis
let modelName = "Logistic growth"

/// Output three files: the maximum likelihood estimate,
/// a trace of the optimisation process, and the estimated vs
/// observed time series.
fun result ->
    EstimationResult.saveAll resultsDirectory subject modelName thinTraceBy result
(**
### Other statistics with save functions

`Bristlecone.Data` includes methods for saving results from the following
statistical functions that Bristlecone performs:
*)
// Save confidence intervals:
Bristlecone.Data.Confidence.save

// Save convergence statistics based on multiple 'chains'
// of analysis on the same subject and model:
Bristlecone.Data.Convergence.save

// Save Akaike weights calculated for model selection:
Bristlecone.Data.ModelSelection.save

