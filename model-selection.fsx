(**
// can't yet format YamlFrontmatter (["title: Model Selection"; "category: Techniques"; "categoryindex: 1"; "index: 3"], Some { StartLine = 2 StartColumn = 0 EndLine = 6 EndColumn = 8 }) to pynb markdown

*)
#r "nuget: Bristlecone,{{package-version}}"
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

