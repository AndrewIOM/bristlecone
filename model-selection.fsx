(**
[![Script](https://acm.im/bristlecone//img/badge-script.svg)](https://acm.im/bristlecone//model-selection.fsx)&emsp;
[![Notebook](https://acm.im/bristlecone//img/badge-notebook.svg)](https://acm.im/bristlecone//model-selection.ipynb)

*)
#r "nuget: Bristlecone,2.0.0"
(**
# Model Selection

Through **Model Selection**, alternative model hypothesis results are
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
fun (hypothesisResults: ModelSelection.ResultSet.ResultSet<string, Language.Hypotheses.Hypothesis> seq) ->
    hypothesisResults
    |> ModelSelection.Akaike.akaikeWeightsForSet (fun h -> h.ReferenceCode)
    |> Bristlecone.Data.ModelSelection.save "/some/dir"

