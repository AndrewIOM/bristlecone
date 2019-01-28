namespace Bristlecone.Data

open Bristlecone
open FSharp.Data

type BristleconeResult = CsvProvider<"templates/saved-data.csv">

module Cache =

    type Subject = string
    type ModelSystemId = string
    type AnalysisId = System.Guid
    type Data = {
        Iteration: int
        Parameters: (string * float) list
    }

    type CachingMessage = 
        | Add of Subject * AnalysisId * Data
        | Clear of Subject * AnalysisId

    type BristleconeCache = MailboxProcessor<CachingMessage>


    module Row =

        let fromTuple = fun (a,b,c,d,e,f,g) -> BristleconeResult.Row(a,b,c,d,e,f,g)

        let fromEstimate subject hypothesisId analysisId (result:Bristlecone.ModelSystem.EstimationResult) : seq<BristleconeResult.Row> =
            result.Trace
            |> Seq.rev
            |> Seq.mapi (fun iterationNumber (likelihood,values) ->
                result.Parameters
                |> Map.toList
                |> Seq.mapi(fun i (name,_) ->
                    (subject,
                     hypothesisId,
                     iterationNumber + 1,
                     analysisId,
                     name.Value,
                     likelihood,
                     values.[i]) |> fromTuple ))
            |> Seq.concat

        let fromEstimates subject hypothesisId analysisId (estimates:seq<Bristlecone.ModelSystem.EstimationResult>) : seq<BristleconeResult.Row> =
            estimates |> Seq.collect (fromEstimate subject hypothesisId analysisId)


    let saveTrace directory subject modelId analysisId results =
        let csv = new BristleconeResult (
                    results |> Row.fromEstimates subject modelId analysisId |> Seq.toList )
        let path = System.IO.DirectoryInfo(directory) // TODO handle errors
        csv.Save(sprintf "%sdphil-shrub-output-%s-%i-%s.csv" path.FullName subject modelId (analysisId.ToString()))