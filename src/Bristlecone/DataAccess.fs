namespace Bristlecone.Data

open Bristlecone
open Bristlecone.ModelSystem
open FSharp.Data

// Bristlecone loads and saves CSV data in these formats:
// 1. [Invididual] MLE: best-fitting parameter steps and their likelihood
// 2. [Individual] Trace: record of optimisation steps
// 3. [Invididual] Series: best-fitting parameter steps and their likelihood
// 4. [Ensemble] Model-selection (weights)

module Config =

    type DataType =
        | MLE
        | Trace
        | Series

    let filePath directory subject modelId (resultId:System.Guid) dataType =
        let path = System.IO.DirectoryInfo(directory)
        if path.Exists then
            let t = 
                match dataType with
                | MLE -> "mle"
                | Trace -> "trace"
                | Series -> "series"
            sprintf "%sbristlecone-%s-%i-%s-%s.csv" path.FullName subject modelId t (resultId.ToString())
        else invalidArg "directory" "The specified directory does not exist"


module Trace =

    type BristleconeTrace = CsvProvider<"templates/individual-trace.csv">

    module Row =

        let fromEstimate subject modelId (result:EstimationResult) : seq<BristleconeTrace.Row> =
            result.Trace
            |> Seq.rev
            |> Seq.mapi (fun iterationNumber (likelihood,values) ->
                result.Parameters
                |> Map.toList
                |> Seq.mapi(fun i (name,_) ->
                    (subject,
                     modelId,
                     iterationNumber + 1,
                     result.ResultId,
                     name.Value,
                     likelihood,
                     values.[i]) |> BristleconeTrace.Row ))
            |> Seq.concat


    let save directory subject modelId result =
        let csv = new BristleconeTrace(result |> Row.fromEstimate subject modelId)
        let filePath = Config.filePath directory subject modelId result.ResultId Config.DataType.Trace
        csv.Save(filePath)

    /// Load a trace from a file
    let load directory subject modelId =
        invalidOp "Not implemented"


module MLE =

    type IndividualMLE = CsvProvider<"templates/individual-mle.csv">

    module Row =

        let fromResult subject hypothesisId (result:EstimationResult) =
            result.Parameters
            |> Map.toList
            |> Seq.map(fun (name,v) ->
                (subject,
                 hypothesisId,
                 result.ResultId,
                 name.Value,
                 result.Likelihood,
                 v |> Parameter.getEstimate) |> IndividualMLE.Row )


    let save directory subject modelId result =
        let csv = new IndividualMLE (result |> Row.fromResult subject modelId)
        let filePath = Config.filePath directory subject modelId result.ResultId Config.DataType.MLE
        csv.Save(filePath)

    let load directory subject modelId =
        invalidOp "Not finished"


module Series =

    type IndividualSeries = CsvProvider<"templates/individual-series.csv">

    module Row =

        let fromResult subject hypothesisId (result:EstimationResult) =
            result.Series
            |> Map.toList
            |> Seq.collect(fun (name,series) ->
                series
                |> TimeSeries.toObservations
                |> Seq.map(fun (v,t) -> IndividualSeries.Row (subject, hypothesisId, name.Value, t, v.Fit, v.Obs, result.Likelihood )))


    let save directory subject modelId result =
        let csv = new IndividualSeries(result |> Row.fromResult subject modelId)
        let filePath = Config.filePath directory subject modelId result.ResultId Config.DataType.Series
        csv.Save(filePath)

    let load x =
        invalidOp "Not finished"


module EstimationResult =

    /// Save the Maximum Likelihood Estimate, trace of the optimisation
    /// procedure, and time-series.
    let saveAll directory subject modelId result =
        Trace.save directory subject modelId result
        MLE.save directory subject modelId result
        Series.save directory subject modelId result

    let loadAll directory subject modelId =
        invalidOp "Not implemented"


module ModelSelection =

    open Bristlecone.ModelSystem

    let save x =
        invalidOp "Not finished"


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
