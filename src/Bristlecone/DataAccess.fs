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
        | Intervals

    let filePath directory subject modelId (resultId:System.Guid) dataType =
        let path = System.IO.DirectoryInfo(directory)
        if path.Exists then
            let t = 
                match dataType with
                | MLE -> "mle"
                | Trace -> "trace"
                | Series -> "series"
                | Intervals -> "ci"
            sprintf "%sbristlecone-%s-%i-%s-%s.csv" path.FullName subject modelId t (resultId.ToString())
        else invalidArg "directory" "The specified directory does not exist"

    let filePathEnsemble directory =
        let path = System.IO.DirectoryInfo(directory)
        if path.Exists then sprintf "%sbristlecone-ensemble-weights.csv" path.FullName
        else invalidArg "directory" "The specified directory does not exist"

    let regexGuid = "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"

    /// Find files in a directory that match the standard bristlecone
    /// file naming scheme.
    let fileMatch directory subject modelId dataType =
        let path = System.IO.DirectoryInfo(directory)
        if path.Exists then
            let t = 
                match dataType with
                | MLE -> "mle"
                | Trace -> "trace"
                | Series -> "series"
                | Intervals -> "ci"
            let files = path.GetFiles(sprintf "bristlecone-%s-%i-%s-*.csv" subject modelId t)
            let regex = sprintf "bristlecone-%s-%i-%s-(%s).csv" subject modelId t regexGuid
            files |> Seq.choose(fun f -> 
                let m = System.Text.RegularExpressions.Regex.Match(f.Name, regex)
                if m.Success
                then 
                    let guid = m.Groups.[1].Value |> System.Guid.Parse
                    (guid, f) |> Some
                else None )
        else invalidArg "directory" "The specified directory does not exist"


module Trace =

    type BristleconeTrace = CsvProvider<"templates/individual-trace.csv", IgnoreErrors = true>

    module Row =

        let fromEstimate thinBy subject modelId (result:EstimationResult) : seq<BristleconeTrace.Row> =
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
            |> Seq.everyNth thinBy
            |> Seq.concat

        let toTrace (data:BristleconeTrace) : (float * float []) list =
            data.Rows
            |> Seq.groupBy(fun r -> r.Iteration)
            |> Seq.map(fun (i,r) -> 
                (r |> Seq.head).NegativeLogLikelihood, 
                r |> Seq.map(fun r -> r.ParameterValue) |> Seq.toArray)
            |> Seq.toList

    let save directory subject modelId thinBy result =
        let csv = new BristleconeTrace(result |> Row.fromEstimate thinBy subject modelId)
        let filePath = Config.filePath directory subject modelId result.ResultId Config.DataType.Trace
        csv.Save(filePath)

    /// Load a trace from a file
    let load directory subject modelId =
        let traceFiles = Config.fileMatch directory subject modelId Config.DataType.Trace
        traceFiles
        |> Seq.choose(fun (i,f) ->
            let data = BristleconeTrace.Load f.FullName
            match data.Rows |> Seq.length with
            | 0 -> None
            | _ -> (i, data |> Row.toTrace) |> Some )


module MLE =

    type IndividualMLE = CsvProvider<"templates/individual-mle.csv", IgnoreErrors = true>

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

        let toResult (data:IndividualMLE) =
            if data.Rows |> Seq.isEmpty
            then Error "An MLE file is corrupt"
            else 
                let mle = (data.Rows |> Seq.head).NegativeLogLikelihood
                let pool = 
                    data.Rows
                    |> Seq.map(fun r -> (ShortCode.create r.ParameterCode, r.ParameterValue))
                    |> Map.ofSeq
                (mle, pool) |> Ok

    let save directory subject modelId result =
        let csv = new IndividualMLE (result |> Row.fromResult subject modelId)
        let filePath = Config.filePath directory subject modelId result.ResultId Config.DataType.MLE
        csv.Save(filePath)

    let load directory subject modelId =
        let traceFiles = Config.fileMatch directory subject modelId Config.DataType.MLE
        traceFiles
        |> Seq.choose(fun (i,f) ->
            let data = IndividualMLE.Load f.FullName
            match data.Rows |> Seq.length with
            | 0 -> None
            | _ ->
                let parsed = data |> Row.toResult
                match parsed with
                | Ok mle -> (i, mle) |> Some
                | Error _ -> None )


module Series =

    type IndividualSeries = CsvProvider<"templates/individual-series.csv", IgnoreErrors = true>

    module Row =

        let fromResult subject hypothesisId (result:EstimationResult) =
            result.Series
            |> Map.toList
            |> Seq.collect(fun (name,series) ->
                series
                |> TimeSeries.toObservations
                |> Seq.map(fun (v,t) -> IndividualSeries.Row (subject, hypothesisId, name.Value, t, v.Fit, v.Obs, result.Likelihood )))

        let toSeries (data:IndividualSeries) : CodedMap<FitSeries> =
            data.Rows
            |> Seq.groupBy(fun r -> r.Variable)
            |> Seq.map(fun (g,r) -> 
                let ts = 
                    r 
                    |> Seq.map(fun r -> ({ Fit = r.Expected; Obs = r.Observed}, r.Time))
                    |> TimeSeries.fromObservations
                (ShortCode.create g, ts))
            |> Map.ofSeq

    let save directory subject modelId result =
        let csv = new IndividualSeries(result |> Row.fromResult subject modelId)
        let filePath = Config.filePath directory subject modelId result.ResultId Config.DataType.Series
        csv.Save(filePath)

    let load directory subject modelId =
        let seriesFiles = Config.fileMatch directory subject modelId Config.DataType.Series
        seriesFiles
        |> Seq.choose(fun (i,f) ->
            let data = IndividualSeries.Load f.FullName
            match data.Rows |> Seq.length with
            | 0 -> None
            | _ -> (i, data |> Row.toSeries) |> Some )


module EstimationResult =

    /// Save the Maximum Likelihood Estimate, trace of the optimisation
    /// procedure, and time-series.
    let saveAll directory subject modelId thinTraceBy result =
        Trace.save directory subject modelId thinTraceBy result
        MLE.save directory subject modelId result
        Series.save directory subject modelId result

    /// Load an `EstimationResult` that has previously been saved as
    /// three seperate dataframes. Results will only be reconstructed
    /// when file names and formats are in original Bristlecone format.
    let loadAll directory subject (modelSystem:ModelSystem) modelId =
        let mles = MLE.load directory subject modelId |> Seq.map(fun (k,v) -> k.ToString(), v)
        let series = Series.load directory subject modelId |> Seq.map(fun (k,v) -> k.ToString(), v)
        let traces = Trace.load directory subject modelId |> Seq.map(fun (k,v) -> k.ToString(), v)
        mles
        |> Seq.keyMatch series
        |> Seq.map(fun (k,v1,v2) -> (k, (v1, v2)))
        |> Seq.keyMatch traces
        |> Seq.map(fun (k,t,(s,(l,p))) ->
            { ResultId = k |> System.Guid.Parse
              Likelihood = l
              Parameters = modelSystem.Parameters |> Map.map(fun k v -> Parameter.setEstimate v (p |> Map.find k))
              Series = s 
              Trace = t })


module Confidence =

    open Bristlecone.Optimisation.ConfidenceInterval

    type IndividualCI = CsvProvider<"templates/individual-mle-ci.csv">

    module Row =

        let fromResult subject hypothesisId runId (result:CodedMap<ConfidenceInterval>) =
            result
            |> Map.toList
            |> Seq.collect(fun (name,ci) ->
                [ 68., ci.``68%``; 95., ci.``95%`` ]
                |> Seq.map(fun (i,iv) ->
                    (subject, hypothesisId, runId,
                     name.Value, i, iv.Lower, iv.Upper) |> IndividualCI.Row ))

    let save directory subject modelId runId result =
        let csv = new IndividualCI (result |> Row.fromResult subject modelId runId)
        let filePath = Config.filePath directory subject modelId runId Config.DataType.Intervals
        csv.Save(filePath)


module ModelSelection =

    open Bristlecone.ModelSelection.Akaike

    type EnsembleAIC = CsvProvider<"templates/ensemble-model-selection.csv">

    module Row = 

        let fromResult (result:seq<string * int * EstimationResult * AkaikeWeight>) =
            result
            |> Seq.map(fun (subject,hypothesisId,r,aic) ->
                (subject, hypothesisId, r.ResultId,
                 aic.Likelihood, aic.AIC, aic.AICc, aic.Weight) |> EnsembleAIC.Row )

    let save directory result =
        let csv = new EnsembleAIC (result |> Row.fromResult)
        let filePath = Config.filePathEnsemble directory
        csv.Save(filePath)


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
