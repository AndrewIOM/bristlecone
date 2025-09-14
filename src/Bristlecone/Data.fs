namespace Bristlecone.Data

open Bristlecone
open Bristlecone.ModelSystem
open FSharp.Data

/// <namespacedoc>
///   <summary>
/// Contains functions that enable loading and saving of core Bristlecone
/// types to file storage. Bristlecone loads and saves CSV data for both
/// individual model results (MLEs, traces, predicted vs observed series),
/// and for ensemble-level statistics (model selection / weights).
/// </summary>
/// </namespacedoc>
///
/// <summary>Specifies how file paths are constructed for different data types.</summary>
module Config =

    type DataType =
        | MLE
        | Trace
        | Series
        | Intervals
        | StepAhead of steps: int
        | Components

    type EnsembleType =
        | Weights
        | Convergence
        | RMSE

    let typeAsLabel dataType =
        match dataType with
        | MLE -> "mle"
        | Trace -> "trace"
        | Series -> "series"
        | Intervals -> "ci"
        | Components -> "components"
        | StepAhead(steps) -> sprintf "%i-step-ahead-prediction" steps

    let ensembleAsLabel dataType =
        match dataType with
        | Weights -> "ensemble-weights"
        | Convergence -> "ensemble-convergence"
        | RMSE -> "ensemble-rmse"

    let filePath directory subject modelId (resultId: System.Guid) dataType =
        let path = System.IO.DirectoryInfo(directory)

        if path.Exists then
            let t = typeAsLabel dataType
            sprintf "%sbristlecone-%s-%s-%s-%s.csv" path.FullName subject modelId t (resultId.ToString())
        else
            invalidArg "directory" "The specified directory does not exist"

    let filePathEnsemble directory dataType =
        let path = System.IO.DirectoryInfo(directory)
        let t = ensembleAsLabel dataType

        if path.Exists then
            sprintf "%sbristlecone-%s.csv" path.FullName t
        else
            invalidArg "directory" "The specified directory does not exist"

    let regexGuid =
        "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"

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
                | Components -> "components"
                | StepAhead(steps) -> sprintf "%istepahead" steps

            let files = path.GetFiles(sprintf "bristlecone-%s-%s-%s-*.csv" subject modelId t)
            let regex = sprintf "bristlecone-%s-%s-%s-(%s).csv" subject modelId t regexGuid

            files
            |> Seq.choose (fun f ->
                let m = System.Text.RegularExpressions.Regex.Match(f.Name, regex)

                if m.Success then
                    let guid = m.Groups.[1].Value |> System.Guid.Parse
                    (guid, f) |> Some
                else
                    None)
        else
            invalidArg "directory" "The specified directory does not exist"

/// <summary>Functions for loading and saving optimisation traces</summary>
[<RequireQualifiedAccess>]
module Trace =

    type BristleconeTrace = CsvProvider<"templates/individual-trace.csv", IgnoreErrors=true>

    module internal Row =

        let fromEstimate thinBy subject modelId result : seq<BristleconeTrace.Row> =
            result.Trace
            |> Seq.rev
            |> Seq.mapi (fun iterationNumber (likelihood, values) ->
                result.Parameters
                |> Parameter.Pool.toList
                |> Seq.mapi (fun i (name, _) ->
                    (subject, modelId, iterationNumber + 1, result.ResultId, name.Value, float likelihood, float values.[i])
                    |> BristleconeTrace.Row))
            |> if (thinBy |> Option.isSome) then
                   Seq.everyNth thinBy.Value
               else
                   id
            |> Seq.concat

        let toTrace (data: BristleconeTrace) : (float * float[]) list =
            data.Rows
            |> Seq.groupBy (fun r -> r.Iteration)
            |> Seq.map (fun (i, r) ->
                i, (r |> Seq.head).NegativeLogLikelihood, r |> Seq.map (fun r -> r.ParameterValue) |> Seq.toArray)
            |> Seq.sortByDescending (fun (i, _, _) -> i)
            |> Seq.map (fun (_, x, y) -> x, y)
            |> Seq.toList

    /// Save the trace of an `EstimationResult` to a CSV file.
    let save directory subject modelId thinBy result =
        let csv = new BristleconeTrace(result |> Row.fromEstimate thinBy subject modelId)

        let filePath =
            Config.filePath directory subject modelId result.ResultId Config.DataType.Trace

        csv.Save(filePath)

    /// Load a trace from a CSV file, as saved by Bristlecone.
    let load directory subject modelId =
        let traceFiles = Config.fileMatch directory subject modelId Config.DataType.Trace

        traceFiles
        |> Seq.choose (fun (i, f) ->
            let data = BristleconeTrace.Load f.FullName

            match data.Rows |> Seq.length with
            | 0 -> None
            | _ -> (i, data |> Row.toTrace) |> Some)

/// <summary>Functions of loading and saving Maximum Likelihood Estimates from model fits</summary>
[<RequireQualifiedAccess>]
module MLE =

    type IndividualMLE = CsvProvider<"templates/individual-mle.csv", IgnoreErrors=true>

    module internal Row =

        let fromResult subject hypothesisId result =
            result.Parameters
            |> Parameter.Pool.toTensorWithKeysReal
            |> fun (k,v) -> Seq.zip k (v |> Tensors.Typed.toFloatArray)
            |> Seq.map (fun (name, v) ->
                (subject,
                 hypothesisId,
                 result.ResultId,
                 name.Value,
                 result.Likelihood |> Units.removeUnitFromFloat,
                 v |> Units.removeUnitFromFloat)
                |> IndividualMLE.Row)

        let toResult (modelSystem: ModelSystem<'modelTimeUnit>) (data: IndividualMLE) =
            if data.Rows |> Seq.isEmpty then
                Error "An MLE file is corrupt"
            else
                let mle = (data.Rows |> Seq.head).NegativeLogLikelihood

                let pKeys = Parameter.Pool.keys modelSystem.Parameters
                let estimatedTheta =
                    pKeys
                    |> List.map(fun pName ->
                        let e = data.Rows |> Seq.find (fun r -> r.ParameterCode = pName.Value)
                        e.ParameterValue * 1.<parameter> )
                    |> List.toArray
                    |> Tensors.Typed.ofVector

                let newPool = Parameter.Pool.fromRealVector estimatedTheta modelSystem.Parameters

                (mle, newPool) |> Ok

    let save directory subject modelId result =
        let csv = new IndividualMLE(result |> Row.fromResult subject modelId)

        let filePath =
            Config.filePath directory subject modelId result.ResultId Config.DataType.MLE

        csv.Save(filePath)

    /// <summary>Load the Maximum Likelihood Estimates
    /// from the given directory for a given subject and model combination.</summary>
    /// <param name="directory">The results folder where files are saved</param>
    /// <param name="subject">An identifier for the subject of the fit</param>
    /// <param name="modelId">An identifier for the model that was fit</param>
    /// <returns>A sequence of tuples which contain the analysis ID followed by another tuple
    /// that contains the likelihood and theta (parameter set)</returns>
    let load directory subject (modelSystem: ModelSystem<'modelTimeUnit>) modelId =
        let files = Config.fileMatch directory subject modelId Config.DataType.MLE

        files
        |> Seq.choose (fun (i, f) ->
            let data = IndividualMLE.Load f.FullName

            match data.Rows |> Seq.length with
            | 0 -> None
            | _ ->
                let parsed = data |> Row.toResult modelSystem

                match parsed with
                | Ok mle -> (i, mle) |> Some
                | Error _ -> None)

    /// <summary>Load the Maximum Likelihood Estimate (with the lowest -log Liklihood)
    /// from the given directory for a given subject and model combination.</summary>
    /// <param name="directory">The results folder where files are saved</param>
    /// <param name="subject">An identifier for the subject of the fit</param>
    /// <param name="modelId">An identifier for the model that was fit</param>
    /// <returns>A tuple containing the analysis ID followed by another tuple
    /// that contains the likelihood and theta (parameter set)</returns>
    let loadBest directory subject modelSystem modelId =
        load directory subject modelSystem modelId
        |> Seq.minBy (fun (_, (mle, _)) -> mle)

/// <summary>Functions for loading and saving predicted vs observed time-series for model fits</summary>
[<RequireQualifiedAccess>]
module Series =

    open Bristlecone.Time

    type IndividualSeries = CsvProvider<"templates/individual-series.csv", IgnoreErrors=true>

    module internal Row =

        let fromResult subject hypothesisId result =
            result.Series
            |> Map.toList
            |> Seq.collect (fun (name, series) ->
                series
                |> TimeSeries.toObservations
                |> Seq.map (fun (v, t) ->
                    IndividualSeries.Row(subject, hypothesisId, name.Value, t, float v.Fit, float v.Obs, float result.Likelihood)))

        let toSeries (data: IndividualSeries) =
            data.Rows
            |> Seq.groupBy (fun r -> r.Variable)
            |> Seq.choose (fun (g, r) ->
                let ts =
                    r
                    |> Seq.map (fun r -> ({ Fit = r.Expected * 1.<state>; Obs = r.Observed * 1.<state> }, r.Time))
                    |> TimeSeries.fromObservations DateMode.calendarDateMode

                ShortCode.create g |> Option.map (fun c -> c, ts))
            |> Map.ofSeq

    let save directory subject modelId result =
        let csv = new IndividualSeries(result |> Row.fromResult subject modelId)

        let filePath =
            Config.filePath directory subject modelId result.ResultId Config.DataType.Series

        csv.Save(filePath)

    let load directory subject modelId =
        let seriesFiles = Config.fileMatch directory subject modelId Config.DataType.Series

        seriesFiles
        |> Seq.choose (fun (i, f) ->
            let data = IndividualSeries.Load f.FullName

            match data.Rows |> Seq.length with
            | 0 -> None
            | _ -> (i, data |> Row.toSeries) |> Some)

[<RequireQualifiedAccess>]
module EstimationResult =

    /// <summary>Save the Maximum Likelihood Estimate, trace of the optimisation
    /// procedure, and time-series.</summary>
    /// <param name="directory">Relative or absolute directory to save files to</param>
    /// <param name="subject">An identifier for the subject of the test</param>
    /// <param name="modelId">An identifier for the model used</param>
    /// <param name="thinTraceBy">If Some, an integer representing the nth traces to keep from the optimisation trace.
    /// If None, does not thin the trace.</param>
    /// <param name="result">The estimation result to save</param>
    let saveAll directory subject modelId thinTraceBy result =
        Trace.save directory subject modelId thinTraceBy result
        MLE.save directory subject modelId result
        Series.save directory subject modelId result

    /// Load an `EstimationResult` that has previously been saved as
    /// three seperate dataframes. Results will only be reconstructed
    /// when file names and formats are in original Bristlecone format.
    let loadAll directory subject (modelSystem: ModelSystem<'modelTimeUnit>) modelId =
        let mles =
            MLE.load directory subject modelSystem modelId
            |> Seq.map (fun (k, v) -> k.ToString(), v)

        let series =
            Series.load directory subject modelId |> Seq.map (fun (k, v) -> k.ToString(), v)

        let traces =
            Trace.load directory subject modelId |> Seq.map (fun (k, v) -> k.ToString(), v)

        mles
        |> Seq.keyMatch series
        |> Seq.map (fun (k, v1, v2) -> (k, (v1, v2)))
        |> Seq.keyMatch traces
        |> Seq.map (fun (k, t, (s, (l, p))) ->
            { ResultId = k |> System.Guid.Parse
              Likelihood = l * 1.<``-logL``>
              Parameters = p
              Series = s
              InternalDynamics = None
              Trace = t |> List.map(fun (x,y) -> x * 1.<``-logL``>, y |> Array.map ((*) 1.<parameter>)) }
        )

[<RequireQualifiedAccess>]
module Confidence =

    open Bristlecone.Confidence

    type IndividualCI = CsvProvider<"templates/individual-mle-ci.csv">

    module Row =

        let fromResult subject hypothesisId runId (result: CodedMap<ConfidenceInterval>) =
            result
            |> Map.toList
            |> Seq.collect (fun (name, ci) ->
                [ 68., ci.``68%``; 95., ci.``95%`` ]
                |> Seq.map (fun (i, iv) ->
                    (subject, hypothesisId, runId, name.Value, i, iv.Lower |> Units.removeUnitFromFloat, iv.Upper |> Units.removeUnitFromFloat)
                    |> IndividualCI.Row))

    let save directory subject modelId runId result =
        let csv = new IndividualCI(result |> Row.fromResult subject modelId runId)

        let filePath =
            Config.filePath directory subject modelId runId Config.DataType.Intervals

        csv.Save(filePath)

[<RequireQualifiedAccess>]
module ModelSelection =

    open Bristlecone.ModelSelection.Akaike

    type EnsembleAIC = CsvProvider<"templates/ensemble-model-selection.csv">

    module Row =

        let fromResult (result: seq<string * string * EstimationResult<'data, 'timeunit, 'timespan> * AkaikeWeight>) =
            result
            |> Seq.map (fun (subject, hypothesisId, r, aic) ->
                (subject, hypothesisId, r.ResultId, float aic.Likelihood, aic.AIC, aic.AICc, aic.Weight)
                |> EnsembleAIC.Row)

    let save directory result =
        let csv = new EnsembleAIC(result |> Row.fromResult)
        let filePath = Config.filePathEnsemble directory Config.EnsembleType.Weights
        csv.Save(filePath)

[<RequireQualifiedAccess>]
module Convergence =

    type ConvergenceStats = CsvProvider<"templates/ensemble-convergence.csv">

    let toCsvRows (result: seq<Diagnostics.Convergence.ConvergenceStatistic>) =
        result
        |> Seq.map (fun r ->
            ConvergenceStats.Row(r.Subject, r.HypothesisId, r.Parameter.Value, r.StatisticName, r.StatisticValue))

    let save directory result =
        let csv = new ConvergenceStats(result |> toCsvRows)
        let filePath = Config.filePathEnsemble directory Config.EnsembleType.Convergence
        csv.Save(filePath)

/// <summary>Loads and saves one-step-ahead and n-step-ahead prediction
/// results into and out of simple CSV files</summary>
[<RequireQualifiedAccess>]
module NStepAhead =

    type NStepAhead = CsvProvider<"templates/individual-n-step-ahead.csv">
    type NStepAheadStats = CsvProvider<"templates/ensemble-rmse.csv">

    let internal toCsvRows
        subjectId
        hypothesisId
        analysisId
        nSteps
        (result: CodedMap<FitSeries<System.DateTime, 'timeunit, 'timespan> * Statistics.NStepStatistics>)
        =
        result
        |> Seq.collect (fun r ->
            r.Value
            |> fst
            |> Time.TimeSeries.toObservations
            |> Seq.map (fun (fit, t) ->
                NStepAhead.Row(subjectId, hypothesisId, analysisId, t, float fit.Obs, nSteps, float fit.Fit)))

    let internal toStatCsvRows
        (results:
            seq<
                string *
                string *
                CodedMap<ModelSystem.FitSeries<'date, 'timeunit, 'timespan> * Statistics.NStepStatistics>
             >)
        =
        results
        |> Seq.collect (fun (s, h, r) ->
            r
            |> Seq.map (fun kv -> NStepAheadStats.Row(s, h, kv.Key.Value, "RMSE", (snd kv.Value).RMSE)))

    /// <summary>Save an individual n-step prediction (excluding statistics) for an
    /// individual subject and hypothesis / model.</summary>
    /// <param name="directory">The file directory to save to</param>
    /// <param name="subjectId">Subject ID</param>
    /// <param name="modelId">Model ID or Hypothesis ID</param>
    /// <param name="analysisId">The unique reference of the estimation result used to generate n-step predictions</param>
    /// <param name="stepsAhead">The number of time-steps ahead predicted</param>
    /// <param name="result">The n-step prediction from Bristlecone.oneStepAhead or similar</param>
    let save directory subjectId modelId analysisId stepsAhead result =
        let csv =
            new NStepAhead(result |> toCsvRows subjectId modelId analysisId stepsAhead)

        let filePath =
            Config.filePath directory subjectId modelId analysisId (Config.DataType.StepAhead stepsAhead)

        csv.Save(filePath)

    /// <summary>Saves a full ensemble overview of the root mean squared error
    /// of n-step ahead predictions.</summary>
    /// <param name="directory">The file directory to save to</param>
    /// <param name="results">A sequence of subjectID * hypothesisID * n-step result</param>
    let saveAllRMSE directory results =
        let csv = new NStepAheadStats(results |> toStatCsvRows)
        let filePath = Config.filePathEnsemble directory (Config.EnsembleType.RMSE)
        csv.Save(filePath)
