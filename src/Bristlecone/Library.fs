namespace Bristlecone

open System
open Bristlecone.Logging

/// <namespacedoc>
///   <summary>The core library of Bristlecone, containing model-fitting functions.</summary>
/// </namespacedoc>
///
/// Main functionality of Bristlecone, including functions to scaffold
/// `ModelSystem`s and for model-fitting (tests and real fits).
[<RequireQualifiedAccess>]
module Bristlecone =

    open Bristlecone.Time
    open Bristlecone.ModelSystem
    open Bristlecone.EstimationEngine
    open Bristlecone.Statistics

    /// <summary>A basic estimation engine for discrete-time equations, using a Nelder-Mead optimiser.</summary>
    let mkDiscrete: EstimationEngine<float, 'date, 'timeunit, 'timespan> =
        { TimeHandling = Discrete
          OptimiseWith = Optimisation.Amoeba.single Optimisation.Amoeba.Solver.Default
          LogTo = Console.logger (1000)
          Random = MathNet.Numerics.Random.MersenneTwister(true)
          Conditioning = Conditioning.NoConditioning }

    /// <summary>A basic estimation engine for ordinary differential equations, using a Nelder-Mead optimiser.</summary>
    let mkContinuous: EstimationEngine<float, 'date, 'timeunit, 'timespan> =
        { TimeHandling = Continuous <| Integration.MathNet.integrate
          OptimiseWith = Optimisation.Amoeba.single Optimisation.Amoeba.Solver.Default
          LogTo = Bristlecone.Logging.Console.logger (1000)
          Random = MathNet.Numerics.Random.MersenneTwister(true)
          Conditioning = Conditioning.RepeatFirstDataPoint }

    /// <summary>Substitute a specific logger into</summary>
    /// <param name="out"></param>
    /// <param name="engine"></param>
    /// <typeparam name="'a"></typeparam>
    /// <typeparam name="'b"></typeparam>
    /// <returns></returns>
    let withOutput out engine = { engine with LogTo = out }

    /// Use a mersenne twister random number generator
    /// with a specific seed.
    let withSeed seed engine =
        { engine with
            Random = MathNet.Numerics.Random.MersenneTwister(seed, true) }

    /// Use a custom integration method
    let withContinuousTime t engine =
        { engine with
            TimeHandling = Continuous t }

    /// Choose how the start point is chosen when solving the model system
    let withConditioning c engine = { engine with Conditioning = c }

    let withTunedMCMC tuning engine =
        { engine with
            OptimiseWith = Optimisation.MonteCarlo.randomWalk tuning }

    let withGradientDescent engine =
        { engine with
            OptimiseWith = Optimisation.Amoeba.single Optimisation.Amoeba.Solver.Default }

    let withCustomOptimisation optim engine = { engine with OptimiseWith = optim }

    module internal Fit =

        /// Places a map of `TimeSeries` into a `TimeFrame` that has data
        /// that shares a common timeline. If no timeline is shared, returns
        /// an `Error`.
        let observationsToCommonTimeFrame (equations: CodedMap<ModelEquation<'data>>) timeSeriesData =
            let equationKeys = equations |> Map.keys

            timeSeriesData
            |> Map.filter (fun k _ -> equationKeys |> Seq.contains k)
            |> TimeFrame.tryCreate
            |> Result.ofOption "Observations for dynamic variables must share a common sampling time sequence"

        /// Finds environmental data in a timeseries map (i.e. those datasets that are not
        /// dynamic variables or measures), and constructs a common `TimeFrame`.
        /// Returns None if there is no environmental data.
        let environmentDataToCommonTimeFrame dynamicVariableKeys measureKeys timeSeriesData =
            let environmentSeries =
                timeSeriesData
                |> Map.filter (fun k _ -> dynamicVariableKeys |> Seq.append measureKeys |> Seq.contains k |> not)

            match environmentSeries.Count with
            | 0 -> None |> Ok
            | _ ->
                match TimeFrame.tryCreate environmentSeries with
                | Some e -> e |> Some |> Ok
                // TODO Ensure that environment data overlaps with conditioning period?
                | None ->
                    Error
                        "Forcing variables were specified on differing timelines. Pre-process your data to make environmental data conform to a common temporal baseline and time interval"

        /// TODO make function to check if time-series one is a subset of
        /// time-series two.
        let exactSubsetOf one two =
            // match environmentVariableResolution with
            // | None -> ()
            // | Some eRes ->
            Ok(one, two)

        /// Returns a tuple of the start point (t0) and the
        /// subsequent time-series (t1 .. tn).
        let t0 timeSeriesData (conditionMode: Conditioning.Conditioning<'a>) logger =
            timeSeriesData
            |> Solver.Conditioning.startPoint conditionMode
            // |> Option.map(fun t0 -> t0, timeSeriesData)
            |> Option.defaultWith (fun () ->
                logger
                <| DebugEvent "No conditioning was specified. Using t1 as conditioning data."

                invalidOp "Not supported"
            // match t0 with
            // | Some t0 ->
            //     let startIndex = timeline |> Seq.head
            //     // TODO. The environmental data must span the conditioning period - how to ensure this?
            //     fixedStep engine.LogTo engine.TimeHandling (startIndex - externalSteps.Head) endIndex t0 forcings
            // | None ->
            //     engine.LogTo <| DebugEvent "No conditioning was specified. Using t1 as conditioning data."
            //     let startIndex = timeline |> Seq.tail |> Seq.head
            //     fixedStep engine.LogTo engine.TimeHandling (startIndex - externalSteps.Head) endIndex t0 forcings
            )


    /// <summary>
    /// Fit a time-series model to data.
    ///
    /// Please note: it is strongly recommended that you test that the given `EstimationEngine`
    /// can correctly identify known parameters for your model. Refer to the `Bristlecone.testModel`
    /// function, which can be used to generate known data and complete this process.
    /// </summary>
    /// <param name="engine">The engine encapsulates all settings that form part of the estimation
    /// method. Importantly, this includes the random number generator used for all stages
    /// of the analysis; if this is set using a fixed seed, the result will be reproducable.</param>
    /// <param name="endCondition">You must specify a stopping condition, after which
    /// the optimisation process will cease. Bristlecone includes built-in end conditions
    /// in the `Bristlecone.Optimisation.EndConditions` module.</param>
    /// <param name="timeSeriesData"></param>
    /// <param name="model"></param>
    /// <returns></returns>
    let tryFit engine endCondition timeSeriesData (model: ModelSystem<'data>) =

        // A. Setup initial time point values based on conditioning method.
        let t0 = Fit.t0 timeSeriesData engine.Conditioning engine.LogTo

        // Check there is time-series data actually included and corresponding to correct equations.
        let hasRequiredData =
            if timeSeriesData.IsEmpty then
                Error "No time-series data was specified"
            else if Set.isSubset (model.Equations |> Map.keys |> set) (timeSeriesData |> Map.keys |> set) then
                Ok timeSeriesData
            else
                Error(
                    sprintf
                        "Required time-series data were missing. Need: %A"
                        (model.Equations |> Map.keys |> Seq.map (fun k -> k.Value) |> String.concat " + ")
                )

        // B. Create a continuous-time that outputs float[]
        // containing only the values for the dynamic variable resolution.
        let continuousSolver =
            result {

                let! timeSeriesData = hasRequiredData

                // 1. Set time-series into common timeline
                let! commonDynamicTimeFrame = Fit.observationsToCommonTimeFrame model.Equations timeSeriesData

                let! commonForcingsTimeFrame =
                    Fit.environmentDataToCommonTimeFrame
                        (Map.keys model.Equations)
                        (Map.keys model.Measures)
                        timeSeriesData

                engine.LogTo
                <| GeneralEvent(
                    sprintf
                        "Time-series start at %s with resolution %A."
                        (commonDynamicTimeFrame.StartDate.ToString "yyyy/MM/dd")
                        (commonDynamicTimeFrame |> TimeFrame.resolution)
                )

                // 2. Ensure that dynamic variables is an exact or subset of environmental variables
                let! _ = Fit.exactSubsetOf commonDynamicTimeFrame commonForcingsTimeFrame

                let solver outputStep =
                    Solver.solver outputStep commonDynamicTimeFrame commonForcingsTimeFrame engine t0

                return solver
            }

        let discreteSolver = Solver.Discrete.solve t0

        let data = timeSeriesData |> Map.map (fun _ ts -> ts.Values |> Seq.toArray)

        // Setup optimisation algorithm given the parameter constraint mode.
        let optimise, constrainedParameters =
            match engine.OptimiseWith with
            | Optimiser.InDetachedSpace optim ->
                let detatchedPool, optimConstraints =
                    Parameter.Pool.detatchConstraints model.Parameters

                optim
                    engine.Random
                    engine.LogTo
                    endCondition
                    (detatchedPool |> Parameter.Pool.toDomain optimConstraints)
                    None,
                detatchedPool
            | Optimiser.InTransformedSpace optim ->
                let optimConstraints =
                    List.init (Parameter.Pool.count model.Parameters) (fun _ -> Parameter.Unconstrained)

                optim
                    engine.Random
                    engine.LogTo
                    endCondition
                    (model.Parameters |> Parameter.Pool.toDomain optimConstraints)
                    None,
                model.Parameters

        result {
            let! continuousSolver = continuousSolver

            let objective =
                Objective.create
                    { model with
                        Parameters = constrainedParameters }
                    (continuousSolver Solver.StepType.External)
                    discreteSolver
                    data

            let result = objective |> optimise
            let lowestLikelihood, bestPoint = result |> List.minBy (fun (l, _) -> l)

            let estimatedSeries =
                Objective.predict
                    { model with
                        Parameters = constrainedParameters }
                    (continuousSolver Solver.StepType.External)
                    discreteSolver
                    bestPoint

            let estimatedHighRes =
                Objective.predict
                    { model with
                        Parameters = constrainedParameters }
                    (continuousSolver Solver.StepType.Internal)
                    discreteSolver
                    bestPoint

            let paired =
                timeSeriesData
                |> Map.filter (fun key _ -> estimatedSeries |> Map.containsKey key)
                |> Map.map (fun k observedSeries ->
                    let expected = estimatedSeries |> Map.find k

                    observedSeries
                    |> TimeSeries.toObservations
                    |> Seq.zip expected
                    |> Seq.map (fun (e, (o, d)) -> ({ Obs = o; Fit = e }, d))
                    |> TimeSeries.fromObservations observedSeries.DateMode)

            return
                { ResultId = Guid.NewGuid()
                  Likelihood = lowestLikelihood
                  Parameters = bestPoint |> Parameter.Pool.fromPointInTransformedSpace constrainedParameters
                  Series = paired
                  Trace = result
                  InternalDynamics = Some estimatedHighRes }
        }

    /// <summary>Fit a time-series model to data.</summary>
    /// <param name="engine">An estimation engine configured and tested for the given model.</param>
    /// <param name="endCondition">The condition at which optimisation should cease.</param>
    /// <param name="timeSeriesData">Time-series dataset that contains a series for each equation in the model system.</param>
    /// <param name="model">A model system of equations, likelihood function, estimatible parameters, and optional measures.</param>
    /// <returns>The result of the model-fitting procedure. If an error occurs, throws an exception.</returns>
    let fit engine endCondition timeSeriesData (model: ModelSystem) =
        tryFit engine endCondition timeSeriesData model |> Result.forceOk

    open Test

    /// <summary>Tests that the specified estimation engine can correctly
    /// estimate known parameters given specfici test settings.
    /// Random parameter sets and resultant fake time-series data are generated
    /// for the model system by using the rules and noise generation settings
    /// in the stated test settings.</summary>
    /// <param name="engine"></param>
    /// <param name="settings"></param>
    /// <param name="model"></param>
    /// <returns>A test result that indicates the error structure.
    /// It is wrapped in an F# Result, indicating if the procedure
    /// was successful or not.</returns>
    let tryTestModel engine (settings: Test.TestSettings<float>) (model: ModelSystem) =
        engine.LogTo <| GeneralEvent "Attempting to generate parameter set."

        engine.LogTo
        <| GeneralEvent(
            sprintf
                "The data must comply with %i rules after %i tries."
                settings.GenerationRules.Length
                settings.Attempts
        )

        result {
            let! settings = Test.isValidSettings model settings
            let! trueData, theta = Test.Compute.tryGenerateData engine settings model settings.Attempts

            let realEstimate =
                fit
                    { engine with
                        OptimiseWith = Optimisation.None.none }
                    settings.EndCondition
                    (Map.merge trueData settings.EnvironmentalData (fun x y -> x))
                    { model with
                        Parameters = Parameter.Pool.fromEstimated theta }

            let estimated =
                fit engine settings.EndCondition (Map.merge trueData settings.EnvironmentalData (fun x y -> x)) model

            let paramDiffs: Test.ParameterTestResult list =
                estimated.Parameters
                |> Parameter.Pool.toList
                |> List.map (fun (k, v) ->
                    let est = Parameter.getEstimate v

                    let real =
                        theta
                        |> Parameter.Pool.toList
                        |> List.find (fun (k2, v) -> k2 = k)
                        |> snd
                        |> Parameter.getEstimate

                    match est with
                    | Ok e ->
                        match real with
                        | Ok r ->
                            { Identifier = k.Value
                              RealValue = r
                              EstimatedValue = e }
                        | Error _ -> failwith "Error"
                    | Error _ -> failwith "Error")

            let errorStructure =
                estimated.Series
                |> Seq.map (fun k -> k.Key.Value, k.Value.Values |> Seq.map (fun t -> (t.Fit - t.Obs) ** 2.))
                |> Map.ofSeq

            return
                { ErrorStructure = errorStructure
                  Parameters = paramDiffs
                  Series = estimated.Series |> Seq.map (fun k -> k.Key.Value, k.Value) |> Map.ofSeq
                  RealLikelihood = realEstimate.Likelihood
                  EstimatedLikelihood = estimated.Likelihood }
        }

    /// <summary>Test that the specified estimation engine can correctly
    /// estimate known parameters. Random parameter sets are generated
    /// from the given model system.</summary>
    /// <param name="engine">An estimation engine containing the method used for model-fitting.</param>
    /// <param name="settings">Test settings that define how the test will be conducted.</param>
    /// <param name="model">The model system to test against the estimation engine.</param>
    /// <returns>A test result that indicates differences between the expected and actual fit.</returns>
    let testModel engine settings model =
        tryTestModel engine settings model |> Result.forceOk

    /// <summary>Repeat a model fit many times, removing a single data point at random each time.</summary>
    /// <param name="engine">The estimation engine / fitting method</param>
    /// <param name="endCondition">The end condition for each model fit</param>
    /// <param name="bootstrapCount">Number of times to bootstrap data</param>
    /// <param name="model">A model system / hypothesis to fit</param>
    /// <param name="series">Time-series to fit with model</param>
    /// <returns>A list of estimation results (one for each bootstrap) for further analysis</returns>
    let bootstrap (engine: EstimationEngine.EstimationEngine<'T, 'date, 'timeunit, 'timespan>) endCondition bootstrapCount model series =
        let rec bootstrap s numberOfTimes solutions =
            if (numberOfTimes > 0) then
                let subset = TimeSeries.Bootstrap.removeSingle engine.Random () s // TODO STEPPING
                let result = fit engine endCondition subset model

                engine.LogTo <| GeneralEvent(sprintf "Completed bootstrap %i" numberOfTimes)

                bootstrap s (numberOfTimes - 1) (solutions |> List.append [ result ])
            else
                solutions

        bootstrap series bootstrapCount []

    /// <summary>Addresses the question: "How good am I at predicting the next data point?. Given fitted parameters,
    /// assesses how well the model predicts the next data point from each point in the time-series data.
    /// This approach can provide another indication of model performance.</summary>
    /// <param name="engine">The exact estimation engine used for the existing model fit</param>
    /// <param name="hypothesis">The exact model system / hypothesis from which parameters have been already estimated</param>
    /// <param name="preTransform">A function that may transform each shorter time-series before prediction. This may be needed,
    /// for example, if there are custom start values that need to be configured in a complex way (e.g. for derived mesaurement
    /// variables).</param>
    /// <param name="timeSeries">The observed data to predict against.</param>
    /// <param name="estimatedTheta">A parameter pool containing already estimated parameters from model fitting step</param>
    /// <returns>A time-series for each variable containing a step-ahead prediction</returns>
    let oneStepAhead
        engine
        hypothesis
        (preTransform: CodedMap<TimeSeries<'T, 'date, 'timeunit, 'timespan>> -> CodedMap<TimeSeries<'T, 'date, 'timeunit, 'timespan>>)
        (timeSeries)
        (estimatedTheta: Parameter.Pool)
        : CodedMap<FitSeries<'date, 'timeunit, 'timespan> * NStepStatistics> =
        let hypothesisMle: ModelSystem<'T> =
            { hypothesis with
                Parameters = Parameter.Pool.fromEstimated estimatedTheta }

        let pairedDataFrames =
            timeSeries
            |> Map.map (fun _ fitSeries ->
                fitSeries
                |> TimeSeries.toObservations
                |> Seq.pairwise
                |> Seq.map (fun (t1, t2) -> TimeSeries.fromObservations fitSeries.DateMode [ t1; t2 ] |> TimeSeries.map (fun (x, y) -> x)))

        let timeParcelCount = (pairedDataFrames |> Seq.head).Value |> Seq.length

        let data =
            seq { 1..timeParcelCount }
            |> Seq.map (fun i -> pairedDataFrames |> Map.map (fun _ v -> v |> Seq.item (i - 1)) |> preTransform)

        // It is predicting with a repeated first point:
        // The next point estimate is at t1
        // The next point observation is at t2
        data
        |> Seq.collect (fun d ->
            let est =
                fit
                    (engine
                     |> withCustomOptimisation Optimisation.None.none
                     |> withConditioning Conditioning.RepeatFirstDataPoint)
                    (Optimisation.EndConditions.afterIteration 0)
                    d
                    hypothesisMle

            let nextObservation =
                d
                |> Map.map (fun c ts -> ts |> TimeSeries.toObservations |> Seq.skip 1 |> Seq.head)

            let paired =
                nextObservation
                |> Seq.map (fun kv ->
                    let nextEstimate = (est.Series.[kv.Key].Values |> Seq.head).Fit

                    (kv.Key,
                     { Obs = kv.Value |> fst
                       Fit = nextEstimate },
                     kv.Value |> snd))

            paired)
        |> Seq.groupBy (fun (k, _, _) -> k)
        |> Seq.map (fun (tsName, values) ->
            let sos = values |> Seq.averageBy (fun (_, x, _) -> (x.Obs - x.Fit) ** 2.)
            tsName, (values |> Seq.map (fun (_, v, t) -> (v, t)) |> TimeSeries.fromObservations, { RMSE = sqrt sos }))
        |> Map.ofSeq


    /// <summary>Wrappers for fitting functions that use `Array.Parallel` to run many analyses at once.</summary>
    module Parallel =

        /// <summary>A wrapper for `Bristlecone.fit` that uses Array.Parallel to run many analyses at once.</summary>
        /// <param name="engine">An estimation engine</param>
        /// <param name="endCondition">An end condition</param>
        /// <param name="model">The model / hypothesis to fit</param>
        /// <param name="timeSeries">Time-series data to fit with the model</param>
        /// <returns>A list of estimation results</returns>
        let fit engine endCondition model timeSeries =
            timeSeries
            |> Array.Parallel.map (fun g -> fit engine endCondition g model)
            |> Array.toList
