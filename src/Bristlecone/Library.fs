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
    let mkDiscrete () =
        { TimeHandling = Discrete
          OptimiseWith = Optimisation.Amoeba.single Optimisation.Amoeba.Solver.Default
          LogTo = Console.logger 1000<iteration>
          Random = MathNet.Numerics.Random.MersenneTwister true
          ToModelTime = DateMode.Conversion.CalendarDates.toYears
          InterpolationGlobal = Solver.InterpolationMode.Lower
          InterpolationPerVariable = Map.empty
          Conditioning = Conditioning.NoConditioning }

    /// <summary>A basic estimation engine for ordinary differential equations, using a Nelder-Mead optimiser.</summary>
    let mkContinuous () =
        { TimeHandling = Continuous <| Integration.RungeKutta.rk4
          OptimiseWith = Optimisation.Amoeba.swarm 5 10 Optimisation.Amoeba.Solver.Default
          LogTo = Console.logger 1000<iteration>
          Random = MathNet.Numerics.Random.MersenneTwister true
          ToModelTime = DateMode.Conversion.CalendarDates.toYears
          InterpolationGlobal = Solver.InterpolationMode.Lower
          InterpolationPerVariable = Map.empty
          Conditioning = Conditioning.RepeatFirstDataPoint }


    /// <summary>Substitute a specific logger into</summary>
    /// <param name="out"></param>
    /// <param name="engine"></param>
    /// <typeparam name="'a"></typeparam>
    /// <typeparam name="'b"></typeparam>
    /// <returns></returns>
    let withOutput out engine = { engine with LogTo = out }

    let withTimeConversion<'d, 'd2, 'timespan, 'timespan2, [<Measure>] 'modelTimeUnit, 'o1, [<Measure>] 'o2, [<Measure>] 'u>
        (fn: DateMode.Conversion.ResolutionToModelUnits<'d2, 'timespan2, 'modelTimeUnit>)
        (engine: EstimationEngine<'d, 'o1, 'o2, 'u>)
        : EstimationEngine<'d2, 'timespan2, 'modelTimeUnit, 'u> =
        { TimeHandling = engine.TimeHandling
          OptimiseWith = engine.OptimiseWith
          LogTo = engine.LogTo
          Random = engine.Random
          ToModelTime = fn
          InterpolationGlobal = engine.InterpolationGlobal
          InterpolationPerVariable = engine.InterpolationPerVariable
          Conditioning = engine.Conditioning }

    let forDailyModel engine =
        withTimeConversion DateMode.Conversion.CalendarDates.toDays engine

    let forMonthlyModel engine =
        withTimeConversion DateMode.Conversion.CalendarDates.toMonths engine

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

    let withBristleconeOptimiser engine =
        { engine with
            OptimiseWith = Optimisation.MonteCarlo.bristleconeSampler }

    let withGradientDescent engine =
        { engine with
            OptimiseWith = Optimisation.Amoeba.single Optimisation.Amoeba.Solver.Default }

    let withCustomOptimisation optim engine = { engine with OptimiseWith = optim }

    module internal Fit =

        /// Places a map of `TimeSeries` into a `TimeFrame` that has data
        /// that shares a common timeline. If no timeline is shared, returns
        /// an `Error`.
        let observationsToCommonTimeFrame (dynamicEquationKeys: ShortCode.ShortCode seq) timeSeriesData =
            timeSeriesData
            |> Map.map (fun _ v ->
                v
                |> TimeSeries.map (fun (v, _) -> v |> Units.removeUnitFromFloat |> (*) 1.<state>))
            |> Map.filter (fun k _ -> dynamicEquationKeys |> Seq.contains k)
            |> TimeFrame.tryCreate
            |> Result.ofOption "Observations for dynamic variables must share a common sampling time sequence"

        /// Finds environmental data in a timeseries map (i.e. those datasets that are not
        /// dynamic variables or measures), and constructs a common `TimeFrame`.
        /// Returns None if there is no environmental data.
        let environmentDataToCommonTimeFrame dynamicVariableKeys measureKeys timeSeriesData =
            let environmentSeries =
                timeSeriesData
                |> Map.map (fun _ v ->
                    v
                    |> TimeSeries.map (fun (v, _) -> v |> Units.removeUnitFromFloat |> (*) 1.<environment>))
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

        /// Determines which interpolation scheme to use for a particular
        /// environmental time-series, for cases when the exact time is not
        /// present.
        let interpolationFor engine key =
            engine.InterpolationPerVariable
            |> Map.tryFind key
            |> Option.defaultValue engine.InterpolationGlobal


    // Temporary helpers until optim-space-transformed can be handled correctly:
    let private unsafeEraseSpace<'space> =
        unbox<Parameter.Pool.OptimiserConfig<``optim-space``>>

    let private unsafeEraseSpaceForward<'space> =
        unbox<Tensors.TypedTensor<Tensors.Vector, ``optim-space-transformed``>>

    let private dynamicVariableKeys (models: ModelSystem.ModelForm<'modelTimeUnit>) =
        match models with
        | ModelForm.DifferenceEqs eqs -> eqs |> Map.keys
        | ModelForm.DifferentialEqs eqs -> eqs |> Map.keys

    /// Convert a list of optimiser-space solutions into real-space solutions
    let internal toRealSpaceSolutions
        (config: Parameter.Pool.AnyOptimiserConfig)
        (solutions: Solution list)
        : (float<``-logL``> * float<``parameter``>[]) list =

        match config with
        | Parameter.Pool.DetachedConfig cfg ->
            solutions
            |> List.map (fun (ll, pointOptSpace) ->
                let realVec = cfg.Compiled.Forward pointOptSpace
                ll, realVec |> Tensors.Typed.toFloatArray)

        | Parameter.Pool.TransformedConfig cfg ->
            solutions
            |> List.map (fun (ll, pointOptSpace) ->
                let realVec = cfg.Compiled.Forward(unsafeEraseSpaceForward pointOptSpace)
                ll, realVec |> Tensors.Typed.toFloatArray)

    let internal validateEnvData expectedKeys actualMap =
        let expectedSet = expectedKeys |> Set.ofList
        let actualSet = actualMap |> Map.keys |> Set.ofSeq

        if Set.isSubset expectedSet actualSet then
            Ok()
        else
            Error
            <| sprintf
                "Environment data keys do not match model definition. Expected %A but provided with %A."
                expectedKeys
                (Map.keys actualMap)

    let internal validateConditionedStartData
        (conditioned: Solver.Conditioning.Resolved<_, _, _>)
        (initialisers: CodedMap<Initialiser<state>>)
        equationKeys
        =
        let conditionedStates =
            Seq.concat
                [ Map.keys conditioned.StatesHiddenForSolver
                  conditioned.StatesObservedForSolver.Keys
                  initialisers.Keys ]

        let missing = Set.difference (Set.ofSeq equationKeys) (Set.ofSeq conditionedStates)

        if missing.IsEmpty then
            Ok()
        else
            Error
            <| sprintf
                "Some states were not conditioned at t0: %A. If these are hidden states, a custom start may be required."
                missing

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
    let tryFit
        (engine: EstimationEngine<'date, 'timespan, 'modelTimeUnit, 'stateUnit>)
        endCondition
        (observedSeries: CodedMap<TimeSeries<float<'stateUnit>, 'date, 'yearType, 'timespan>>)
        (model: ModelSystem<'modelTimeUnit>)
        =

        let resultId = Guid.NewGuid()

        let requiredByLikelihood =
            model.NegLogLikelihood.RequiredCodes
            |> List.map (fun c ->
                match c with
                | Measure c -> c
                | State c -> c)

        // TODO Ensure that ODEs + measures actually produce the states required by likelihood function.

        let requireObservedForLikelihood
            : Result<CodedMap<TimeSeries<float<'stateUnit>, 'date, 'yearType, 'timespan>>, string> =
            if model.NegLogLikelihood.RequiredCodes.IsEmpty then
                Error "The set likelihood function required no data"
            else if observedSeries.IsEmpty then
                Error "No time-series data was specified"
            else if Set.isSubset (requiredByLikelihood |> set) (observedSeries |> Map.keys |> set) then
                Ok observedSeries
            else
                Error(
                    sprintf
                        "Required time-series data were missing. Need: %A"
                        (requiredByLikelihood |> Seq.map (fun k -> k.Value) |> String.concat " + ")
                )

        result {

            // 1. Check required dynamic time-series are present
            let! tsData = requireObservedForLikelihood

            // 2. Build common timeline time-frames
            let! observedOnCommonTimeline = Fit.observationsToCommonTimeFrame requiredByLikelihood observedSeries

            let! exogenousOnCommonTimeline =
                Fit.environmentDataToCommonTimeFrame requiredByLikelihood (Map.keys model.Measures) observedSeries

            do!
                validateEnvData
                    model.EnvironmentKeys
                    (exogenousOnCommonTimeline
                     |> Option.map (fun tf -> tf.Series)
                     |> Option.defaultValue Map.empty)

            // 3. Resolve time-series given requested conditioning of t0.
            let equationKeys = dynamicVariableKeys model.Equations

            let conditioned =
                Solver.Conditioning.resolve
                    engine.Conditioning
                    observedOnCommonTimeline
                    exogenousOnCommonTimeline
                    equationKeys
                    (Map.keys model.Measures)

            conditioned.Log |> Option.iter (GeneralEvent >> engine.LogTo)

            // Check that conditioned contains t0 all required states
            do! validateConditionedStartData conditioned model.Initialisers equationKeys

            engine.LogTo
            <| GeneralEvent(
                sprintf
                    "Time-series (conditioned) start at %A with resolution %A."
                    conditioned.StatesObservedForSolver.StartDate
                    (conditioned.StatesObservedForSolver |> TimeFrame.resolution)
            )

            // Log out variables
            engine.LogTo
            <| GeneralEvent(
                sprintf
                    "Observed states: %A. Hidden states: %A. Used in likelihood: %A\nExogeneous time-series: %A"
                    conditioned.StatesObservedForSolver.Keys
                    (Map.keys conditioned.StatesHiddenForSolver)
                    conditioned.ObservedForPairing.Keys
                    (conditioned.ExogenousForSolver |> Option.map (fun t -> t.Keys))
            )

            observedOnCommonTimeline.Series
            |> Map.iter (fun k v ->
                engine.LogTo
                <| GeneralEvent(sprintf "Common timeline (states and measures): %s (start at %A)" k.Value v.StartDate))

            exogenousOnCommonTimeline
            |> Option.iter (fun c ->
                c.Series
                |> Map.iter (fun k v ->
                    engine.LogTo
                    <| GeneralEvent(sprintf "Common timeline (environment): %s (start at %A)" k.Value v.StartDate)))

            // TODO Ensure that dynamic variables are an exact or subset of environmental variables
            let! _ = Fit.exactSubsetOf observedOnCommonTimeline exogenousOnCommonTimeline

            // 4. Compile solver (auto‑selects discrete/differential)
            let solver stepType =
                Solver.SolverCompiler.compile
                    engine.LogTo
                    engine.ToModelTime
                    model.Equations
                    engine.TimeHandling
                    stepType
                    conditioned.StatesObservedForSolver
                    conditioned.MeasuresForSolver
                    conditioned.StatesHiddenForSolver
                    model.Initialisers
                    conditioned.ExogenousForSolver
                    (Fit.interpolationFor engine)

            // A centralised transform for parameters between point and parameter space.
            let optimConfig =
                match engine.OptimiseWith with
                | Optimisation.InDetachedSpace _ ->
                    Parameter.Pool.DetachedConfig(Parameter.Pool.toOptimiserConfigBounded model.Parameters)
                | Optimisation.InTransformedSpace _ ->
                    Parameter.Pool.TransformedConfig(Parameter.Pool.toOptimiserConfigTransformed model.Parameters)

            let optimise =
                match engine.OptimiseWith, optimConfig with
                | Optimisation.InDetachedSpace optim, Parameter.Pool.DetachedConfig cfg ->
                    optim engine.Random engine.LogTo endCondition cfg.Domain None
                | Optimisation.InTransformedSpace optim, Parameter.Pool.TransformedConfig cfg ->
                    optim engine.Random engine.LogTo endCondition (unsafeEraseSpace cfg).Domain None
                | _ -> invalidOp "Mode/config mismatch"

            let obsDataForObjective =
                Solver.Conditioning.toObservationData conditioned.ObservedForPairing

            engine.LogTo
            <| GeneralEvent(sprintf "Time-series used within objective: %A." (Map.keys obsDataForObjective))

            let obsTimes = conditioned.ObservedForPairing |> TimeFrame.dates

            let objective =
                Objective.create
                    model.NegLogLikelihood
                    model.Measures
                    (solver (Solver.StepType.External obsTimes))
                    optimConfig
                    obsDataForObjective

            let result = objective |> optimise

            let lowestLikelihood, bestPoint =
                match result |> Optimisation.Optimiser.tryGetSolution with
                | Some sol -> sol
                | None -> failwith "The optimisation algorithm did not return any results."

            let estimatedSeries =
                Objective.createPredictor
                    model.Measures
                    (solver (Solver.StepType.External obsTimes))
                    optimConfig
                    bestPoint

            let estimatedHighRes =
                Objective.createPredictor model.Measures (solver Solver.StepType.Internal) optimConfig bestPoint

            let paired =
                conditioned.ObservedForPairing.Series
                |> Map.filter (fun key _ -> estimatedSeries |> Map.containsKey key)
                |> Map.map (fun k observedSeries ->
                    let expected = estimatedSeries |> Map.find k |> Tensors.Typed.toFloatArray

                    observedSeries
                    |> TimeSeries.toObservations
                    |> Seq.zip expected
                    |> Seq.map (fun (e, (o, d)) ->
                        ({ Obs = o |> Units.removeUnitFromFloat |> (*) 1.<state>
                           Fit = e },
                         d))
                    |> TimeSeries.fromObservations observedSeries.DateMode)

            let bestPoint =
                match optimConfig with
                | Parameter.Pool.DetachedConfig o -> o.Compiled.Forward bestPoint
                | Parameter.Pool.TransformedConfig o -> o.Compiled.Forward(unbox bestPoint) // TODO Remove unbox

            let bestPointPool = Parameter.Pool.fromRealVector bestPoint model.Parameters

            let estimatedHighResFloat =
                estimatedHighRes |> Map.map (fun _ v -> v |> Tensors.Typed.toFloatArray) |> Some

            engine.LogTo CompleteEvent

            let trace =
                result
                |> List.map (fun r ->
                    { ComponentName = r.Component
                      StageName = r.Stage
                      ReplicateNumber = r.Replicate
                      Results = r.Results |> toRealSpaceSolutions optimConfig })

            return
                { ResultId = resultId
                  Likelihood = lowestLikelihood
                  Parameters = bestPointPool
                  Series = paired
                  Trace = trace
                  InternalDynamics = estimatedHighResFloat }
        }

    /// <summary>Fit a time-series model to data.</summary>
    /// <param name="engine">An estimation engine configured and tested for the given model.</param>
    /// <param name="endCondition">The condition at which optimisation should cease.</param>
    /// <param name="timeSeriesData">Time-series dataset that contains a series for each equation in the model system.</param>
    /// <param name="model">A model system of equations, likelihood function, estimatible parameters, and optional measures.</param>
    /// <returns>The result of the model-fitting procedure. If an error occurs, throws an exception.</returns>
    let fit engine endCondition timeSeriesData (model: ModelSystem<'modelTimeUnit>) =
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
    let tryTestModel
        (engine: EstimationEngine<'date, 'timespan, 'modelTimeUnit, 'state>)
        endCondition
        (settings: Test.TestSettings<'state, 'date, 'timeunit, 'timespan>)
        (model: ModelSystem<'modelTimeUnit>)
        =

        engine.LogTo <| GeneralEvent "Attempting to generate parameter set."

        engine.LogTo
        <| GeneralEvent(
            sprintf
                "The data must comply with %i rules after %i tries."
                settings.GenerationRules.Length
                settings.RetryDataGen
        )

        result {

            // Validate settings and generate synthetic data
            let! settings = Test.isValidSettings model settings

            // Set conditioning for solver to be the custom start values
            let engine = engine |> withConditioning (Conditioning.Custom settings.StartValues)
            let! trueData, trueParamPool = Test.Compute.tryGenerateData engine settings model settings.RetryDataGen

            // Merge dynamic + environmental data into one coded map
            let mergedData = Map.merge trueData settings.EnvironmentalData (fun dyn _env -> dyn)

            engine.LogTo <| GeneralEvent(sprintf "Dataset is %A" mergedData)

            engine.LogTo
            <| GeneralEvent(sprintf "Parameters to test are %A" (trueParamPool |> Parameter.Pool.toTensorWithKeysReal))

            // Fit with true parameters (no optimisation)
            let! realEstimate =
                tryFit
                    { engine with
                        OptimiseWith = Optimisation.None.none }
                    endCondition
                    mergedData
                    { model with
                        Parameters = Parameter.Pool.fromEstimated trueParamPool }

            // Fit normally (optimisation enabled)
            let! estimated = tryFit engine endCondition mergedData model

            let paramDiffs: Test.ParameterTestResult list =
                estimated.Parameters
                |> Parameter.Pool.toList
                |> List.map (fun (sc, _) ->
                    match
                        Parameter.Pool.tryGetRealValue sc.Value estimated.Parameters,
                        Parameter.Pool.tryGetRealValue sc.Value trueParamPool
                    with
                    | Some e, Some r ->
                        { Identifier = sc.Value
                          RealValue = r
                          EstimatedValue = e }
                    | _ -> failwithf "Missing estimate for parameter %s" sc.Value)

            // Per-series squared error
            let errorStructure =
                let squared (x: float<state>) = x * x

                estimated.Series
                |> Seq.map (fun kv ->
                    let key = kv.Key.Value
                    let errs = kv.Value.Values |> Seq.map (fun t -> t.Fit - t.Obs |> squared)
                    key, errs)
                |> Map.ofSeq

            engine.LogTo CompleteEvent

            return
                { ErrorStructure = errorStructure
                  IterationsRun = (estimated.Trace |> List.sumBy (fun i -> i.Results.Length)) * 1<iteration>
                  Parameters = paramDiffs
                  Series = estimated.Series |> Seq.map (fun k -> k.Key.Value, k.Value) |> Map.ofSeq
                  RealLikelihood = realEstimate.Likelihood
                  EstimatedLikelihood = estimated.Likelihood
                  Trace = estimated.Trace }
        }

    /// <summary>Test that the specified estimation engine can correctly
    /// estimate known parameters. Random parameter sets are generated
    /// from the given model system.</summary>
    /// <param name="engine">An estimation engine containing the method used for model-fitting.</param>
    /// <param name="settings">Test settings that define how the test will be conducted.</param>
    /// <param name="model">The model system to test against the estimation engine.</param>
    /// <returns>A test result that indicates differences between the expected and actual fit.</returns>
    let testModel
        (engine: EstimationEngine<'date, 'timespan, 'modelTimeUnit, 'state>)
        endCondition
        (settings: TestSettings<'state, 'date, 'yearUnit, 'timespan>)
        (model: ModelSystem<'modelTimeUnit>)
        =
        tryTestModel engine endCondition settings model |> Result.forceOk

    /// <summary>Repeat a model fit many times, removing a single data point at random each time.</summary>
    /// <param name="engine">The estimation engine / fitting method</param>
    /// <param name="endCondition">The end condition for each model fit</param>
    /// <param name="bootstrapCount">Number of times to bootstrap data</param>
    /// <param name="model">A model system / hypothesis to fit</param>
    /// <param name="series">Time-series to fit with model</param>
    /// <returns>A list of estimation results (one for each bootstrap) for further analysis</returns>
    let bootstrap
        (engine: EstimationEngine.EstimationEngine<'date, 'timespan, 'modelTimeUnit, 1>)
        (endCondition: EndCondition)
        bootstrapCount
        (model: ModelSystem<'modelTimeUnit>)
        (series: Map<ShortCode.ShortCode, TimeSeries.TimeSeries<float, 'date, 'timeunit, 'timespan>>)
        =
        let rec bootstrap
            (s: Map<ShortCode.ShortCode, TimeSeries.TimeSeries<float, 'date, 'timeunit, 'timespan>>)
            numberOfTimes
            solutions
            =
            if numberOfTimes > 0 then
                let resolution = series |> Seq.head |> (fun s -> s.Value |> TimeSeries.resolution)

                let stepping =
                    match resolution with
                    | Resolution.Variable -> failwith "Cannot boostrap variable resolution data"
                    | Resolution.Fixed f -> f

                let subset = TimeSeries.Bootstrap.removeSingle engine.Random stepping s
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
        (hypothesis: ModelSystem<'modelTimeUnit>)
        preTransform
        (timeSeries: Map<ShortCode.ShortCode, TimeSeries.TimeSeries<float, 'date, 'timeunit, 'timespan>>)
        estimatedTheta
        =

        let hypothesisMle =
            { hypothesis with
                Parameters = Parameter.Pool.fromEstimated estimatedTheta }

        let dateMode = (timeSeries |> Seq.head).Value.DateMode

        // Helper: fit to first point, predict second
        let predictNext dataset =
            let est =
                fit
                    (engine
                     |> withCustomOptimisation Optimisation.None.none
                     |> withConditioning Conditioning.RepeatFirstDataPoint)
                    (Optimisation.EndConditions.atIteration 0<iteration>)
                    dataset
                    hypothesisMle

            let nextObs =
                dataset
                |> Map.map (fun _ ts -> ts |> TimeSeries.toObservations |> Seq.skip 1 |> Seq.head)

            nextObs
            |> Seq.map (fun kv ->
                let nextFit = est.Series.[kv.Key].Values |> Seq.head |> (fun p -> p.Fit)

                kv.Key,
                { Obs = kv.Value |> fst |> (*) 1.<state>
                  Fit = nextFit },
                kv.Value |> snd)

        let squared (x: float<state>) = x * x

        // Stream over each time step, building 2‑point datasets on the fly
        timeSeries
        |> Seq.collect (fun (KeyValue(_, ts)) ->
            ts
            |> TimeSeries.toObservations
            |> Seq.pairwise
            |> Seq.mapi (fun i (t1, t2) ->
                // Build a mini‑series for all variables at this index
                let dataset =
                    timeSeries
                    |> Map.map (fun _ fullTs ->
                        fullTs
                        |> TimeSeries.toObservations
                        |> Seq.skip i
                        |> Seq.truncate 2
                        |> TimeSeries.fromObservations fullTs.DateMode)
                    |> preTransform

                dataset))
        |> Seq.collect predictNext
        |> Seq.groupBy (fun (k, _, _) -> k)
        |> Seq.map (fun (name, triples) ->
            let rmse =
                triples |> Seq.averageBy (fun (_, v, _) -> squared (v.Obs - v.Fit)) |> sqrt

            name,
            (triples
             |> Seq.map (fun (_, v, t) -> v, t)
             |> TimeSeries.fromObservations dateMode,
             { RMSE = rmse |> Units.removeUnitFromFloat }))
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
