namespace Bristlecone

open System
open Bristlecone.Logging

/// Configures a single function that represents a model and its likelihood
/// when fit to time-series data.
module Objective =

    open Bristlecone.Optimisation
    open ModelSystem

    let parameteriseModel parameterPool point (model:ModelEquation) =
        model (point |> ParameterPool.toParamList parameterPool)

    /// Pairs observed time series to predicted series for dynamic variables only.
    /// Environmental forcings and hidden variables are removed.
    let pairObservationsToExpected (observed:CodedMap<float[]>) (expected:CodedMap<float[]>) : CodedMap<PredictedSeries> =
        observed
        |> Map.filter(fun key _ -> expected |> Map.containsKey key)
        |> Map.map (fun key value ->
            let r = { Observed = value; Expected = expected |> Map.find key }
            if r.Observed.Length = r.Expected.Length then r else invalidOp (sprintf "The predicted series %s was a different length to the observed series" key.Value))

    /// The system's `Measures` are computed from the product of the solver.
    let measure (system:ModelSystem) solveDiscrete (expected:CodedMap<float[]>) : CodedMap<float[]> =
        system.Measures
        |> Map.map (fun key measure -> solveDiscrete key measure expected)
        |> Map.fold (fun acc key value -> Map.add key value acc) expected

    let predict (system:ModelSystem) integrate solveDiscrete (p:Point<float>) =
        system.Equations
        |> Map.map (fun _ v -> parameteriseModel system.Parameters p v)
        |> integrate
        |> measure system solveDiscrete

    /// Computes measurement variables and appends to expected data
    let create (system:ModelSystem) integrate solveDiscrete (observed:CodedMap<float[]>) (p:Point<float>) =
        p
        |> predict system integrate solveDiscrete
        |> pairObservationsToExpected observed
        |> system.Likelihood (p |> ParameterPool.toParamList system.Parameters)


/// Conditioning of time-series data, which allows for maximum use of observed time-series data.
module Conditioning =

    /// Strategy for assigning a start time - `t0` - to a time series.  
    let startPoint conditioning (series:CodedMap<TimeSeries<'a>>) =
        match conditioning with
        | NoConditioning -> None
        | RepeatFirstDataPoint -> series |> Map.map(fun _ v -> v.Values |> Seq.head) |> Some
        | Custom precomputed -> precomputed |> Some


/// Helper functions for the creation of `Solver` functions, which apply time-series models
/// to time-series data (when using Bristlecone time-series types).
module Solver =

    open Bristlecone.EstimationEngine

    type Solver<'a> = CodedMap<ODE> -> CodedMap<'a[]>

    type StepType =
        | Internal
        | External

    /// Step the solver using the high resolution, and output at low resolution.
    /// External steps can be variable in size.
    let variableExternalStep engine forcings eqs =
        match engine.TimeHandling with
        | Discrete -> invalidOp "Not configured"
        | Continuous i ->
            fun state t0 t1 ->
                i engine.LogTo t0 t1 (t1-t0) state forcings eqs

    /// Step the solver using high resolution, and output at low resolution.
    /// External steps are of a fixed width.
    let fixedStep engine tStart tEnd initialState forcings =
        match engine.TimeHandling with
        | Discrete -> invalidOp "Not configured"
        | Continuous i -> fun eqs -> 
            i engine.LogTo tStart tEnd 1. initialState forcings eqs
            |> Map.map (fun _ v -> v |> Array.tail |> Seq.toArray)

    let solver stepType dStart dRes (dData:CodedMap<TimeSeries<'a>>) eData engine t0 : Solver<'a> =

        /// Use variable or fixed stepping depending on data
        match dRes with
        | Fixed fRes ->

            /// Integration runs on time centered on dynamic data start time.
            /// Integration runs on environmental data resolution.
            let timeline,forcings =
                match eData with
                | Some (e,_,eRes) -> 

                    /// Environmental data may be variable or fixed
                    /// We don't support variable environmental data, so fail
                    match eRes with
                    | Variable -> failwith "Variable-time environmental forcing data is not supported"
                    | Fixed efRes ->
                        let timeline = (dData |> Seq.head).Value |> TimeIndex.indexSeries dStart efRes |> Seq.map fst
                        let envIndex = e |> Map.map(fun k v -> TimeIndex.TimeIndex(dStart, efRes, TimeIndex.IndexMode.Interpolate Statistics.Interpolate.lower, v)) //TimeIndex.IndexMode.Interpolate Statistics.Interpolate.bilinear, v))
                        (timeline, envIndex)
                | None -> 
                    let timeline = (dData |> Seq.head).Value |> TimeIndex.indexSeries dStart fRes |> Seq.map fst
                    (timeline, Map.empty)

            engine.LogTo <| GeneralEvent (sprintf "Applying a fixed-resolution solver (%A)." fRes) 
            engine.LogTo <| GeneralEvent (sprintf "Data falls on a common timeline: %A" (timeline |> Seq.toList))
            engine.LogTo <| GeneralEvent (sprintf "Dynamic data is: %A" dData)
            engine.LogTo <| GeneralEvent (sprintf "Original E data was %A" eData)
            engine.LogTo <| GeneralEvent (sprintf "Forcings are %A" (forcings |> Map.map(fun k v -> v.Values)))

            /// Parameters for the integration routine
            let startIndex = timeline |> Seq.head
            let endIndex = timeline |> Seq.last
            // Internal step size is 1, given that the time-index is scaled to the steps of the high-resolution external data
            let externalStep = (timeline |> Seq.tail |> Seq.head) - (timeline |> Seq.head) //TODO Figure out how to do this properly
            engine.LogTo <| GeneralEvent (sprintf "External step size is %f" externalStep)
            /// Condition data by setting a 'conditioned' starting state. 
            /// This is -1 position on the common timeline
            /// NB. The environmental data must span the conditioning period - how to ensure this?

            /// Run the integration as one operation from conditioned t0 to tn
            /// Filter the results so that only results that match low-res data in time are included
            let solve = fixedStep engine (startIndex - externalStep) endIndex t0 forcings
            fun ode -> 
                match stepType with
                | Internal -> solve ode
                | External ->
                    solve ode
                    |> Map.map (fun _ v -> v |> Seq.everyNth (int externalStep) |> Seq.toArray) //TODO proper lookup

        | Variable ->
            // Run time as individual steps, to allow length to vary (slower)
            // Allow environmental data when on same temporal profile
            match eData with
            | Some (e,eDate,eRes) -> invalidOp "Variable time solver with environmental data has not been implemented yet."
            | None -> invalidOp "I've yet to finish the variable-time solver"


    module Discrete =

        /// A continuation representing a transform of pre-computed data
        /// float = previous state
        type Measurement<'a> = 'a -> Bristlecone.ModelSystem.Environment -> Bristlecone.ModelSystem.Environment -> 'a

        /// Finds the solution of a discretised model system, given time-series data.
        /// `startPoint` - a preconditioned point to represent time-zero.
        let solve startPoint : ShortCode -> Measurement<'a> -> CodedMap<'a[]> -> 'a[] =
            fun (c:ShortCode) (m:Measurement<'a>) (expected:CodedMap<'a[]>) ->
                expected
                |> Map.toList
                |> List.map(fun (c,d) -> d |> Array.map(fun x -> (c, x)) |> Seq.toList) // Add shortcode to each time point's value
                |> Bristlecone.List.flip // Flip list so that it is primarily timepoint-indexed
                |> List.map Map.ofList // Make environment at each time into a CodedMap<float>
                // Scan through the environments, outputting the measure at each time
                |> List.scan(fun (previousEnv,previousX) currentEnv -> 
                    (currentEnv, m previousX previousEnv currentEnv) ) (startPoint, startPoint.Item c)
                |> List.tail    // Remove conditioned point (time zero)
                |> List.map snd
                |> List.toArray


module Bristlecone =

    open Time
    open ModelSystem
    open EstimationEngine
    open Bristlecone.Optimisation

    /// A standard estimation engine using a random-walk monte carlo optimiser.
    let mkDiscrete : EstimationEngine<float,float> = {
        TimeHandling = Discrete
        OptimiseWith = Optimisation.MonteCarlo.randomWalk []
        LogTo = Bristlecone.Logging.Console.logger(1000)
        Constrain = ConstraintMode.Detached
        Random = MathNet.Numerics.Random.MersenneTwister(true)
        Conditioning = NoConditioning }

    /// A standard `EstimationEngine` for ordinary differential equation models.
    let mkContinuous = {
        TimeHandling = Continuous <| Integration.MathNet.integrate
        OptimiseWith = Optimisation.MonteCarlo.randomWalk []
        LogTo = Bristlecone.Logging.Console.logger(1000)
        Constrain = ConstraintMode.Detached
        Random = MathNet.Numerics.Random.MersenneTwister(true)
        Conditioning = RepeatFirstDataPoint }

    /// Add a writer
    let withOutput out engine =
        { engine with LogTo = out }

    /// Use a custom integration method
    let withContinuousTime t engine =
        { engine with TimeHandling = Continuous t }

    /// Choose how the start point is chosen when solving the model system
    let withConditioning c engine =
        { engine with Conditioning = c }

    let withTunedMCMC tuning engine =
        { engine with OptimiseWith = Optimisation.MonteCarlo.randomWalk tuning }

    let withGradientDescent engine =
        { engine with OptimiseWith = Optimisation.Amoeba.Solver.solve Optimisation.Amoeba.Solver.Default }

    let withCustomOptimisation optim engine =
        { engine with OptimiseWith = optim }

    type TimeSeriesNature =
        | Measure
        | Dynamic
        | Forcing

    /// **Description**
    /// Fit a mathematical model to data. 
    /// **Parameters**
    ///   * `engine` - parameter of type `EstimationEngine<float,float>`
    ///   * `model` - parameter of type `ModelSystem`
    ///   * `timeSeriesData` - parameter of type `Map<ShortCode,TimeSeries<float>>`
    ///   * `iterations` - parameter of type `int`
    ///   * `burnin` - parameter of type `int`
    ///
    /// **Output Type**
    ///   * `EstimationResult`
    ///
    /// **Exceptions**
    ///
    let fit engine endCondition timeSeriesData (model:ModelSystem) =

        // 1. Parse model to setup time series data and resolution
        // Determine if each series is (a) measure, (b) dynamic, or (c) forcing
        // Determine dynamic data time resolution
        // Determine forcing data time resolution
        // Fail if time resolutions are not subsets

        // Temporal Resolution
        let dynamicVariableKeys = model.Equations |> Seq.map (fun k -> k.Key)
        let dData,dStart,dRes =
            let data =
                timeSeriesData
                |> Map.filter(fun k _ -> dynamicVariableKeys |> Seq.contains k)
            // printfn "Dynamic data = %A" data
            let resolutions =
                data
                |> TimeSeries.validateCommonTimeline
                |> Seq.map(fun k -> (k.Value.StartDate |> snd, k.Value.Resolution))
                |> Seq.distinct
            if resolutions |> Seq.length <> 1 
            then invalidArg "timeSeriesData" "Observations for dynamic variables must share a common sampling time sequence"
            else (data, resolutions |> Seq.head |> fst, resolutions |> Seq.head |> snd)

        // Ensure that environmental data - if any - are specified on a common timeline
        let measureKeys = model.Measures |> Seq.map (fun k -> k.Key)
        let eData =
            let eData = timeSeriesData |> Map.filter(fun k _ -> dynamicVariableKeys |> Seq.append measureKeys |> Seq.contains k |> not)
            // printfn "Environment data = %A" eData
            if eData.Count = 0 then None
            else
                let resolutions =
                    eData
                    |> TimeSeries.validateCommonTimeline
                    |> Seq.map(fun k -> (k.Value.StartDate |> snd, k.Value.Resolution))
                    |> Seq.distinct
                if resolutions |> Seq.length <> 1 
                then invalidArg "timeSeriesData" "Forcing variables were specified on differing timelines. Pre-process your data to make environmental data conform to a common temporal baseline and time interval"
                else (eData, resolutions |> Seq.head |> fst, resolutions |> Seq.head |> snd) |> Some

        // Ensure that dynamic variables is an exact or subset of environmental variables
        // match environmentVariableResolution with
        // | None -> ()
        // | Some eRes -> 

        let data = (timeSeriesData |> Map.map (fun _ ts -> ts.Values |> Seq.toArray))

        /// Condition initial time point
        let conditionedPoint = 
            let x = Conditioning.startPoint engine.Conditioning timeSeriesData
            match x with
            | Some y -> y
            | None -> invalidOp "Not supported"

        // B. Create a solver that outputs float[] containing only the values
        // for the dynamic variable resolution
        let solver outputStep = Solver.solver outputStep dStart dRes dData eData engine conditionedPoint
        let discreteSolve = Solver.Discrete.solve conditionedPoint

        // A. Setup constraints on parameters, depending on optimisation requirements
        let constrainedParameters, optimisationConstraints = 
            match engine.Constrain with
            | Transform -> (model.Parameters, [1 .. model.Parameters.Count] |> List.map(fun _ -> Unconstrained))
            | Detached -> 
                let par,con = 
                    model.Parameters 
                    |> Map.map (fun k v -> detatchConstraint v) 
                    |> Map.toList 
                    |> List.map (fun (k,(x,y)) -> ((k, x), y))
                    |> List.unzip
                (par |> Map.ofList, con)

        let objective = Objective.create { model with Parameters = constrainedParameters } (solver Solver.StepType.External) discreteSolve data

        let optimise = engine.OptimiseWith engine.Random engine.LogTo endCondition (constrainedParameters |> ParameterPool.toDomain optimisationConstraints)
        let result = objective |> optimise
        let lowestLikelihood, bestPoint = result |> List.minBy (fun (l,_) -> l)

        let estimatedSeries = Objective.predict { model with Parameters = constrainedParameters } (solver Solver.StepType.External) discreteSolve bestPoint
        let paired = 
            timeSeriesData 
            |> Map.filter(fun key _ -> estimatedSeries |> Map.containsKey key)
            |> Map.map (fun k observedSeries ->
                let expected = estimatedSeries |> Map.find k
                observedSeries
                |> TimeSeries.toObservations
                |> Seq.zip expected
                |> Seq.map(fun (e,(o,d)) -> ({ Obs = o; Fit = e }, d))
                |> TimeSeries.fromObservations )

        let estimatedHighRes = Objective.predict { model with Parameters = constrainedParameters } (solver Solver.StepType.Internal) discreteSolve bestPoint

        { ResultId   = System.Guid.NewGuid()
          Likelihood = lowestLikelihood
          Parameters = bestPoint |> ParameterPool.fromPoint constrainedParameters
          Series     = paired
          Trace      = result }, estimatedHighRes

    /// Functions for fitting models that do not include a temporal dimension.
    module Invariant =

        /// Fit a time-invariant model.
        let fitWithoutTime optimise constrain log endCondition random (data:CodedMap<float>) (model:ModelSystem) =
            let engine = {
                TimeHandling = Discrete
                OptimiseWith = optimise
                Conditioning = NoConditioning
                Constrain = constrain
                Random = random
                LogTo = log
            }
            invalidOp "Not implemented"
            //let ts = data |> Map.map(fun _ v -> TimeSeries.fromSeq (v, DateTime.Now))
            //fit engine endCondition ts model


    module Test =

        type GenerationRule = (ShortCode * (seq<float> -> bool))

        type TestSettings<'a> = {
            TimeSeriesLength: int
            StartValues: CodedMap<'a>
            EndCondition: EndCondition<'a>
            GenerationRules: GenerationRule list
            NoiseGeneration: ParameterPool -> CodedMap<TimeSeries<'a>> -> CodedMap<TimeSeries<'a>>
            EnvironmentalData: CodedMap<TimeSeries<'a>>
            Resolution: FixedTemporalResolution
            Random: System.Random
            StartDate: DateTime
            Attempts: int
        }

        /// Draw a random set of parameters
        /// TODO Correct handling of constrained parameter values (when drawing parameter sets)

        let drawParameterSet rnd parameters =
            parameters
            |> Map.map (fun _ v -> 
                let lower,upper = Parameter.bounds v
                let trueValue = Statistics.Distributions.ContinuousUniform.draw rnd lower upper ()
                Parameter.setEstimate v trueValue )

        /// Generate a fixed-resolution time-series for testing model fits
        let generateFixedSeries writeOut equations timeMode seriesLength startPoint startDate resolution env theta =
            let applyFakeTime s = TimeSeries.fromSeq startDate resolution s
            let eqs = equations |> Map.map (fun _ v -> v theta)
            match timeMode with
            | Discrete -> invalidOp "Not supported at this time"
            | Continuous i -> 
                i writeOut 0. (seriesLength |> float) 1. startPoint env eqs
                |> Map.map (fun _ v -> applyFakeTime v)

        /// A test procedure for computing measures given time series data.
        let generateMeasures measures startValues (expected:CodedMap<TimeSeries<'a>>) : CodedMap<TimeSeries<'a>> =
            let time = (expected |> Seq.head).Value |> TimeSeries.toObservations |> Seq.map snd
            measures
            |> Map.map (fun key measure -> Solver.Discrete.solve startValues key measure (expected |> Map.map(fun _ t -> t.Values |> Seq.toArray)))
            |> Map.fold (fun acc key value -> Map.add key (time |> Seq.zip value |> TimeSeries.fromObservations) acc) expected

        /// Generate data
        let generateData engine model testSettings theta =
            let envIndex = testSettings.EnvironmentalData |> Map.map(fun k v -> TimeIndex.TimeIndex(testSettings.StartDate, testSettings.Resolution, TimeIndex.IndexMode.Interpolate Statistics.Interpolate.bilinear, v))
            theta
            |> generateFixedSeries engine.LogTo model.Equations engine.TimeHandling testSettings.TimeSeriesLength testSettings.StartValues testSettings.StartDate testSettings.Resolution envIndex
            |> testSettings.NoiseGeneration theta
            |> generateMeasures model.Measures testSettings.StartValues

        /// Check rule conditions
        let rec ruleCheck rules attempts series = 
            let brokeTheRules = 
                rules
                |> List.map(fun (key,ruleFn) -> series |> Map.find key |> TimeSeries.toObservations |> Seq.map fst |> ruleFn)
                |> List.contains false
            if brokeTheRules then
                if attempts = 0 
                then Error "Could not generate data that complies with the given ruleset" 
                else ruleCheck rules (attempts - 1) series
            else Ok series

    /// **Description**
    /// Test that the specified estimation engine can correctly estimate known parameters. Random parameter sets are generated from the given model system.
    /// **Parameters**
    ///   * `model` - a `ModelSystem` of equations and parameters
    ///   * `testSettings` - settings
    ///   * `engine` - an `EstimationEngine`
    let testModel engine (settings:Test.TestSettings<float>) (model:ModelSystem) =
        let theta = Test.drawParameterSet settings.Random model.Parameters
        engine.LogTo <| GeneralEvent (sprintf "The true parameters are: %A" theta)
        let trueData = 
            Test.generateData engine model settings theta 
            |> Test.ruleCheck settings.GenerationRules settings.Attempts
        match trueData with
        | Ok d -> 
            let estimated = fit engine settings.EndCondition (Map.merge d settings.EnvironmentalData (fun x y -> x)) model
            (estimated, trueData, theta) |> Ok
        | Error e -> Error e

    /// **Description**
    /// Repeat a model fit many times, removing a single data point at random each time.
    /// **Parameters**
    ///   * `engine` - parameter of type `EstimationEngine<float,float>`
    ///   * `iterations` - parameter of type `int`
    ///   * `burnin` - parameter of type `int`
    ///   * `bootstrapCount` - parameter of type `int`
    ///   * `hypothesis` - parameter of type `ModelSystem`
    ///   * `identifier` - parameter of type `ShortCode`
    ///   * `series` - parameter of type `CodedMap<TimeSeries<float>>`
    ///
    /// **Output Type**
    ///   * `EstimationResult list`
    ///
    /// **Exceptions**
    ///
    let bootstrap engine random iterations bootstrapCount hypothesis (identifier:ShortCode) series =
        let rec bootstrap s numberOfTimes solutions =
            if (numberOfTimes > 0) then
                let subset = Optimisation.Bootstrap.removeSingle random s
                let result = fit engine iterations subset hypothesis
                engine.LogTo <| GeneralEvent (sprintf "%s: completed bootstrap %i" identifier.Value numberOfTimes)
                bootstrap s (numberOfTimes - 1) (solutions |> List.append [result])
            else solutions
        bootstrap series bootstrapCount []

    /// "How good am I at predicting the next data point"?
    /// 
    let oneStepAhead engine hypothesis (preTransform:CodedMap<TimeSeries<float>>->CodedMap<TimeSeries<float>>) (timeSeries) (estimatedTheta:ParameterPool) =
        let mleToBounds mlePool = mlePool |> Map.map(fun k v -> Parameter.create (Parameter.detatchConstraint v |> snd) (v |> Parameter.getEstimate) (v |> Parameter.getEstimate))
        let hypothesisMle : ModelSystem =  { hypothesis with Parameters = mleToBounds estimatedTheta }
        let pairedDataFrames =
            timeSeries
            |> Map.map(fun _ fitSeries -> 
                fitSeries 
                |> TimeSeries.toObservations 
                |> Seq.pairwise 
                |> Seq.map (fun (t1,t2) -> TimeSeries.fromObservations [t1; t2] |> TimeSeries.map(fun (x,y) -> x )))
        // printfn "Paired data frames = %A" pairedDataFrames
        let timeParcelCount = (pairedDataFrames |> Seq.head).Value |> Seq.length
        // printfn "Time parcels: %i" timeParcelCount
        let data =
            seq { 1 .. timeParcelCount }
            |> Seq.map(fun i -> pairedDataFrames |> Map.map(fun _ v -> v |> Seq.item (i-1)) |> preTransform)
        // printfn "Data: %A" data

        // TODO Remove this hack:
        // First data point is repeated, then skipped when returned
        data
        |> Seq.map (fun d -> fit (engine |> withCustomOptimisation Optimisation.None.passThrough |> withConditioning RepeatFirstDataPoint) (EndConditions.afterIteration 0) d hypothesisMle)
        |> Seq.toList


    module Parallel =

        let fit engine resolution iterations system growth =
            growth |> Array.Parallel.map (fit engine iterations system)


[<AutoOpen>]
module DSL =

    let parameter = Parameter.create

    let code = ShortCode.create

    let lookup (map:CodedMap<float>) name = 
        match map.TryFind (code name) with
        | Some k -> k
        | None -> invalidOp (sprintf "Could not find %s in the map" name)