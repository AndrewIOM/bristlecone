namespace Bristlecone

open System
open Bristlecone.Logging

module Bristlecone =

    open Bristlecone.Time
    open Bristlecone.ModelSystem
    open Bristlecone.EstimationEngine

    /// A standard estimation engine using a random-walk monte carlo optimiser.
    let mkDiscrete : EstimationEngine<float,float> = {
        TimeHandling = Discrete
        OptimiseWith = Optimisation.MonteCarlo.randomWalk []
        LogTo = Console.logger(1000)
        Constrain = Parameter.ConstraintMode.Detached
        Random = MathNet.Numerics.Random.MersenneTwister(true)
        Conditioning = Conditioning.NoConditioning }

    /// A standard `EstimationEngine` for ordinary differential equation models.
    let mkContinuous = {
        TimeHandling = Continuous <| Integration.MathNet.integrate
        OptimiseWith = Optimisation.MonteCarlo.randomWalk []
        LogTo = Bristlecone.Logging.Console.logger(1000)
        Constrain = Parameter.ConstraintMode.Detached
        Random = MathNet.Numerics.Random.MersenneTwister(true)
        Conditioning = Conditioning.RepeatFirstDataPoint }

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
    let fit engine endCondition timeSeriesData (model:ModelSystem) =

        // 1. Validate observed time-series are on common timeline
        let dynamicVariableKeys = model.Equations |> Seq.map (fun k -> k.Key)
        let measureKeys = model.Measures |> Seq.map (fun k -> k.Key)
        let dynamicSeries =
            let data = timeSeriesData |> Map.filter(fun k _ -> dynamicVariableKeys |> Seq.contains k)
            match TimeFrame.tryCreate data with
            | Some f -> f
            | None -> failwith "Observations for dynamic variables must share a common sampling time sequence"

        // 2. Validate forcing time-series are on common timeline
        let eData =
            let eData = timeSeriesData |> Map.filter(fun k _ -> dynamicVariableKeys |> Seq.append measureKeys |> Seq.contains k |> not)
            match eData.Count with
            | 0 -> None
            | _ -> 
                match TimeFrame.tryCreate eData with
                | Some e -> e |> Some
                // TODO Ensure that environment data overlaps with conditioning period?
                | None -> failwith "Forcing variables were specified on differing timelines. Pre-process your data to make environmental data conform to a common temporal baseline and time interval"

        // 3. Validate that dynamic variables is an exact or subset of environmental variables
        // match environmentVariableResolution with
        // | None -> ()
        // | Some eRes -> 

        let data = (timeSeriesData |> Map.map (fun _ ts -> ts.Values |> Seq.toArray))

        /// Condition initial time point
        let conditionedPoint = 
            let x = Solver.Conditioning.startPoint engine.Conditioning timeSeriesData
            match x with
            | Some y -> y
            | None ->
                engine.LogTo <| DebugEvent "No conditioning was specified. Using t1 as conditioning data."
        

            
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


        // B. Create a solver that outputs float[] containing only the values
        // for the dynamic variable resolution
        let solver outputStep = Solver.solver outputStep dynamicSeries eData engine conditionedPoint
        let discreteSolve = Solver.Discrete.solve conditionedPoint

        // A. Setup constraints on parameters, depending on optimisation requirements
        let constrainedParameters, optimisationConstraints = 
            match engine.Constrain with
            | Parameter.Transform -> (model.Parameters, [1 .. Parameter.Pool.count model.Parameters] |> List.map(fun _ -> Parameter.Unconstrained))
            | Parameter.Detached -> 
                let par,con = 
                    model.Parameters
                    |> Parameter.Pool.toList
                    |> List.map (fun (k,v) ->
                        let x,y = Parameter.detatchConstraint v
                        (k, x), y )
                    |> List.unzip
                (par |> Parameter.Pool.fromList, con)

        let objective = Objective.create { model with Parameters = constrainedParameters } (solver Solver.StepType.External) discreteSolve data

        let optimise = engine.OptimiseWith engine.Random engine.LogTo endCondition (constrainedParameters |> Parameter.Pool.toDomain optimisationConstraints)
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

        { ResultId   = Guid.NewGuid()
          Likelihood = lowestLikelihood
          Parameters = bestPoint |> Parameter.Pool.fromPoint constrainedParameters
          Series     = paired
          Trace      = result
          InternalDynamics = Some estimatedHighRes }

    /// Functions for fitting models that do not include a temporal dimension.
    module Invariant =

        /// Fit a time-invariant model.
        let fitWithoutTime optimise constrain log endCondition random (data:CodedMap<float>) (model:ModelSystem) =
            let engine = {
                TimeHandling = Discrete
                OptimiseWith = optimise
                Conditioning = Conditioning.NoConditioning
                Constrain = constrain
                Random = random
                LogTo = log
            }
            invalidOp "Not implemented"
            //let ts = data |> Map.map(fun _ v -> TimeSeries.fromSeq (v, DateTime.Now))
            //fit engine endCondition ts model


    module Test =

        type GenerationRule = (ShortCode.ShortCode * (seq<float> -> bool))

        type TestSettings<'a> = {
            TimeSeriesLength: int
            StartValues: CodedMap<'a>
            EndCondition: EndCondition<'a>
            GenerationRules: GenerationRule list
            NoiseGeneration: Parameter.Pool -> CodedMap<TimeSeries<'a>> -> CodedMap<TimeSeries<'a>>
            EnvironmentalData: CodedMap<TimeSeries<'a>>
            Resolution: FixedTemporalResolution
            Random: System.Random
            StartDate: DateTime
            Attempts: int
        }

        /// Draw a random set of parameters
        /// TODO Correct handling of constrained parameter values (when drawing parameter sets)

        let drawParameterSet rnd pool =
            pool
            |> Parameter.Pool.toList
            |> List.map (fun (k,v) -> 
                let lower,upper = 
                    match Parameter.bounds v with
                    | Some b -> b
                    | None -> failwith "Parameters already estimated"
                let trueValue = Statistics.Distributions.ContinuousUniform.draw rnd lower upper ()
                match Parameter.setTransformedValue v trueValue with
                | Ok p -> k,p
                | Error e -> failwith e )
            |> Parameter.Pool.fromList

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
    let bootstrap engine random iterations bootstrapCount hypothesis (identifier:ShortCode.ShortCode) series =
        let rec bootstrap s numberOfTimes solutions =
            if (numberOfTimes > 0) then
                let subset = Bootstrap.removeSingle random s
                let result = fit engine iterations subset hypothesis
                engine.LogTo <| GeneralEvent (sprintf "%s: completed bootstrap %i" identifier.Value numberOfTimes)
                bootstrap s (numberOfTimes - 1) (solutions |> List.append [result])
            else solutions
        bootstrap series bootstrapCount []

    /// "How good am I at predicting the next data point"?
    /// 
    let oneStepAhead engine hypothesis (preTransform:CodedMap<TimeSeries<float>>->CodedMap<TimeSeries<float>>) (timeSeries) (estimatedTheta:Parameter.Pool) =
        let hypothesisMle : ModelSystem =  { hypothesis with Parameters = Parameter.Pool.fromEstimated estimatedTheta }
        let pairedDataFrames =
            timeSeries
            |> Map.map(fun _ fitSeries -> 
                fitSeries 
                |> TimeSeries.toObservations 
                |> Seq.pairwise 
                |> Seq.map (fun (t1,t2) -> TimeSeries.fromObservations [t1; t2] |> TimeSeries.map(fun (x,y) -> x )))
        let timeParcelCount = (pairedDataFrames |> Seq.head).Value |> Seq.length
        let data =
            seq { 1 .. timeParcelCount }
            |> Seq.map(fun i -> pairedDataFrames |> Map.map(fun _ v -> v |> Seq.item (i-1)) |> preTransform)

        // TODO Remove this hack:
        // First data point is repeated, then skipped when returned
        data
        |> Seq.map (fun d -> fit (engine |> withCustomOptimisation Optimisation.None.passThrough |> withConditioning Conditioning.RepeatFirstDataPoint) (Optimisation.EndConditions.afterIteration 0) d hypothesisMle)
        |> Seq.toList


    module Parallel =

        let fit engine endCondition model growth =
            growth |> Array.Parallel.map (fun g -> fit engine endCondition g model)
