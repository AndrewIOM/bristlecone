namespace Bristlecone

open System
open Bristlecone.Logging

[<RequireQualifiedAccess>]
module Bristlecone =

    open Bristlecone.Time
    open Bristlecone.ModelSystem
    open Bristlecone.EstimationEngine

    /// A standard estimation engine using a random-walk monte carlo optimiser.
    let mkDiscrete : EstimationEngine<float,float> = {
        TimeHandling = Discrete
        OptimiseWith = Optimisation.Amoeba.single Optimisation.Amoeba.Solver.Default
        LogTo = Console.logger(1000)
        Random = MathNet.Numerics.Random.MersenneTwister(true)
        Conditioning = Conditioning.NoConditioning }

    /// A standard `EstimationEngine` for ordinary differential equation models.
    let mkContinuous = {
        TimeHandling = Continuous <| Integration.MathNet.integrate
        OptimiseWith = Optimisation.Amoeba.single Optimisation.Amoeba.Solver.Default
        LogTo = Bristlecone.Logging.Console.logger(1000)
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
        { engine with OptimiseWith = Optimisation.Amoeba.single Optimisation.Amoeba.Solver.Default }

    let withCustomOptimisation optim engine =
        { engine with OptimiseWith = optim }

    module Fit =

        /// Places a map of `TimeSeries` into a `TimeFrame` that has data
        /// that shares a common timeline. If no timeline is shared, returns
        /// an `Error`.
        let observationsToCommonTimeFrame (equations:CodedMap<ModelEquation>) timeSeriesData =
            let equationKeys = equations |> Map.keys
            timeSeriesData 
            |> Map.filter(fun k _ -> equationKeys |> Seq.contains k)
            |> TimeFrame.tryCreate
            |> Result.ofOption "Observations for dynamic variables must share a common sampling time sequence"

        /// Finds environmental data in a timeseries map (i.e. those datasets that are not
        /// dynamic variables or measures), and constructs a common `TimeFrame`.
        /// Returns None if there is no environmental data.
        let environmentDataToCommonTimeFrame dynamicVariableKeys measureKeys timeSeriesData =
            let environmentSeries = 
                timeSeriesData 
                |> Map.filter(fun k _ -> 
                    dynamicVariableKeys 
                    |> Seq.append measureKeys 
                    |> Seq.contains k 
                    |> not)
            match environmentSeries.Count with
            | 0 -> None |> Ok
            | _ -> 
                match TimeFrame.tryCreate environmentSeries with
                | Some e -> e |> Some |> Ok
                // TODO Ensure that environment data overlaps with conditioning period?
                | None -> Error "Forcing variables were specified on differing timelines. Pre-process your data to make environmental data conform to a common temporal baseline and time interval"

        /// TODO make function to check if time-series one is a subset of
        /// time-series two.
        let exactSubsetOf one two =
            // match environmentVariableResolution with
            // | None -> ()
            // | Some eRes -> 
            Ok (one, two)

        /// Returns a tuple of the start point (t0) and the 
        /// subsequent time-series (t1 .. tn).
        let t0 timeSeriesData (conditionMode: Conditioning.Conditioning<'a>) logger = 
            timeSeriesData
            |> Solver.Conditioning.startPoint conditionMode
            // |> Option.map(fun t0 -> t0, timeSeriesData)
            |> Option.defaultWith (fun () ->
                logger <| DebugEvent "No conditioning was specified. Using t1 as conditioning data."
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
    let fit engine endCondition timeSeriesData (model:ModelSystem) =

        // A. Setup initial time point values based on conditioning method.
        let t0 = Fit.t0 timeSeriesData engine.Conditioning engine.LogTo

        // B. Create a continuous-time that outputs float[] 
        // containing only the values for the dynamic variable resolution.
        let continuousSolver = 
            result {
                // 1. Set time-series into common timeline
                let! commonDynamicTimeFrame = Fit.observationsToCommonTimeFrame model.Equations timeSeriesData
                let! commonForcingsTimeFrame = Fit.environmentDataToCommonTimeFrame (Map.keys model.Equations) (Map.keys model.Measures) timeSeriesData
                engine.LogTo <| GeneralEvent (sprintf "Time-series start at %s with resolution %A." (commonDynamicTimeFrame.StartDate.ToString "yyyy/MM/dd") commonDynamicTimeFrame.Resolution)

                // 2. Ensure that dynamic variables is an exact or subset of environmental variables
                let! _ = Fit.exactSubsetOf commonDynamicTimeFrame commonForcingsTimeFrame

                let solver outputStep = Solver.solver outputStep commonDynamicTimeFrame commonForcingsTimeFrame engine t0
                return solver
        }

        let discreteSolver = Solver.Discrete.solve t0

        let data = 
            timeSeriesData 
            |> Map.map (fun _ ts -> ts.Values |> Seq.toArray)

        // Setup optimisation algorithm given the parameter constraint mode.
        let optimise, constrainedParameters = 
            match engine.OptimiseWith with
            | Optimiser.InDetachedSpace optim -> 
                let detatchedPool, optimConstraints = Parameter.Pool.detatchConstraints model.Parameters
                optim engine.Random engine.LogTo endCondition (detatchedPool |> Parameter.Pool.toDomain optimConstraints) None,
                detatchedPool
            | Optimiser.InTransformedSpace optim -> 
                let optimConstraints = List.init (Parameter.Pool.count model.Parameters) (fun _ -> Parameter.Unconstrained)
                optim engine.Random engine.LogTo endCondition (model.Parameters |> Parameter.Pool.toDomain optimConstraints) None,
                model.Parameters

        result {
            let! continuousSolver = continuousSolver
            let objective = 
                Objective.create 
                    { model with Parameters = constrainedParameters }
                    (continuousSolver Solver.StepType.External)
                    discreteSolver
                    data

            let result = objective |> optimise
            let lowestLikelihood, bestPoint = result |> List.minBy (fun (l,_) -> l)

            let estimatedSeries = Objective.predict { model with Parameters = constrainedParameters } (continuousSolver Solver.StepType.External) discreteSolver bestPoint
            let estimatedHighRes = Objective.predict { model with Parameters = constrainedParameters } (continuousSolver Solver.StepType.Internal) discreteSolver bestPoint
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

            return { 
                ResultId   = Guid.NewGuid()
                Likelihood = lowestLikelihood
                Parameters = bestPoint |> Parameter.Pool.fromPointInTransformedSpace constrainedParameters
                Series     = paired
                Trace      = result
                InternalDynamics = Some estimatedHighRes }
        }


    /// Functions for fitting models that do not include a temporal dimension.
    module Invariant =

        /// Fit a time-invariant model.
        let fitWithoutTime optimise constrain log endCondition random (data:CodedMap<float>) (model:ModelSystem) =
            let engine = {
                TimeHandling = Discrete
                OptimiseWith = optimise
                Conditioning = Conditioning.NoConditioning
                Random = random
                LogTo = log
            }
            invalidOp "Not implemented"
            //let ts = data |> Map.map(fun _ v -> TimeSeries.fromSeq (v, DateTime.Now))
            //fit engine endCondition ts model

    open Test

    /// **Description**
    /// Test that the specified estimation engine can correctly estimate known parameters. Random parameter sets are generated from the given model system.
    /// **Parameters**
    ///   * `model` - a `ModelSystem` of equations and parameters
    ///   * `testSettings` - settings
    ///   * `engine` - an `EstimationEngine`
    let testModel engine (settings:Test.TestSettings<float>) (model:ModelSystem) : Result<Test.TestResult,string> =
        engine.LogTo <| GeneralEvent "Attempting to generate parameter set."
        engine.LogTo <| GeneralEvent (sprintf "The data must comply with %i rules after %i tries." settings.GenerationRules.Length settings.Attempts)
        result {
            let! settings = Test.isValidSettings model settings
            let! trueData, theta = Test.Compute.tryGenerateData engine settings model settings.Attempts

            let! realEstimate = fit { engine with OptimiseWith = Optimisation.None.none } settings.EndCondition (Map.merge trueData settings.EnvironmentalData (fun x y -> x)) { model with Parameters = Parameter.Pool.fromEstimated theta }
            printfn "Real estimate was %A" <| realEstimate

            let! estimated = fit engine settings.EndCondition (Map.merge trueData settings.EnvironmentalData (fun x y -> x)) model
            let paramDiffs : Test.ParameterTestResult list =
                estimated.Parameters
                |> Parameter.Pool.toList
                |> List.map(fun (k,v) ->
                    let est = Parameter.getEstimate v
                    let real = theta |> Parameter.Pool.toList |> List.find (fun (k2,v) -> k2 = k) |> snd |> Parameter.getEstimate
                    match est with
                    | Ok e ->
                        match real with
                        | Ok r -> {
                            Identifier = k.Value
                            RealValue = r
                            EstimatedValue = e }
                        | Error _ -> failwith "Error"
                    | Error _ -> failwith "Error" )
            let errorStructure = estimated.Series |> Seq.map(fun k -> k.Key.Value, k.Value.Values |> Seq.map(fun t -> (t.Fit - t.Obs) ** 2. )) |> Map.ofSeq
            return { 
                ErrorStructure = errorStructure
                Parameters = paramDiffs
                Series = estimated.Series |> Seq.map(fun k -> k.Key.Value, k.Value) |> Map.ofSeq
                RealLikelihood = realEstimate.Likelihood
                EstimatedLikelihood = estimated.Likelihood }
        }

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
        |> Seq.map (fun d -> fit (engine |> withCustomOptimisation Optimisation.None.none |> withConditioning Conditioning.RepeatFirstDataPoint) (Optimisation.EndConditions.afterIteration 0) d hypothesisMle)
        |> Seq.toList


    module Parallel =

        let fit engine endCondition model growth =
            growth |> Array.Parallel.map (fun g -> fit engine endCondition g model)
