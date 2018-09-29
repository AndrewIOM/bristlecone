namespace Bristlecone

open System

module Objective =

    open Optimisation.Amoeba
    open Optimisation.Techniques
    open ModelSystem

    let parameteriseModel parameterPool point (model:ModelEquation) =
        model (point |> ParameterPool.toParamList parameterPool)

    let pairObservationsToExpected (observed:CodedMap<float[]>) (expected:CodedMap<float[]>) : CodedMap<PredictedSeries> =
        observed
        |> Map.map (fun key value ->
            { Observed = value
              Expected = expected |> Map.find key } )

    let predict (system:ModelSystem) integrate (p:Point) =
        system.Equations
        |> Map.map (fun _ v -> parameteriseModel system.Parameters p v)
        |> integrate

    let create (system:ModelSystem) integrate (observed:CodedMap<float array>) (p:Point) =
        p
        |> predict system integrate
        |> pairObservationsToExpected observed
        |> system.Likelihood (p |> ParameterPool.toParamList system.Parameters)


module Bristlecone =

    open Time
    open Optimisation.Techniques
    open ModelSystem
    open EstimationEngine

    let conditionStartTime condition key (series:(float*float)[]) =
        match condition with
        | NoConditioning -> series
        | RepeatFirstDataPoint ->
            let first = series |> Array.head
            series |> Array.append [|(fst first - 1.),(snd first)|] 
        | Custom precomputed -> 
            let custom = precomputed |> Map.find key
            let first = series |> Array.head
            series |> Array.append [| (fst first - 1.), custom |] 

    let makeSolverWithData timeMode conditioning series =
        let data = (series |> Map.map (fun _ value -> value |> Array.map snd))
        let initialPoint =
            match conditioning with
            | Custom c -> c
            | _ -> (series |> Map.map (fun _ value -> value |> Array.map snd |> Array.head))
        let cumulativeTime = series |> Map.toList |> List.head |> snd |> Array.map fst |> Array.toList
        let timeStep = (cumulativeTime.Tail.Head) - cumulativeTime.Head
        match timeMode with
        | Discrete -> invalidOp "Not implemented"
        | Continuous i ->
            let solver = i cumulativeTime.Head (cumulativeTime |> List.last) timeStep initialPoint
            solver, data

    let generateFixedSeries equations timeMode seriesLength startPoint theta =
        let applyFakeTime s = TimeSeries.create (DateTime(DateTime.Now.Year,01,01)) Annual s
        let eqs = equations |> Map.map (fun _ v -> v theta)
        match timeMode with
        | Discrete -> invalidOp "Not supported yet"
        | Continuous i -> 
            i 0. (seriesLength |> float) 1. startPoint eqs
            |> Map.map (fun _ v -> applyFakeTime v)

    let drawNormal min max = MathNet.Numerics.Distributions.Normal((max - min),(max - min) / 4.)

    let drawParameterSet parameters =
        parameters
        |> Map.map (fun _ v -> 
            let lower,upper = Parameter.bounds v
            let trueValue = (drawNormal lower upper).Sample()
            Parameter.setEstimate v trueValue )


    /// A standard estimation engine using a random-walk monte carlo optimiser.
    let mkDiscrete = {
        TimeHandling = Discrete
        OptimiseWith = Optimisation.MonteCarlo.randomWalk
        OnError = ignore
        Constrain = ConstraintMode.Detached
        Conditioning = NoConditioning }

    /// A standard `EstimationEngine` for ordinary differential equation models.
    let mkContinuous = {
        TimeHandling = Continuous Integration.MsftOslo.integrateWithErrorHandling
        OptimiseWith = Optimisation.MonteCarlo.randomWalk
        OnError = ignore
        Constrain = ConstraintMode.Detached
        Conditioning = NoConditioning }

    /// Use a custom integration method
    let withContinuousTime t engine =
        { engine with TimeHandling = Continuous t }

    /// Choose how the start point is chosen when solving the model system
    let withConditioning c engine =
        { engine with Conditioning = c }


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
    let fit engine iterations burnin (timeSeriesData:CodedMap<TimeSeries<float>>) (model:ModelSystem) =

        let constrainedParameters, optimisationConstraints = 
            match engine.Constrain with
            | Transform -> model.Parameters, [1 .. model.Parameters.Count] |> List.map(fun _ -> Unconstrained)
            | Detached -> 
                let par,con = 
                    model.Parameters 
                    |> Map.map (fun k v -> detatchConstraint v) 
                    |> Map.toList 
                    |> List.map (fun (k,(x,y)) -> (k,x),y)
                    |> List.unzip
                par |> Map.ofList, con

        let objective =
            timeSeriesData
            |> TimeSeries.validateCommonTimeline
            |> Map.map (fun _ v -> Resolution.scaleTimeSeriesToResolution Annual v)
            |> Map.map (fun k v -> conditionStartTime engine.Conditioning k v)
            |> makeSolverWithData engine.TimeHandling engine.Conditioning
            ||> Objective.create { model with Parameters = constrainedParameters }

        let optimise = engine.OptimiseWith burnin iterations (constrainedParameters |> ParameterPool.toDomain optimisationConstraints)
        let result = objective |> optimise
        let lowestLikelihood, bestPoint = result |> List.minBy (fun (_,l) -> l)

        { Likelihood = lowestLikelihood
          Parameters = bestPoint |> ParameterPool.fromPoint constrainedParameters
          Series = [ ShortCode.create "", { Expected = [||]; Observed = [||]} ] |> Map.ofList
          Trace = result }


    /// **Description**
    /// Test that the specified estimation engine can correctly estimate known parameters. Random parameter sets are generated from the given model system.
    /// **Parameters**
    ///   * `model` - a `ModelSystem` of equations and parameters
    ///   * `permutations` - number of times to generate random parameters
    ///   * `startingConditions` - a coded map of values at t=0
    ///   * `engine` - an `EstimationEngine`
    let testModel engine timeSeriesLength startingConditions iterations burnin (model:ModelSystem) =
        let theta = drawParameterSet model.Parameters
        let trueSeries = theta |> generateFixedSeries model.Equations engine.TimeHandling timeSeriesLength startingConditions
        printfn "True time series is: %A" trueSeries
        let estimated = fit engine iterations burnin trueSeries model
        estimated, trueSeries, theta


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
    let bootstrap engine iterations burnin bootstrapCount hypothesis (identifier:ShortCode) series =
        let rec bootstrap s numberOfTimes solutions =
            if (numberOfTimes > 0) then
                let subset = Optimisation.Techniques.Bootstrap.removeSingle s
                let result = fit engine iterations burnin subset hypothesis
                printfn "%s: completed bootstrap %i" identifier.Value numberOfTimes
                bootstrap s (numberOfTimes - 1) (solutions |> List.append [result])
            else solutions
        bootstrap series bootstrapCount []


    module PlantIndividual =

        open PlantIndividual

        let fit engine iterations burnin system (plant:PlantIndividual) =
            let g =
                match plant.Growth |> growthSeries with
                | Absolute g -> g
                | Cumulative g -> g
                | Relative g -> g
            let predictors = plant.Environment |> Map.add (ShortCode.create "x") g
            fit engine iterations burnin predictors system


    module Parallel =

        let fit engine resolution iterations system growth =
            growth |> Array.Parallel.map (fit engine resolution iterations system)


[<AutoOpen>]
module DSL =

    let parameter = Parameter.create

    let code = ShortCode.create

    let lookup (map:CodedMap<float>) name = map.[code name]
