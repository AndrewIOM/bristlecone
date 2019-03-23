namespace Bristlecone

open System
open Bristlecone.Logging

/// Configures a single function that represents a model and its likelihood
/// when fit to time-series data.
module Objective =

    open Bristlecone.Optimisation
    open Bristlecone.Optimisation.Techniques
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


/// Helper functions for the creation of `Solvers`, which can apply time-series models
/// to time-series data (when using Bristlecone time-series types).
module Solver =

    open Bristlecone.EstimationEngine

    /// Step the solver using the high resolution, and output at low resolution.
    /// External steps can be variable in size.
    let variableExternalStep engine forcings eqs =
        match engine.TimeHandling with
        | Discrete -> invalidOp "Not configured"
        | Continuous i ->
            fun state t0 t1 ->
                i engine.LogTo t0 t1 (t1-t0) state forcings eqs

    /// Step the solver using high resolution, and output at low resolution.
    /// External steps are of a fixed width. This is more efficient than running
    /// with variable steps.
    let fixedStep engine tStart tEnd initialState forcings =
        match engine.TimeHandling with
        | Discrete -> invalidOp "Not configured"
        | Continuous i -> fun eqs -> 
            // printfn "Start time is %f and end time is %f" tStart tEnd
            // printfn "Initial state is %A" initialState
            i engine.LogTo tStart tEnd 1. initialState forcings eqs
            // Skip the start point (conditioned point)
            // Return only the external step values
            |> Map.map (fun _ v -> 
                // printfn "Prediction is %i long" v.Length
                v |> Array.tail |> Seq.toArray)


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
    open Optimisation.Techniques
    open ModelSystem
    open EstimationEngine

    let generateFixedSeries writeOut equations timeMode seriesLength startPoint startDate theta =
        let applyFakeTime s = TimeSeries.fromSeq startDate (Days 1) s
        let eqs = equations |> Map.map (fun _ v -> v theta)
        match timeMode with
        | Discrete -> invalidOp "Not supported yet"
        | Continuous i -> 
            i writeOut 0. (seriesLength |> float) 1. startPoint Map.empty eqs // TODO allow testing to incorporate environmental forcings
            |> Map.map (fun _ v -> applyFakeTime v)

    /// A standard estimation engine using a random-walk monte carlo optimiser.
    let mkDiscrete : EstimationEngine<float,float> = {
        TimeHandling = Discrete
        OptimiseWith = Optimisation.MonteCarlo.randomWalk []
        LogTo = Bristlecone.Logging.Console.logger()
        Constrain = ConstraintMode.Detached
        Conditioning = NoConditioning }

    /// A standard `EstimationEngine` for ordinary differential equation models.
    let mkContinuous = {
        TimeHandling = Continuous <| Integration.MathNet.integrate
        OptimiseWith = Optimisation.MonteCarlo.randomWalk []
        LogTo = Bristlecone.Logging.Console.logger()
        Constrain = ConstraintMode.Detached
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

        // B. Find minimum and maximum temporal resolution

        // Ensure that dynamic variables are all running on a common temporal resolution
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
        let solver =

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
                            let envIndex = e |> Map.map(fun k v -> TimeIndex.TimeIndex(dStart, efRes, TimeIndex.IndexMode.Interpolate Statistics.Interpolate.bilinear, v))
                            (timeline, envIndex)
                    | None -> 
                        let timeline = (dData |> Seq.head).Value |> TimeIndex.indexSeries dStart fRes |> Seq.map fst
                        (timeline, Map.empty)

                // printfn "Timeline is %A" (timeline |> Seq.toList)
                // printfn "Forcings are %A" (forcings |> Map.map(fun x t -> t.Values |> Seq.toList))

                /// Parameters for the integration routine
                let startIndex = timeline |> Seq.head
                let endIndex = timeline |> Seq.last
                // Internal step size is 1, given that the time-index is scaled to the steps of the high-resolution external data
                let externalStep = (timeline |> Seq.tail |> Seq.head) - (timeline |> Seq.head) //TODO Figure out how to do this properly
                // printfn "External step size is %f" externalStep
                /// Condition data by setting a 'conditioned' starting state. 
                /// This is -1 position on the common timeline
                /// NB. The environmental data must span the conditioning period - how to ensure this?

                /// Run the integration as one operation from conditioned t0 to tn
                /// Filter the results so that only results that match low-res data in time are included
                let solve = Solver.fixedStep engine (startIndex - externalStep) endIndex conditionedPoint forcings
                fun ode -> 
                    let r = solve ode
                    // printfn "Result is %A" r
                    let rExternal = r |> Map.map (fun _ v -> v |> Seq.everyNth (int externalStep) |> Seq.toArray) //TODO proper lookup
                    // printfn "External result is %A" rExternal
                    // for x in r do
                        // printfn "Length of %s is %i" x.Key.Value x.Value.Length
                    rExternal

            | Variable ->
                // Run time as individual steps, to allow length to vary (slower)
                // Allow environmental data when on same temporal profile
                match eData with
                | Some (e,eDate,eRes) -> invalidOp "Variable time solver with environmental data has not been implemented yet."
                | None -> invalidOp "I've yet to finish the variable-time solver"

        let discreteSolve = Solver.Discrete.solve conditionedPoint
        let objective = Objective.create { model with Parameters = constrainedParameters } solver discreteSolve data

        let optimise = engine.OptimiseWith engine.LogTo endCondition (constrainedParameters |> ParameterPool.toDomain optimisationConstraints)
        let result = objective |> optimise
        let lowestLikelihood, bestPoint = result |> List.minBy (fun (l,_) -> l)

        let estimatedSeries = Objective.predict { model with Parameters = constrainedParameters } solver discreteSolve bestPoint
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

        { ResultId   = System.Guid.NewGuid()
          Likelihood = lowestLikelihood
          Parameters = bestPoint |> ParameterPool.fromPoint constrainedParameters
          Series     = paired
          Trace      = result }


    let drawUniform min max = MathNet.Numerics.Distributions.ContinuousUniform(min, max)

    let drawParameterSet parameters =
        parameters
        |> Map.map (fun _ v -> 
            let lower,upper = Parameter.bounds v
            let trueValue = (drawUniform lower upper).Sample()
            printfn "Bounds are %f - %f (true = %f)" lower upper trueValue
            Parameter.setEstimate v trueValue )

    /// **Description**
    /// Test that the specified estimation engine can correctly estimate known parameters. Random parameter sets are generated from the given model system.
    /// **Parameters**
    ///   * `model` - a `ModelSystem` of equations and parameters
    ///   * `permutations` - number of times to generate random parameters
    ///   * `startingConditions` - a coded map of values at t=0
    ///   * `engine` - an `EstimationEngine`
    let testModel engine timeSeriesLength startingConditions iterations generationRules addNoise (model:ModelSystem) =

        // TODO Sort out parameters
        let rawParameters = 
            let par = 
                model.Parameters 
                |> Map.map (fun k v -> detatchConstraint v) 
                |> Map.toList 
                |> List.map (fun (k,(x,y)) -> ((k, x)))
            (par |> Map.ofList)

        // TODO use other measure method?
        // TODO avoid unwrap and rewrap of timeseries map
        let computeMeasures measures (expected:CodedMap<TimeSeries<float>>) : CodedMap<TimeSeries<float>> =
            let time = (expected |> Seq.head).Value |> TimeSeries.toObservations |> Seq.map snd
            measures
            |> Map.map (fun key measure -> 
                Solver.Discrete.solve startingConditions key measure (expected |> Map.map(fun _ t -> t.Values |> Seq.toArray)))
            |> Map.fold (fun acc key value -> Map.add key (time |> Seq.zip value |> TimeSeries.fromObservations) acc) expected

        let startDate = (DateTime(DateTime.Now.Year, 01, 01))

        let rec generateData attempts =
            let theta = drawParameterSet rawParameters //TODO Investigate constraint functions: breaks if PositiveOnly?
            printfn "Theta proposed: %A" theta
            for t in theta do printfn "Value is %f for %s" (Parameter.getEstimate t.Value) t.Key.Value
            let trueSeries = 
                theta 
                |> generateFixedSeries engine.LogTo model.Equations engine.TimeHandling timeSeriesLength startingConditions startDate
                |> addNoise theta
                |> computeMeasures model.Measures
            let brokeTheRules = 
                generationRules
                |> List.map(fun (key,ruleFn) -> trueSeries |> Map.find key |> TimeSeries.toObservations |> Seq.map fst |> ruleFn)
                |> List.contains false
            match brokeTheRules with
            | true -> if attempts = 0 then invalidOp "Could not generate given rules" else generateData (attempts - 1)
            | false -> (theta, trueSeries)

        let theta, trueSeries = generateData 100000
        let estimated = fit engine iterations trueSeries model
        (estimated, trueSeries, theta)


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
                let subset = Optimisation.Techniques.Bootstrap.removeSingle random s
                let result = fit engine iterations subset hypothesis
                engine.LogTo <| GeneralEvent (sprintf "%s: completed bootstrap %i" identifier.Value numberOfTimes)
                bootstrap s (numberOfTimes - 1) (solutions |> List.append [result])
            else solutions
        bootstrap series bootstrapCount []


    module PlantIndividual =

        open PlantIndividual

        let fit engine iterations system (plant:PlantIndividual) =
            let g =
                match plant.Growth |> growthSeries with
                | Absolute g -> g
                | Cumulative g -> g
                | Relative g -> g
            let predictors = plant.Environment |> Map.add (ShortCode.create "x") g
            fit engine iterations predictors system


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