namespace Bristlecone

open Types
open System
open Types.PlantIndividual

module Objective =

    open Optimisation.Amoeba
    open OptimisationTechniques
    open Types.ParameterEstimation

    let parameteriseModel parameterPool point (model:ModelEquation) =
        model (point |> toParamList parameterPool)

    let pairObservationsToExpected (observed:CodedMap<float[]>) (expected:CodedMap<float[]>) : CodedMap<PredictedSeries> =
        observed
        |> Map.map (fun key value ->
            { Observed = value
              Expected = expected |> Map.find key } )

    let create (system:ModelSystem) integrate (observed:CodedMap<float array>) (p:Point) : float =
        system.Equations
        |> Map.map (fun _ v -> parameteriseModel system.Parameters p v)
        |> integrate
        |> pairObservationsToExpected observed
        |> system.Likelihood (p |> toParamList system.Parameters)


module Bristlecone =

    open Time
    open Types.ParameterEstimation
    open OptimisationTechniques

    type Time = float
    type State = float
    type ODE = (Time -> State -> Environment) -> State

    // Should be: Time seq -> Map<ShortCode,ODE> -> CodedMap<float []>
    type Integrate = float -> float -> float -> Environment -> CodedMap<ODE> -> CodedMap<float []>
    type Optimise = float[] -> float -> float list * float [] list

    // // Aims
    // // 1. Predator-prey model working
    // // 2. Lizzy's models and data working
    // // 2b. Some of the problems from the ecological detective working
    // // 3. My models and data working
    // // 4. My models with snow (use satellite for now) working

    // // Allow plugin of additional third-party / custom components.
    // // They must conform to the type signature defined here.

    // // What does bristlecone do internally?
    // // - Management of time series
    // // - Setting up environmental 'forcings' and managing environment
    // // - Measurement variables? [NB like outputs in state-space system]

    // // Optimisation techniques
    // // A. Amoeba single. Gives a single result.
    // // B. Amoeba bootstrapped. Gives an uncertainty measure for each parameter.
    // // C. MCMC Random walk. Gives an uncertainty measure for each parameter.

    // // AgeModel - an age profile, which can have time steps defined by:
    // // - A list of time points
    // // When integrating in continuous time, time bins may be sample points, or sample bins. This is important in determining the actual time?
    // // NB Problem with timelines - DateTime vs simplistic representation (i.e. float)?

    // // Parameter information required varies depending on estimation routine
    // // - When using MCMC, we need a variance and bounds (e.g. prior)
    // // - When using gradient descent, we only need a starting point / starting bounds

    type TimeMode =
    | DiscreteTime
    | ContinuousTime of Integrate

    type EndCondition =
    | Iterations of int

    /// Engine type captures key aspects of customisable behavior
    /// Key types: parameters
    type EstimationEngine<'data, 'time> = {
        TimeHandling: TimeMode
        OptimiseWith: 'data[] -> 'time -> 'data list * 'data [] list
        EndWhen: EndCondition
        OnError : (string*exn) -> unit
    }

    let start' (engine:EstimationEngine<'a,'b>) (system:ModelSystem) data =
        
        // 1. Given state variables, sort out internal timeline
        // 2. Given external forcings, sort out timeline
        // 3. Sort out start values and time
        // 4. Optimise
        // 4A.--(Integrate >> SortMeasurementVariables >> Calculate Likelihood)
        // 4A.--NB measurement variables ("transforms") may require dynamics in specific form (e.g. cumulative or absolute)
        // 5. Get Estimation Result
        invalidOp "Not implemented"

    let osloIntegration =
        ODE.Oslo.solveWithErrorHandling

    // let mkEstimator () =
    //     { TimeHandling  = ContinuousTime <| ODE.Oslo.solveWithErrorHandling
    //       OptimiseWith  = Optimisation.Amoeba.Solver.solve
    //       EndWhen       = 1000 |> Iterations
    //       OnError       = fun (x,_) -> printfn "%s" x }

    // let estimate engine modelSystem (data:CodedMap<TimeSeries<'a>>) =
    //     start' engine modelSystem data



    let private unwrap (x:float<_>) = float x

    module DefaultSetup =
        let optimisationLevels = 6
        let amoebaCount = 10


    let startingValues (series:CodedMap<TimeSeries<float<_>>>) : CodedMap<float> =
        series
        |> Map.map (fun _ v -> v.Values.[0] )

    let getCommonTime series =
        series
        |> Map.toList
        |> List.map snd
        |> TimeSeries.commonTimeline

    let dataToResolution resolution (series:TimeSeries<float>) =
        match resolution with
        | Annual ->  
            let steps =
                series.TimeSteps 
                |> Array.scan (+) TimeSpan.Zero
                |> Array.map (fun t -> float (series.StartDate + t).Year)
                |> Array.tail
            series.Values
            |> Array.zip steps
        | Monthly -> invalidOp "not implemented"
        | Daily -> invalidOp "not implemented"
        | CustomTicks _ -> invalidOp "not implemented"

    let conditionStartTime (series:(float*float)[]) = 
        let first = series |> Array.head
        series |> Array.append [|(fst first - 1.),(snd first)|] 

    // Common timelines only
    // Fixed temporal resolution only
    let estimate' resolution iterations (system:ModelSystem) (start:StartingValues) (series:CodedMap<TimeSeries<float<_>>>) =
        match series |> getCommonTime with
        | None -> invalidOp "The timeline is not common for these series. The timeline must be the same for all series"
        | Some _ ->
            let scaledSeries = series |> Map.map (fun _ s -> s |> dataToResolution resolution)
            printfn "Series scaled: %A" scaledSeries
            let cumulativeTime = scaledSeries |> Map.toList |> List.head |> snd |> Array.map fst |> Array.toList
            let startTime = cumulativeTime.Head
            let endTime = cumulativeTime |> List.last
            let timeStep = (cumulativeTime.Tail.Head) - cumulativeTime.Head
            printfn "Start = %A, end = %A, step = %A" startTime endTime timeStep
            let startingValues = 
                match start with
                | FirstDataItem -> startingValues series
                | Custom x -> x
            // let integrator = ODE.Oslo.solveWithErrorHandling startTime endTime timeStep startingValues
            let integrator = ODE.MathNet.solve startTime endTime timeStep startingValues
            // let optimise = OptimisationTechniques.HeuristicOptimisation.heuristicOptimisation DefaultSetup.optimisationLevels iterations DefaultSetup.amoebaCount (system.Parameters |> toDomain)
            let optimise = Optimisation.MCMC.randomWalk 1 iterations (system.Parameters |> toDomain)
            let f = Objective.create system integrator (scaledSeries |> Map.map (fun _ value -> value |> Array.map snd))

            // Get results, and rewrap point as parameters
            let likelihood,point = optimise f

            let bestPoint, bestLikelihood =
                likelihood
                |> List.zip point
                |> List.minBy (fun (_,l) -> l)

            let estimated = fromPoint system.Parameters bestPoint

            // Get predicted and observed series together
            let estimatedSeries = integrator (system.Equations |> Map.map (fun _ v -> v estimated))
            let paired = series |> Map.map (fun k v -> { Observed = v |> TimeSeries.toSeq |> Seq.map fst |> Seq.toArray; Expected = estimatedSeries |> Map.find k })

            { Likelihood = bestLikelihood; Parameters = estimated; Series = paired; Trace = likelihood,point }

    let estimate resolution iterations system (start:StartingValues) (growth:GrowthSeries<_>) =
        // Mess around getting float series out of data...
        let g =
            match growth with
            | Absolute g -> g // Can have missing data
            | Cumulative g -> g // Cannot have missing data!
            | Relative g -> g
        // Check series is not empty
        // Condition for initial conditions...?
        // ** IF ABSOLUTE DATA BUT FITTING CUMULATIVE FUNCTION, TAKE DERIV OF PREDICTION BEFORE LIKELIHOOD? **
        estimate' resolution iterations system start ([ShortCode.create "x", g] |> Map.ofList)

    let estimatePlant resolution iterations system start (plant:PlantIndividual) =
        let g =
            match plant.Growth |> growthSeries with
            | Absolute g -> g
            | Cumulative g -> g
            | Relative g -> g
        let predictors = plant.Environment |> Map.add (ShortCode.create "x") g
        estimate' resolution iterations system start predictors

    let test' resolution iterations timeSeriesLength startValues (system:ModelSystem) =

        let drawNormal min max = MathNet.Numerics.Distributions.Normal((max - min),(max - min) / 4.)

        // Sample a random 'true' parameter set using the given bounds for each
        // parameter as the range of a normal distribution.
        let randomTheta =
            system.Parameters 
            |> Map.map (fun _ v -> 
                printfn "%A" v
                let lower,upper = Parameter.bounds v
                let trueValue = (drawNormal lower upper).Sample()
                Parameter.setEstimate v trueValue )

        // - Find the expected values for all times

        let applyFakeTime s = TimeSeries.create (DateTime(2000,01,01)) (TimeSpan.FromDays 370. ) s
        let startTime = 0.
        let endTime = timeSeriesLength |> float
        let timeStep = 1.

        let solve p =
            let eqs =
                system.Equations
                |> Map.map (fun _ v -> v p)
            // ODE.Oslo.solve startTime endTime timeStep startValues eqs
            ODE.MathNet.solve startTime endTime timeStep startValues eqs

        let trueSeries = randomTheta |> solve
 
        // - Estimate the parameters given the parameter space

        let est = estimate' resolution iterations system (StartingValues.Custom startValues)
        trueSeries |> (Map.map (fun _ v -> v |> applyFakeTime) >> est)


    let bootstrap resolution hypothesis iterations bootstrapCount (identifier:ShortCode) series =

        let rec bootstrap s numberOfTimes solutions =
            if (numberOfTimes > 0) then
                let subset = OptimisationTechniques.Bootstrap.removeSingle s
                let result = estimate' resolution hypothesis iterations FirstDataItem subset
                printfn "%s: completed bootstrap %i" identifier.Value numberOfTimes
                bootstrap s (numberOfTimes - 1) (solutions |> List.append [result])
            else solutions

        bootstrap series bootstrapCount []


    ///**Description**
    ///Boostrapping removes a single observation from the time series, and estimates parameters 'n' times.
    ///**Output Type**
    ///Details of the plant invidual, alongside an array of parameter estimates.
    let bootstrapPlant resolution iterations bootstrapCount hypothesis plant =
        printfn "Bootstrapping parameter estimation for %s (x%i)" plant.Identifier.Value bootstrapCount
        let g =
            match plant.Growth |> growthSeries with
            | Absolute g -> g
            | Cumulative g -> g
            | Relative g -> g
        let predictors = plant.Environment |> Map.add (ShortCode.create "x") g
        bootstrap resolution iterations hypothesis bootstrapCount plant.Identifier predictors


    module Parallel =

        let estimate resolution iterations system growth =
            growth |> Array.Parallel.map (estimate resolution iterations system)

