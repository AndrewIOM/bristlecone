namespace DendroFit

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
        expected
        |> Map.map (fun key value ->
            { Expected = value
              Observed = observed |> Map.find key } )

    let create (system:ModelSystem) integrate (observed:CodedMap<float array>) (p:Point) : float =
        system.Equations
        |> Map.map (fun _ v -> parameteriseModel system.Parameters p v)
        |> integrate
        |> pairObservationsToExpected observed
        |> system.Likelihood (p |> toParamList system.Parameters)


module DendroFit =

    open OptimisationTechniques
    open OptimisationTechniques.HeuristicOptimisation
    open Time
    open Types.ParameterEstimation

    let private unwrap (x:float<_>) = float x

    module DefaultSetup =
        let optimisationLevels = 6
        let amoebaCount = 10
        let testGridSectors = 10

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
    let estimate' resolution iterations (system:ModelSystem) (series:CodedMap<TimeSeries<float<_>>>) =
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
            let integrator = ODE.Oslo.solve startTime endTime timeStep (startingValues series)
            let optimise = heuristicOptimisation DefaultSetup.optimisationLevels iterations DefaultSetup.amoebaCount (system.Parameters |> toDomain)
            let f = Objective.create system integrator (scaledSeries |> Map.map (fun _ value -> value |> Array.map snd))
            
            // Get results, and rewrap point as parameters
            let likelihood,point = optimise f
            let estimated = fromPoint system.Parameters point

            // Get predicted and observed series together
            let estimatedSeries = ODE.Oslo.solve startTime endTime timeStep (startingValues series) (system.Equations |> Map.map (fun _ v -> v estimated))
            let paired = estimatedSeries |> Map.map (fun k v -> { Expected = v; Observed = series |> Map.find k |> TimeSeries.toSeq |> Seq.map fst |> Seq.toArray })
           
            { Likelihood = likelihood; Parameters = estimated; Series = paired }

    let estimate resolution iterations system (growth:GrowthSeries<_>) =
        // Mess around getting float series out of data...
        let g =
            match growth with
            | Absolute g -> g // Can have missing data
            | Cumulative g -> g // Cannot have missing data!
            | Relative g -> g
        // Check series is not empty
        // Condition for initial conditions...?
        // ** IF ABSOLUTE DATA BUT FITTING CUMULATIVE FUNCTION, TAKE DERIV OF PREDICTION BEFORE LIKELIHOOD? **
        estimate' resolution iterations system ([ShortCode.create "x", g] |> Map.ofList)

    let estimatePlant resolution iterations system (plant:PlantIndividual) =
        let g =
            match plant.Growth |> growthSeries with
            | Absolute g -> g
            | Cumulative g -> g
            | Relative g -> g
        let predictors = plant.Environment |> Map.add (ShortCode.create "x") g
        estimate' resolution iterations system predictors
    
    let test' resolution iterations timeSeriesLength (system:ModelSystem) =
        // Systematically sample parameter space for multiple outcomes:
        // - Establish 'true' parameters within parameter space

        let sectors = DefaultSetup.testGridSectors
        let trueParameters =
            [ 0 .. sectors ]
            |> List.map (fun i -> 
                system.Parameters 
                |> Map.map (fun _ v -> 
                    printfn "%A" v
                    let lower,upper = Parameter.bounds v
                    let step = (upper - lower) / (float sectors)
                    let trueValue = lower + step * (float i)
                    Parameter.setEstimate v trueValue ) )

        // - Find the expected values for all times

        let applyFakeTime s = TimeSeries.create (DateTime(2000,01,01)) (TimeSpan.FromDays 370. ) s
        let startTime = 0.
        let endTime = timeSeriesLength |> float
        let timeStep = 1.

        let solve p =
            let eqs =
                system.Equations
                |> Map.map (fun _ v -> v p)
            let startValues = [ ShortCode.create "x", 0.01; ShortCode.create "N", 1.0] |> Map.ofList
            ODE.Oslo.solve startTime endTime timeStep startValues eqs

        let trueSeries = trueParameters |> List.map solve
 
        // - Estimate the parameters given the parameter space

        let est = estimate' resolution iterations system
        let estimates =
            trueSeries |> List.map (Map.map (fun _ v -> v |> applyFakeTime) >> est)

        // Grid out parameter space
        // NB How does computation rely on co-variance of parameters?

        // - Compare the expected and 'true' time series, and likelihoods
        let likelihood = 
            estimates
            |> List.sumBy (fun e -> e.Likelihood)
        // likelihood / (float sectors) / (startTime - endTime)

        // Generate a 'sensitivity' for each parameter
        // This is the value generated versus the 'true' value
        let sensitivity =
            trueParameters
            |> List.zip estimates
            |> List.collect (fun (e,p) ->
                e.Parameters
                |> Map.map (fun k v -> 
                    let other = p |> Map.find k |> Parameter.getEstimate
                    [other; v |> Parameter.getEstimate ]
                    |> Seq.stddev  )
                |> Map.toList )
            |> List.groupBy fst
            |> List.map(fun (k,s) -> k, s |> List.map(fun (_,l2) -> l2 ) |> Seq.stddev )

        likelihood, sensitivity

    let bootstrap resolution hypothesis iterations bootstrapCount (identifier:ShortCode) series =

        let rec bootstrap s numberOfTimes solutions =
            if (numberOfTimes > 0) then
                let subset = OptimisationTechniques.Bootstrap.removeSingle s
                let result = estimate' resolution hypothesis iterations subset
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
