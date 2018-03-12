namespace DendroFit

open Types
open Types.ParameterEstimation
open Time
open System

module Objective =

    open Optimisation.Amoeba
    open OptimisationHelpers

    let parameteriseModel parameterPool point (model:ModelEquation) =
        model (point |> toParamList parameterPool)

    let pairObservationsToExpected (observed:CodedMap<float[]>) (expected:CodedMap<float[]>) : CodedMap<PredictedSeries> =
        expected
        |> Map.map (fun key value ->
            { Expected = value
              Observed = observed |> Map.find key } )

    let negativeLogLikelihood likelihood series =
        (*-log*) (series |> likelihood)        

    let create (system:ModelSystem) integrate (observed:CodedMap<float array>) (p:Point) : float =
        printfn "P = %A" p
        system.Equations
        |> Map.map (fun _ v -> parameteriseModel system.Parameters p v)
        |> integrate
        |> pairObservationsToExpected observed
        |> negativeLogLikelihood (system.Likelihood (p |> toParamList system.Parameters))


module DendroFit =

    open OptimisationHelpers
    open OptimisationHelpers.HeuristicOptimisation

    let private unwrap (x:float<_>) = float x

    module DefaultSetup =
        let optimisationLevels = 6
        let amoebaCount = 10

    let startingValues (series:CodedMap<TimeSeries<float<_>>>) : CodedMap<float> =
        series
        |> Map.map (fun _ v -> v.Values.[0] ) //TODO handle empty cases

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
        // At the moment, simply replicate the t1 value to t0, for all series
        let first = series |> Array.head
        series |> Array.append [|(fst first - 1.),(snd first)|] 

    // Common timelines only
    // Fixed temporal resolution only
    let estimate' resolution iterations (system:ModelSystem) (series:CodedMap<TimeSeries<float<_>>>) =
        match series |> getCommonTime with
        | None -> invalidOp "The timeline is not common for these series. The timeline must be the same for all series"
        | Some _ ->
            let scaledSeries = series |> Map.map (fun _ s -> s |> dataToResolution resolution |> conditionStartTime)
            printfn "Series scaled: %A" scaledSeries
            let cumulativeTime = scaledSeries |> Map.toList |> List.head |> snd |> Array.map fst |> Array.toList
            let startTime = cumulativeTime.Head
            let endTime = cumulativeTime |> List.last
            let timeStep = (cumulativeTime.Tail.Head) - cumulativeTime.Head
            printfn "Start = %A, end = %A, step = %A" startTime endTime timeStep
            let integrator = ODE.Oslo.solve startTime endTime timeStep (startingValues series)
            let optimise = heuristicOptimisation DefaultSetup.optimisationLevels iterations DefaultSetup.amoebaCount (system.Parameters |> toDomain)
            let f = Objective.create system integrator (scaledSeries |> Map.map (fun _ value -> value |> Array.map snd))
            optimise f

    let estimate resolution iterations system (growth:GrowthSeries<_>) =
        // Mess around getting float series out of data...
        let g =
            match growth with
            | Absolute g -> g // Can have missing data
            | Cumulative g -> g // Cannot have missing data!
        // Check series is not empty
        // Condition for initial conditions...?
        estimate' resolution iterations system ([ShortCode.create "x", g] |> Map.ofList)



    // let likelihoodSample (p:ParameterPool) (pred:CodedMap<PredictedSeries>) : float =
    //     p.["H"] + pred.Item["x"].Expected







    // let private estimate' (hypothesis:ModelSystem) iterations (chronology:GrowthChronology) =
    //     let startDate = float (fst chronology.Head).Year - 1.
    //     let endDate = float (fst (List.last chronology)).Year
    //     let timeStep = 1.
    //     let initialMass = chronology |> List.head |> snd |> unwrap

    //     let f (p:Point) =
    //         let pointParams = p |> toParamList hypothesis.Parameters
    //         let expected = ODE.rungekutta4 startDate initialMass timeStep endDate (hypothesis.Model pointParams) [] []
    //         let obs = chronology |> List.map (fun x -> (removeUnit (snd x)))

    //         if (snd expected) |> containsNan then modelError
    //         else 
    //             if isWithinParameterSpace pointParams
    //                 then hypothesis.LogLikelihood pointParams (snd expected) obs
    //                 else
    //                     let nearestPoint = getNearestValidPoint pointParams
    //                     let likelihood = hypothesis.LogLikelihood nearestPoint (snd expected) obs
    //                     likelihood

    //     let result = heuristicOptimisation 6 iterations 10 (hypothesis.Parameters |> toDomain) f 
    //     result

    // ///<summary>Estimator modified for coupled model systems.</summary>
    // let private estimateCoupled' (hypothesis:CoupledModelSystem) iterations (response:GrowthChronology) (predictor:PredictorChronology) =

    //     let commonTimeScale = commonTimeScale response predictor
    //     let timeStep = 3.//float (commonTimeScale.[1].Year - commonTimeScale.[0].Year) // TODO assumes equal time steps
    //     let startDate = float (commonTimeScale.Head.Year) - timeStep
    //     let endDate = float (List.last commonTimeScale).Year // TODO Remove (this is temporary fix for bootstrapping)
    //     let initialMass = removeUnit (snd (response.[0])) // TODO condition on first value of time series?
    //     let initialPredictor = snd (predictor.[0])

    //     printfn "Starting at %f (N), %f (mass)" initialPredictor initialMass

    //     let responseOnCommonTime = response |> List.filter(fun x -> (commonTimeScale |> Seq.exists (fun y -> y = (fst x))))
    //     let predictorOnCommonTime = predictor |> List.filter(fun x -> (commonTimeScale |> Seq.exists (fun y -> y = (fst x))))

    //     let f (p:Point) =
    //         let pointParams = p |> toParamList hypothesis.Parameters
    //         let expected = ODE.rungekutta4dual startDate initialMass initialPredictor timeStep endDate (hypothesis.Response pointParams) (hypothesis.Predictor pointParams) [] [] []
    //         let obsx = responseOnCommonTime |> List.map (fun x -> (removeUnit (snd x)))
    //         let obsy = predictorOnCommonTime |> List.map snd
    //         let expt, expx, expy = expected
    //         if (expx |> containsNan) || (expy |> containsNan) then modelError
    //         else
    //             let l = hypothesis.LogLikelihood pointParams obsx obsy expx expy
    //             if Double.IsNaN l then infinity
    //             else l
    //             // if not (isWithinParameterSpace pointParams)
    //             // then hypothesis.LogLikelihood pointParams obsx obsy expx expy
    //             // else hypothesis.LogLikelihood pointParams obsx obsy expx expy
    //                 //let nearestPoint = getNearestValidPoint pointParams
    //                 //let likelihood = hypothesis.LogLikelihood nearestPoint obsx obsy expx expy
    //                 //likelihood + (constraintPenalty pointParams)

    //     let result = heuristicOptimisation 6 iterations 10 (hypothesis.Parameters |> toDomain) f 
    //     result
    
    // // <summary>
    // // Estimate parameters once for the given model system and plant individual.
    // // </summary>
    // let estimate hypothesis iterations plant =
    //     printfn "%s: Estimating parameters" plant.Identifier
    //     plant, (estimate' hypothesis iterations plant.Chronology)

    // // <summary>
    // // Estimate parameters for many plant individuals in parallel
    // // </summary>
    // let estimateParallel hypothesis iterations plants =
    //     List.toArray plants |> Array.Parallel.map (estimate hypothesis iterations)

    // let estimateParallelWindowed hypothesis windowSize iterations plants =

    //     let parallelProcessWindow window =
    //         //printfn "Starting bin number %i" x
    //         let solution = estimate' hypothesis iterations window
    //         //printfn "Finished bin #%i: -loglikelihood=%f" x (fst solution)
    //         solution

    //     plants
    //     |> List.map (fun x -> Seq.windowed windowSize x.Chronology |> Seq.toList)
    //     |> List.toArray
    //     |> Array.Parallel.map (fun plant -> Array.Parallel.map parallelProcessWindow)
        

    // // <summary>
    // // Boostrapping removes a single observation from the time series, and estimates parameters 'n' times.
    // // </summary>
    // // <returns>Details of the plant invidual, alongside an array of parameter estimates.</returns>
    // let bootstrap hypothesis iterations bootstrapCount plant =

    //     printfn "Bootstrapping parameter estimation for %s (x%i)" plant.Identifier bootstrapCount

    //     let rec bootstrap obs numberOfTimes solutions =
    //         if (numberOfTimes > 0) then
    //             let subset = removeSingle obs
    //             let result = estimate' hypothesis iterations obs
    //             printfn "%s: completed bootstrap %i" plant.Identifier numberOfTimes
    //             bootstrap obs (numberOfTimes - 1) (solutions |> List.append [result])
    //         else solutions

    //     let estimates = bootstrap plant.Chronology bootstrapCount []
    //     plant, estimates

    // // <summary>
    // // Boostrapping removes a single observation from the time series, and estimates parameters 'n' times.
    // // </summary>
    // // <returns>Details of the plant invidual, alongside an array of parameter estimates.</returns>
    // let bootstrapParallel hypothesis iterations bootstrapCount plants =
    //     List.toArray plants |> Array.Parallel.map (bootstrap hypothesis iterations bootstrapCount)    
    
    // // <summary>
    // // Estimate parameters once for the given model system and plant individual.
    // // </summary>
    // let estimateCoupled hypothesis iterations plant =
    //     printfn "%s: Estimating parameters" plant.Identifier
    //     match plant.Predictor with 
    //     | Some x -> plant, (estimateCoupled' hypothesis iterations plant.Chronology x)
    //     | None -> plant, (estimateCoupled' hypothesis iterations plant.Chronology []) // TODO Clean up


    // // <summary>
    // // Estimate parameters for many plant individuals in parallel
    // // </summary>
    // let estimateCoupledParallel hypothesis iterations plants =
    //     List.toArray plants |> Array.Parallel.map (estimateCoupled hypothesis iterations)


    // let estimateCoupledParallelWindowed hypothesis windowSize iterations plants =

    //     let parallelProcessWindow x y =
    //         //printfn "Starting bin number %i" x
    //         let solution = estimateCoupled' hypothesis iterations x y
    //         printfn "%f" (fst solution)
    //         //printfn "Finished bin #%i: -loglikelihood=%f" x (fst solution)
    //         solution

    //     let windows = 
    //         plants
    //         |> List.map (fun plant -> Seq.windowed windowSize plant.Chronology |> Seq.toList, Seq.windowed windowSize plant.Predictor.Value |> Seq.toList)

    //     // Plant * List of arrays
    //     windows.[0]


    // // <summary>
    // // Boostrapping removes a single observation from the time series, and estimates parameters 'n' times.
    // // </summary>
    // // <returns>Details of the plant invidual, alongside an array of parameter estimates.</returns>
    // let bootstrapCoupled hypothesis iterations bootstrapCount (plant:PlantIndividual) =

    //     printfn "Bootstrapping parameter estimation for %s (x%i)" plant.Identifier bootstrapCount

    //     // A. Setup time series on common timescale (to know how many are available to remove)
    //     let predictor =
    //         match plant.Predictor with 
    //         | Some x -> x
    //         | None -> [] // TODO Clean up
    //     let commonTimeScale = commonTimeScale plant.Chronology predictor
    //     let responseOnCommonTime = plant.Chronology |> List.filter(fun x -> (commonTimeScale |> Seq.exists (fun y -> y = (fst x))))
    //     let predictorOnCommonTime = predictor |> List.filter(fun x -> (commonTimeScale |> Seq.exists (fun y -> y = (fst x))))

    //     // B. Bootstrap function
    //     let rec bootstrap (resp:GrowthChronology) (pred:PredictorChronology) numberOfTimes solutions =
    //         let respBoot,predBoot = removeSingleCoupled resp pred
    //         let result = estimateCoupled' hypothesis iterations respBoot predBoot
    //         printfn "%s: completed bootstrap %i" plant.Identifier numberOfTimes
    //         if (numberOfTimes > 0) then bootstrap responseOnCommonTime predictorOnCommonTime (numberOfTimes - 1) (solutions |> List.append [result])
    //         else solutions
        
    //     let pred = 
    //         match plant.Predictor with 
    //         | Some x -> x
    //         | None -> [] // TODO fail here

    //     let estimates = bootstrap plant.Chronology pred bootstrapCount []
    //     plant, estimates

    // // <summary>
    // // Boostrapping removes a single observation from the time series, and estimates parameters 'n' times.
    // // </summary>
    // // <returns>Details of the plant invidual, alongside an array of parameter estimates.</returns>
    // let bootstrapCoupledParallel hypothesis iterations bootstrapCount plants =
    //     List.toArray plants |> Array.Parallel.map (bootstrapCoupled hypothesis iterations bootstrapCount)
