module Program

open Expecto
open MathNet.Numerics
open Bristlecone

type Point = float[]

/// Represents an n-dim objective to minimise with known minima
type OptimisationProblem =
    { Fn: Point -> float
      Minima: Point list
      MinValue: float
      StartingPoints: Point list }

module Config =

    // We use a fixed random seed so that the results are reproducible
    let randomSeed = 20346
    let rng = Random.MersenneTwister(randomSeed, threadSafe = true) :> System.Random
    let startPointCount = 10
    let accuracy = Accuracy.high
    let likTol = 1.0<``-logL``>
    let paramTol = 0.1


module TestSuite =

    open TestFunctions.LocalMinima
    open Bristlecone.Tensors

    let between x y =
        Statistics.Distributions.ContinuousUniform.draw Config.rng x y ()

    let hypercube d low high =
        [| 1..d |] |> Array.map (fun _ -> between low high)

    let twoDimension low high = [ between low high; between low high ]

    let twoDim f (x: float[]) = f x.[0] x.[1]
    let oneDim f (x: float[]) = f x.[0]
    let multidim f (x: float[]) = f x
    let twoDimT f (x: TypedTensor<Vector, 'a>) = f (Typed.itemAt 0 x) (Typed.itemAt 1 x)

    // Functions, input domain, global minimum, and global minimum point(s)
    let fixedDimension =
        [ "Ackley (2D)", ackley |> multidim, [ (-32.768, 32.768); (-32.768, 32.768) ], 0., [ [ 0.; 0. ] ]
          "Bukin Sixth", bukinSixth |> twoDim, [ (-15., -5.); (-3., 3.) ], 0., [ [ -10.; 1. ] ]
          "Griewank", griewank |> multidim, [ -600., 600.; -600., 600. ], 0., [ [ 0.; 0. ] ]
          "Holder Table",
          holderTable |> twoDim,
          [ (-10., 10.); (-10., 10.) ],
          -19.20850257,
          [ [ 8.05502; 9.66459 ]
            [ 8.05502; -9.66459 ]
            [ -8.05502; 9.66459 ]
            [ -8.05502; -9.66459 ] ]
          "Cross in tray",
          crossInTray |> twoDim,
          [ (-10., 10.); (-10., 10.) ],
          -2.06261185,
          [ [ 1.3491; -1.3491 ]
            [ 1.3491; 1.3491 ]
            [ -1.3491; 1.3491 ]
            [ -1.3491; -1.3491 ] ]
          "Dropwave", dropWave |> twoDim, [ (-5.12, 5.12); (-5.12, 5.12) ], -1., [ [ 0.; 0. ] ]
          "Eggholder", eggHolder |> twoDim, [ (-512., 512.); (-512., 512.) ], -959.6406627, [ [ 512.; 404.2319 ] ]
          "Gramarcy-Lee", gramacyLee |> oneDim, [ (0.5, 2.5) ], -0.869011134989500, [ [ 0.548563444114526 ] ]
          "Langermann", langermann |> twoDim, [ (0., 10.); (0., 10.) ], -5.1621259, [ [ 2.00299219; 1.006096 ] ] ]

    // let nDimensional = [
    //     fun d -> ackley <| hypercube d -32.768 32.768
    //     fun d -> griewank <| hypercube d -600. 600.
    //     fun d -> rastrigin <| hypercube d -5.12 5.12
    // ]

// 1. Check functions resolve correctly
[<Tests>]
let functionTests =
    TestSuite.fixedDimension
    |> List.mapi (fun i (name, f, initial, minVal, minima) ->
        test (sprintf "Fixed-dimension model %i is correctly defined" i) {
            minima
            |> List.map (List.toArray >> f)
            |> List.iter (fun r -> Expect.floatClose Config.accuracy r minVal "Close")
        })
    |> testList "Fixed-dimension functions"
    |> testSequenced


open Bristlecone.Optimisation

let measureTime fn =
    let watch = System.Diagnostics.Stopwatch.StartNew()
    watch.Start()
    let result = fn ()
    watch.Stop()
    result, watch.Elapsed.Milliseconds

let runReplicated nTimes work =
    [| 1..nTimes |] |> Array.Parallel.map (fun _ -> measureTime (fun () -> work ()))

let logger = Logging.ConsoleTable.logger 1000<iteration>
let endCondition = EndConditions.atIteration 10000<iteration> // EndConditions.Profiles.mcmc 10000<iteration> logger
let accuracy = { absolute = 1e-3; relative = 1e-2 }

type BenchmarkResult =
    { ModelName: string
      OptimMethod: string
      Runs: int
      MinimumReal: float<``-logL``>
      MinimumRealPoints: float list list
      MinimumEstimatedMedian: float<``-logL``>
      MinimumEstimatedBest: float<``-logL``>
      DistanceFromMinimumBest: float<``-logL``>
      ParameterSpaceDistanceMedian: float<``optim-space``>
      ParameterEstimates: EstimatedP list
      TimesNearMinimum: int
      MillisecondsMedian: float }

and EstimatedP =
    { Best: float
      Median: float
      StDev: float }

let median<[<Measure>] 'u> (x: float<'u> seq) =
    x
    |> Seq.map float
    |> Statistics.Statistics.Median
    |> LanguagePrimitives.FloatWithMeasure<'u>

let summariseRuns
    modelName
    (optimName: string)
    trueMinima
    minVal
    (minima: float list list)
    (results: ((EstimationEngine.Solution list * float[]) * int) seq)
    =
    let eachRunMinimum =
        results |> Seq.map (fun ((r, _), _) -> r |> Seq.minBy fst)

    let distancesFromRealMinimum =
        eachRunMinimum |> Seq.map (fun o -> abs ((fst o) - minVal))

    let bestPointEstimate =
        eachRunMinimum
        |> Seq.minBy (fun o -> abs ((fst o) - minVal))
        |> snd
        |> Tensors.Typed.toFloatArray

    let elapsedAverage = results |> Seq.map (fun (_, t) -> float t) |> median

    let dims = minima |> Seq.head |> Seq.length

    let paramEstimates =
        [ 1..dims ]
        |> List.map (fun d ->
            let values =
                eachRunMinimum |> Seq.map (fun (_, v) -> (Tensors.Typed.toFloatArray v).[d - 1])

            { Best = bestPointEstimate.[d - 1] |> float
              Median = median (values |> Seq.map float)
              StDev = Statistics.Statistics.StandardDeviation(values |> Seq.map float) |> float })

    let distanceToClosestParamValue =
        eachRunMinimum
        |> Seq.map (fun (_, v) ->
            minima
            |> Seq.map (fun minimum ->
                Seq.zip minimum (Tensors.Typed.toFloatArray v)
                |> Seq.map (fun (x, y) -> abs (x - float y))
                |> Seq.sum)
            |> Seq.min)

    { ModelName = modelName
      OptimMethod = optimName
      Runs = results |> Seq.length
      MinimumReal = minVal
      MinimumRealPoints = trueMinima
      MinimumEstimatedMedian = eachRunMinimum |> Seq.map fst |> median
      MinimumEstimatedBest = eachRunMinimum |> Seq.map fst |> Seq.min
      DistanceFromMinimumBest = distancesFromRealMinimum |> Seq.min
      ParameterSpaceDistanceMedian = distanceToClosestParamValue |> median |> (*) 1.<``optim-space``>
      ParameterEstimates = paramEstimates
      TimesNearMinimum =
        distancesFromRealMinimum
        |> Seq.filter (fun i -> i < 0.1<``-logL``>)
        |> Seq.length
      MillisecondsMedian = elapsedAverage }

let runOptimTests optimFunctions =
    optimFunctions
    |> List.collect (fun (optimName, optimise) ->
        TestSuite.fixedDimension
        |> List.map (fun (modelName, f, domain, minVal, minima) ->
            let domain =
                domain
                |> List.map (fun (min, max) ->
                    min * 1.<``optim-space``>, max * 1.<``optim-space``>, Parameter.Constraint.Unconstrained)
                |> List.toArray

            runReplicated Config.startPointCount (fun () ->
                let startPoint =
                    domain
                    |> Array.map (fun (min, max, _) -> TestSuite.between (float min) (float max))
                    |> Array.map ((*) 1.<``optim-space``>)
                    |> Tensors.Typed.ofVector

                let result: list<EstimationEngine.Solution> =
                    // Using a shim to go between tensor space and float space
                    let f' point =
                        f (Tensors.Typed.toFloatArray point |> Array.map float) * 1.<``-logL``>
                        |> Tensors.Typed.ofScalar

                    match optimise with
                    | EstimationEngine.Optimisation.InTransformedSpace optim ->
                        optim Config.rng logger endCondition domain (Some startPoint) f'
                    | EstimationEngine.Optimisation.InDetachedSpace optim ->
                        optim Config.rng logger endCondition domain (Some startPoint) f'

                result, startPoint |> Tensors.Typed.toFloatArray |> Array.map float)
            |> summariseRuns modelName optimName minima (minVal * 1.<``-logL``>) minima))

[<Measure>]
type ms

type BenchmarkResultFullModel =
    { ModelName: string
      OptimMethod: string
      Runs: int
      SuccessfulRuns: int
      DistanceFromLikelihoodMedian: float
      MillisecondsMedian: float<ms>
      Parameters: Test.ParameterTestResult list list }


module Metrics =

    /// Relative error
    let relError trueVal estVal =
        if trueVal = 0.0 then
            abs estVal
        else
            abs (estVal - trueVal) / abs trueVal

    /// Metrics for an indiviual model/optim run.
    let runMetrics (likTol: float<``-logL``>) (paramTol: float) ((run: Test.TestResult<'a, 'b, 'c, 'u>, time: int)) =
        let likelihoodGap = run.EstimatedLikelihood - run.RealLikelihood

        let paramErrors =
            run.Parameters
            |> List.map (fun p -> relError (float p.RealValue) (float p.EstimatedValue))

        let paramRMSE = sqrt (List.averageBy (fun e -> e * e) paramErrors)
        let success = likelihoodGap <= likTol && paramRMSE <= paramTol

        {| LikelihoodGap = likelihoodGap
           ParamErrors = paramErrors
           ParamN = run.Parameters.Length
           IterationN = run.IterationsRun
           ParamRMSE = paramRMSE
           Success = success
           TimeMs = time |}

    // TODO Harmonise with earlier unit-based definition.
    let median xs =
        let sorted = xs |> List.sort
        let n = sorted.Length

        if n % 2 = 1 then
            sorted.[n / 2]
        else
            (sorted.[n / 2 - 1] + sorted.[n / 2]) / 2.0

    /// Metrics for all runs for an indiviual model/optim combination.
    let summariseRuns likTol paramTol runs =
        let metrics = runs |> Array.map (runMetrics likTol paramTol) |> Array.toList

        let successPct =
            100.0 * float (metrics |> List.filter (fun m -> m.Success) |> List.length)
            / float runs.Length

        let medLikGap = metrics |> List.map (fun m -> m.LikelihoodGap |> float) |> median
        let medRMSE = metrics |> List.map (fun m -> m.ParamRMSE) |> median
        let medTime = metrics |> List.map (fun m -> m.TimeMs |> float) |> median
        let medIter = metrics |> List.map (fun m -> m.IterationN |> float) |> median

        {| SuccessPct = successPct
           MedianLikelihoodGap = medLikGap
           MedianParamRMSE = medRMSE
           MedianTimeMs = int medTime
           RunN = runs.Length
           MedianIterations = int medIter
           PerParamErrors =
            metrics
            |> List.collect (fun m -> m.ParamErrors)
            |> List.chunkBySize metrics.Head.ParamN
            |> List.map median |}


module TimeSeriesTests =

    let settings =
        Test.annualSettings
        |> Test.addNoise (Test.Noise.tryAddNormal "σ[y]" "predator")
        |> Test.addNoise (Test.Noise.tryAddNormal "σ[x]" "prey")
        |> Test.addGenerationRules
            [ Test.GenerationRules.alwaysLessThan 100000. "predator"
              Test.GenerationRules.alwaysMoreThan 10. "predator"
              Test.GenerationRules.alwaysLessThan 100000. "prey"
              Test.GenerationRules.alwaysMoreThan 10. "prey" ]
        |> Test.withTimeSeriesLength 30

    /// An engine that uses annual-based data in an
    /// annual-based model.
    /// Inserts a time conversion from int-based years (data) to float-based years (model).
    let engine optimise =
        Bristlecone.mkContinuous ()
        |> Bristlecone.withCustomOptimisation optimise
        |> Bristlecone.withOutput logger
        |> Bristlecone.withTimeConversion Time.DateMode.Conversion.Annual.toYears

    let runTimeSeriesTests (timeModels: ('a * ModelSystem.ModelSystem<Time.year> * 'b) list) optimFunctions =
        List.allPairs optimFunctions timeModels
        |> List.map (fun ((optimName: string, optimise), (modelName, modelFn, startValues)) ->
            runReplicated Config.startPointCount (fun () ->
                let settings = settings |> Test.addStartValues startValues
                Bristlecone.testModel (engine optimise) settings modelFn)
            |> fun r -> modelName, optimName, Metrics.summariseRuns Config.likTol Config.paramTol r)


// MODELS / OPTIM Fn
// -------

let annealSettings =
    { MonteCarlo.SimulatedAnnealing.AnnealSettings.Default with
        BoilingAcceptanceRate = 0.85
        HeatRamp = (fun t -> t + sqrt t)
        TemperatureCeiling = Some 500.
        HeatStepLength = EndConditions.atIteration 10000<iteration>
        AnnealStepEnd = fun x -> EndConditions.atIteration 10000<iteration> x }

let optimFunctions =
    [ //"amoeba single", Amoeba.single Amoeba.Solver.Default
    //   "amoeba swarm", Amoeba.swarm 5 20 Amoeba.Solver.Default
      //   "anneal classic", MonteCarlo.SimulatedAnnealing.classicalSimulatedAnnealing 0.01<``optim-space``> false annealSettings
    //   "anneal cauchy", MonteCarlo.SimulatedAnnealing.fastSimulatedAnnealing 0.01<``optim-space``> false annealSettings
      "filzbach",
      MonteCarlo.Filzbach.filzbach
          { TuneAfterChanges = 10000
            MaxScaleChange = 0.5
            MinScaleChange = 0.5
            BurnLength = EndConditions.atIteration 100000<iteration> }
    //   "automatic MCMC", MonteCarlo.``Automatic (Adaptive Diagnostics)``
      //   "metropolis-gibbs", MonteCarlo.``Metropolis-within Gibbs``
    //   "adaptive metropolis", MonteCarlo.adaptiveMetropolis 0.250 500<iteration>
    //   "random walk MCMC", MonteCarlo.randomWalk []
    //   "random walk w/ tuning",
    //   MonteCarlo.randomWalk
    //       [ { Method = MonteCarlo.TuneMethod.CovarianceWithScale 0.25
    //           Frequency = 500<iteration>
    //           EndCondition = EndConditions.Profiles.mcmcTuningStep 50000<iteration> logger } ] ]
    ]

let timeModels () =
    [ //"predator-prey", TestFunctions.Timeseries.PredatorPrey.``predator-prey`` (), [ "predator", 1.0; "prey", 1.0 ]
      "predator-prey (noisy)",
      TestFunctions.Timeseries.PredatorPrey.``predator-prey [with noise]`` (),
      [ "predator", 75.; "prey", 50. ] ]

module Output =

    let markdown optimTable timeSeriesTable =
        sprintf
            "
Bristlecone Benchmarks
===\n
## Optimisation Methods\n\n%s\n
## Full Model Systems\n\n### Time-series models\n\n%s"
            optimTable
            timeSeriesTable

    let markdownTable columnNames rowData =
        sprintf
            "| %s |\n| %s |\n%s"
            (columnNames |> String.concat " | ")
            (columnNames |> Seq.map (fun _ -> "---") |> String.concat " | ")
            (rowData
             |> Seq.map (fun d -> sprintf "| %s |" (d |> String.concat " | "))
             |> String.concat "\n")

    let fileName = "../../benchmarks.md"

    let paramEstimatesToCell (p: EstimatedP list) =
        p
        |> Seq.map (fun p -> sprintf "best %.3f / median %.3f (±%.3f)" p.Best p.Median p.StDev)
        |> String.concat "<br>"

    let threeDp (f: float<'u>) = sprintf "%.3f" f

    let summariseParameters (p: list<list<Bristlecone.Test.ParameterTestResult>>) =
        p
        |> List.collect id
        |> List.groupBy (fun g -> g.Identifier)
        |> List.map (fun (g, v) ->
            let distancesFromReal =
                (v |> Seq.map (fun x -> abs (x.EstimatedValue - x.RealValue)))

            sprintf
                "[%s] %.3f best / %.3f median (±%.3f)"
                g
                (distancesFromReal |> Seq.min)
                (distancesFromReal |> median)
                (distancesFromReal |> Seq.map float |> Statistics.Statistics.StandardDeviation))
        |> String.concat "<br>"

    let pointsToCell points =
        points
        |> List.map (fun l -> l |> List.map threeDp |> String.concat " / ")
        |> String.concat "<br>"

[<EntryPoint>]
let main argv =

    let testSuccess = runTestsInAssemblyWithCLIArgs [] argv

    if testSuccess = 0 then
        let optimResults = runOptimTests optimFunctions

        let optimTable =
            Output.markdownTable
                [ "Model name"
                  "Optimisation method"
                  "n runs"
                  "Target minimum"
                  "Best estimated minimum"
                  "Median estimated minimum"
                  "Sum distance from true parameters"
                  "Parameter estimates (individual)"
                  "True solutions"
                  "Milliseconds taken (median)" ]
                (optimResults
                 |> Seq.map (fun o ->
                     [ o.ModelName
                       o.OptimMethod
                       o.Runs.ToString()
                       Output.threeDp o.MinimumReal
                       sprintf
                           "%s (%s -> min)"
                           (Output.threeDp o.MinimumEstimatedBest)
                           (Output.threeDp o.DistanceFromMinimumBest)
                       Output.threeDp o.MinimumEstimatedMedian
                       Output.threeDp o.ParameterSpaceDistanceMedian
                       Output.paramEstimatesToCell o.ParameterEstimates
                       Output.pointsToCell o.MinimumRealPoints
                       o.MillisecondsMedian.ToString() ]))

        System.IO.File.WriteAllText(Output.fileName, Output.markdown optimTable "")

        let timeResults = TimeSeriesTests.runTimeSeriesTests (timeModels ()) optimFunctions //runOptimTests ()

        let timeTable =
            Output.markdownTable
                [ "Model"
                  "Optimisation method"
                  "Success %"
                  "n"
                  "Likelihood gap (med, tol=1.0)"
                  "Param RMSE (rel, median)"
                  "n iterations (median)"
                  "Time to solution (ms, median)" ]
                (timeResults
                 |> Seq.map (fun (modelName, optimName, metrics) ->
                     [ modelName
                       optimName
                       Output.threeDp metrics.SuccessPct + "%"
                       metrics.RunN.ToString()
                       Output.threeDp metrics.MedianLikelihoodGap
                       Output.threeDp metrics.MedianParamRMSE
                       //Output.summariseParameters metrics.MedianParamRMSE
                       metrics.MedianIterations.ToString()
                       metrics.MedianTimeMs.ToString() ]))

        System.IO.File.WriteAllText(Output.fileName, Output.markdown optimTable timeTable)
        0
    else
        testSuccess
