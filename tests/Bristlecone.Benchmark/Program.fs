module Program

open Expecto
open MathNet.Numerics

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
    let rng = Random.MersenneTwister(randomSeed, threadSafe = true)
    let startPointCount = 10
    let accuracy = Accuracy.high


module TestSuite =

    open TestFunctions.LocalMinima

    let between x y =
        Distributions.ContinuousUniform(x, y, Config.rng).Sample()

    let hypercube d low high =
        [| 1..d |] |> Array.map (fun _ -> between low high)

    let twoDimension low high = [ between low high; between low high ]

    let twoDim f (x: float[]) = f x.[0] x.[1]
    let oneDim f (x: float[]) = f x.[0]


    // Functions, input domain, global minimum, and global minimum point(s)
    let fixedDimension =
        [ "Bukin Sixth",    bukinSixth |> twoDim,   [ (-15., -5.); (-3., 3.) ], 0., [ [ -10.; 1. ] ]
        //   "Holder Table",   holderTable |> twoDim,  [ (-10., 10.); (-10., 10.) ], -19.20850257, [[8.05502; 9.66459]; [8.05502; -9.66459]; [-8.05502; 9.66459]; [-8.05502; -9.66459]]
          "Cross in tray",  crossInTray |> twoDim,  [ (-10., 10.); (-10., 10.) ], -2.06261185, [[1.3491; -1.3491]; [1.3491; 1.3491]; [-1.3491; 1.3491]; [-1.3491; -1.3491]]
        //   "Dropwave", dropWave |> twoDim,     [ 1 .. Config.startPointCount ] |> List.map(fun _ -> twoDimension -5.12 5.12), -1., [[0.;0.]]
        //   "Eggholder", eggHolder |> twoDim,    [ 1 .. Config.startPointCount ] |> List.map(fun _ -> twoDimension -5.12 5.12), -959.6406627, [[512.; 404.2319]]
        //   "Gramarcy-Lee", gramacyLee |> oneDim,   [ 1 .. Config.startPointCount ] |> List.map(fun _ -> [between 0.5 2.5]), -0.869011134989500, [[0.548563444114526]]
        //   "Langermann", langermann |> twoDim,   [(0., 10.); (0., 10.)], -5.1621259, [[2.00299219; 1.006096]]
        ]

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


// All of Bristlecone's built-in optimisation methods.
// [ Specific settings at default values ]

open Bristlecone.Optimisation

let annealSettings =
    { MonteCarlo.SimulatedAnnealing.AnnealSettings<float>.Default with
        BoilingAcceptanceRate = 0.85
        HeatRamp = (fun t -> t + sqrt t)
        TemperatureCeiling = Some 500.
        HeatStepLength = EndConditions.afterIteration 100000
        AnnealStepLength =
            (fun x -> (*MonteCarlo.SimulatedAnnealing.EndConditions.improvementCount 5000 1000 x || *)
                EndConditions.afterIteration 10000 x) }

let optimFunctions =
    [ "amoeba single",          Amoeba.Solver.solve Amoeba.Solver.Default
      "amoeba swarm",           Amoeba.swarm 5 20 Amoeba.Solver.Default
      "anneal classic",         MonteCarlo.SimulatedAnnealing.classicalSimulatedAnnealing 0.01 false annealSettings
      "anneal cauchy",          MonteCarlo.SimulatedAnnealing.fastSimulatedAnnealing 0.01 false annealSettings
    //   "filzbach",               MonteCarlo.Filzbach.filzbach { TuneAfterChanges = 10000; MaxScaleChange = 0.5; MinScaleChange = 0.5; BurnLength = EndConditions.afterIteration 100000 }
      // "automatic MCMC",         MonteCarlo.``Automatic (Adaptive Diagnostics)``
      // "metropolis-gibbs",       MonteCarlo.``Metropolis-within Gibbs``
      "adaptive metropolis",    MonteCarlo.adaptiveMetropolis 0.250 500
      "random walk MCMC",       MonteCarlo.randomWalk []
      // "random walk w/ tuning",  MonteCarlo.randomWalk [ MonteCarlo.TuneMethod.CovarianceWithScale 0.25, 500, EndConditions.afterIteration 10000 ]
      ]

let logger = Bristlecone.Logging.Console.logger (10000)
let endCondition = EndConditions.afterIteration 100000
let accuracy = { absolute = 1e-3; relative = 1e-2 }

type BenchmarkResult = {
    ModelName: string
    OptimMethod: string
    Runs: int
    MinimumReal: float
    MinimumEstimatedMedian: float
    MinimumEstimatedBest: float
    ParameterSpaceDistanceMedian: float
    ParameterEstimates: EstimatedP list
    TimesNearMinimum: int
    MillisecondsMedian: float
}

and EstimatedP = { Best: float; Median: float; StDev: float }

let median (x:float seq) =
    Statistics.Statistics.Median x

let summariseRuns modelName optimName minVal minima (results: ((float * float array) list * int64 * 'c) seq) =
    let eachRunMinimum = results |> Seq.map(fun (r,_,_) -> r |> Seq.minBy fst)
    let distancesFromRealMinimum = eachRunMinimum |> Seq.map (fun o -> abs((fst o) - minVal))
    let bestPointEstimate = eachRunMinimum |> Seq.minBy (fun o -> abs((fst o) - minVal)) |> snd
    let elapsedAverage = results |> Seq.map(fun (_,t,_) -> float t) |> median
    
    let dims = minima |> Seq.head |> Seq.length
    let paramEstimates =
        [ 1 .. dims ]
        |> List.map(fun d ->
            let values = eachRunMinimum |> Seq.map(fun (_,v) -> v.[d - 1])
            { Best = bestPointEstimate.[d - 1]; Median = median values; StDev = Statistics.Statistics.StandardDeviation(values) }
        )

    let distanceToClosestParamValue =
        eachRunMinimum
        |> Seq.map(fun (_,v) ->
            minima
            |> Seq.map(fun minimum ->
                Seq.zip minimum v
                |> Seq.map(fun (x,y) -> abs(x - y))
                |> Seq.sum )
            |> Seq.min
        )

    {
        ModelName = modelName
        OptimMethod = optimName
        Runs = results |> Seq.length
        MinimumReal = minVal
        MinimumEstimatedMedian = eachRunMinimum |> Seq.map fst |> median
        MinimumEstimatedBest = distancesFromRealMinimum |> Seq.min
        ParameterSpaceDistanceMedian = distanceToClosestParamValue |> median
        ParameterEstimates = paramEstimates
        TimesNearMinimum = distancesFromRealMinimum |> Seq.filter(fun i -> i < 0.1) |> Seq.length
        MillisecondsMedian = elapsedAverage
    }

let runOptimTests () =
    optimFunctions
    |> List.collect (fun (optimName, optimise) ->
        TestSuite.fixedDimension
        |> List.map (fun (modelName, f, domain, minVal, minima) ->
            let domain =
                domain
                |> List.map (fun (min, max) -> min, max, Bristlecone.Parameter.Constraint.Unconstrained)
                |> List.toArray
            [ 1 .. Config.startPointCount ]
            |> List.map (fun _ -> domain |> Array.map (fun (min, max, _) -> TestSuite.between min max))
            |> List.map (fun startPoint ->
                let watch = System.Diagnostics.Stopwatch.StartNew()
                watch.Start()
                let result = optimise Config.rng logger endCondition domain (Some startPoint) f
                watch.Stop()
                result, watch.ElapsedMilliseconds, startPoint)
            |> summariseRuns modelName optimName minVal minima
        ))

let runTimeSeriesTests () =
    let r = 
        TestFunctions.Timeseries.``predator-prey``
        |> Bristlecone.Bristlecone.testModel engine settings
        |> Result.map(fun r ->
            r.Parameters
            )
    ()


[<EntryPoint>]
let main argv = 

    let testSuccess = runTestsInAssemblyWithCLIArgs [] argv
    if testSuccess = 0
    then 
        let testResults = runOptimTests ()
        System.IO.File.WriteAllText("benchmarks.json", System.Text.Json.JsonSerializer.Serialize testResults)
        0
    else testSuccess