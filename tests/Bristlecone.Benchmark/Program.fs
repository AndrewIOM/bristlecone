module Program

//open Hopac
//open Logary.Configuration
//open Logary.Adapters.Facade
//open Logary.Targets
open Expecto
open MathNet.Numerics

type Point = float[]

/// Represents an n-dim objective to minimise with known minima
type OptimisationProblem = {
  Fn: Point -> float
  Minima: Point list
  MinValue: float
  StartingPoints: Point list
}

module Config =

  // We use a fixed random seed so that the results are reproducible
  let randomSeed = 20346
  let rng = Random.MersenneTwister(randomSeed, threadSafe = true)
  let startPointCount = 100


module TestSuite =

  open TestFunctions.LocalMinima

  let between x y = Distributions.ContinuousUniform(x,y,Config.rng).Sample()

  let hypercube d low high = 
    [|1 .. d|] |> Array.map(fun _ -> between low high)

  let twoDimension low high =
    [between low high; between low high]

  let twoDim f (x:float[]) = f x.[0] x.[1]
  let oneDim f (x:float[]) = f x.[0]

  let nDimensional = [
      fun d -> ackley <| hypercube d -32.768 32.768
      fun d -> griewank <| hypercube d -600. 600.
      fun d -> rastrigin <| hypercube d -5.12 5.12
  ]

  // Functions, input domain, global minimum, and global minimum point(s)
  let fixedDimension = [
    bukinSixth |> twoDim,   [ 1 .. Config.startPointCount ] |> List.map(fun _ -> [(between -15. -5.); (between -3. 3.)]), 0., [[-10.; 1.]]
    crossInTray |> twoDim,  [ 1 .. Config.startPointCount ] |> List.map(fun _ -> twoDimension -10. 10.), -2.06261, [[1.3491; -1.3491]; [1.3491; 1.3491]; [-1.3491; 1.3491]; [-1.3491; -1.3491]]
    //dropWave |> twoDim,     [ 1 .. Config.startPointCount ] |> List.map(fun _ -> twoDimension -5.12 5.12), -1., [[0.;0.]]
    eggHolder |> twoDim,    [ 1 .. Config.startPointCount ] |> List.map(fun _ -> twoDimension -5.12 5.12), -959.6407, [[512.; 404.2319]]
    //gramacyLee |> oneDim,   [ 1 .. Config.startPointCount ] |> List.map(fun _ -> [between 0.5 2.5]), -0.869011134989500, [[0.548563444114526]]
    holderTable |> twoDim,  [ 1 .. Config.startPointCount ] |> List.map(fun _ -> twoDimension -10. 10.), -19.2085, [[8.05502; 9.66459]; [8.05502; -9.66459]; [-8.05502; 9.66459]; [-8.05502; -9.66459]]
    //langermann |> twoDim,   [ 1 .. Config.startPointCount ] |> List.map(fun _ -> twoDimension 0. 10.), -5.1621259, [[2.00299219; 1.006096]]
  ]



// 1. Check functions resolve correctly
[<Tests>]
let functionTests =
  TestSuite.fixedDimension
  |> List.mapi(fun i (f,initial,minVal,minima) ->
      test (sprintf "Fixed-dimension model %i is correctly defined" i) {
        let results = minima |> List.map (List.toArray >> f)
        results |> List.iter (fun r -> Expect.floatClose Accuracy.medium r minVal "Close")
        //Expect.allEqual results minVal (sprintf "The global minimum %f was not calculated correctly at points %A" minVal results)        
      }
  ) |> testList "Fixed-dimension functions" |> testSequenced


// All of Bristlecone's built-in optimisation methods.
// [ Specific settings at default values ]

open Bristlecone.Optimisation

let annealSettings = 
  { MonteCarlo.SimulatedAnnealing.AnnealSettings<float>.Default with 
      BoilingAcceptanceRate = 0.85
      HeatRamp = (fun t -> t + sqrt t); TemperatureCeiling = Some 500.
      HeatStepLength = EndConditions.afterIteration 1000
      AnnealStepLength = (fun x -> MonteCarlo.SimulatedAnnealing.EndConditions.improvementCount 5000 250 x || EndConditions.afterIteration 10000 x) }

let optimFunctions = [
  "amoeba single",          Amoeba.Solver.solve Amoeba.Solver.Default
  "amoeba swarm",           Amoeba.swarm 5 20 Amoeba.Solver.Default
  "anneal classic",         MonteCarlo.SimulatedAnnealing.classicalSimulatedAnnealing 0.01 false annealSettings
  "anneal cauchy",          MonteCarlo.SimulatedAnnealing.fastSimulatedAnnealing 0.01 false annealSettings
  "filzbach",               MonteCarlo.Filzbach.filzbach { TuneAfterChanges = 1; MaxScaleChange = 0.1; MinScaleChange = 0.1; BurnLength = EndConditions.afterIteration 10000 }
  "automatic MCMC",         MonteCarlo.``Automatic (Adaptive Diagnostics)``
  "metropolis-gibbs",       MonteCarlo.``Metropolis-within Gibbs``
  "adaptive metropolis",    MonteCarlo.adaptiveMetropolis 0.250 500
  "random walk MCMC",       MonteCarlo.randomWalk []
  "random walk w/ tuning",  MonteCarlo.randomWalk [ MonteCarlo.TuneMethod.CovarianceWithScale 0.25, 500, EndConditions.afterIteration 10000 ]  
]

let logger = Bristlecone.Logging.Console.logger(1000)
let endCondition = EndConditions.afterIteration 100000
let accuracy = Accuracy.low

let optimTests =
  optimFunctions
  |> List.collect(fun (optimName,optimise) ->
    TestSuite.fixedDimension
    |> List.mapi(fun i (f,initials,minVal,minima) ->
        test (sprintf "Model %i with optimiser: %s" i optimName) {

          let startPoints = 
            initials |> List.map(fun start ->
              start |> List.map(fun i -> i, i + 0.001, Bristlecone.Parameter.Constraint.Unconstrained) |> List.toArray)

          let trace = optimise Config.rng logger endCondition (startPoints.[0]) f
          let minimum = trace |> Seq.minBy(fun s -> s |> fst)
          if not <| System.Double.IsNaN minVal then Expect.floatClose accuracy (minimum |> fst) minVal "Did not find global minimum"
          if minima.Length > 0 then
            // Find closest global minimum
            let closest = minima |> List.minBy(fun m -> minimum |> snd |> Array.zip (m |> List.toArray) |> Array.sumBy(fun (x1,x2) -> if x1 > x2 then x1 - x2 else x2 - x1))
            Expect.all (closest |> List.toArray |> Array.zip (minimum |> snd)) (fun (x1,x2) ->
              x2 - x1 < 1.0
            ) "The identified minimum was not near a global minimum"
        }
      )
    )

[<Tests>]
let benchmarks = optimTests |> testList "Benchmarks" |> testSequenced
// TODO run n-dimensional tests in multiple dimensions

[<EntryPoint>]
let main argv =
//  let logary =
//    Config.create "Bristlecone.Tests" "localhost"
//    |> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
//    |> Config.processing (Events.events |> Events.sink ["console";])
//    |> Config.build
//    |> run
//  LogaryFacadeAdapter.initialise<Expecto.Logging.Logger> logary

  runTestsInAssemblyWithCLIArgs [] argv