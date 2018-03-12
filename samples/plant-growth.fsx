#r "../packages/NETStandard.Library.NETFramework/build/net461/lib/netstandard.dll"
#r "../packages/FSharp.Data/lib/net45/FSharp.Data.dll"
#r "../src/DendroFit/bin/Debug/netstandard2.0/Microsoft.Research.Oslo.dll"
#r "../src/DendroFit/bin/Debug/netstandard2.0/DendroFit.dll"

///////////////////////////////////
/// Shrub Modelling with DendroFit
///////////////////////////////////

(* Some description of the modelling procedure. *)

open DendroFit
open Types.ParameterEstimation
open Types
open Time

/////////////////////////////////////////
// HELPER STUFF - TO MOVE INTO DENDROFIT

let growthSeries plant =
    match plant with
    | PlantIndividual.RingWidth rw ->
        match rw with
        | Absolute rws -> TimeSeries.map rws (fun (x,t) -> removeUnit x, t) |> Absolute
        | Cumulative _ -> invalidOp "Not implemented"
    | _ -> invalidOp "Not implemented"

// END HELPER STUFF
/////////////////////////////////////////

// 1. Setup model system

let eq (p:CodedMap<Parameter>) _ x _ =
    (p |> ParameterPool.value "r") * x * ( 1. - (x / (p |> ParameterPool.value "k")))

let l parameters (predictions: CodedMap<PredictedSeries>) =
    let predicted = predictions.Item (ShortCode.create "x")
    ModelLibrary.Likelihood.sumOfSquares parameters predicted.Expected predicted.Observed

let p = [ ShortCode.create "r", NotEstimated (0.01 ,0.2 )
          ShortCode.create "k", NotEstimated (1., 10.) ] |> Map.ofList

let system = { Equations = [ShortCode.create "x", eq] |> Map.ofList; Likelihood = l; Parameters = p }

// 2. Load Data
let yuribei = DataAccess.Shrub.loadRingWidths "/Users/andrewmartin/Documents/DPhil Zoology/Modelling/DendroFit/samples/yuribei-rw.csv"

// 3. Predict
yuribei.[1..1] |> List.map (fun x -> DendroFit.estimate Annual 1 system (growthSeries x.Growth))


///////////////////
/// EXAMPLE 2. Coupled Model
///////////////////

// 1. Load in Environmental Data
let d15N = DataAccess.Shrub.loadLocalEnvironmentVariable "/Users/andrewmartin/Documents/DPhil Zoology/Modelling/DendroFit/samples/yuribei-d15N.csv"
    
let shrubData = 
    yuribei
    |> Seq.map (fun s -> s.Identifier.Value, s)
    |> Seq.keyMatch d15N
    |> Seq.map (fun (_,plant,d15N) -> PlantIndividual.zipEnv (ShortCode.create "d15N") plant d15N)
    |> Seq.toList

// 2. Define N-growth models

let dxdt (p:CodedMap<Parameter>) _ x environment =
     (p |> ParameterPool.value "r") * (p |> ParameterPool.value "a") * x * ( 1. - (x / (p |> ParameterPool.value "k")))

let dndt (p:CodedMap<Parameter>) _ x environment =
    2.

let pCoupled = [ ShortCode.create "r", NotEstimated (0.01 / 365.00 ,0.5 / 365.00 )
                 ShortCode.create "k", NotEstimated (1., 100.)
                 ShortCode.create "a", NotEstimated (1., 100.) ] |> Map.ofList

let systemCoupled = { Equations = [dxdt; dndt]; Likelihood = l; Parameters = pCoupled }



let brusselator t x1 x2 = (1.0+x1*x1*x2-4.0*x1, 3.0*x1-x1*x1*x2) // right part of the ODE
let solution = ODE.Oslo.solve2D 0.0 1.01 3.0 brusselator 20.0 1. // initial time and x1,x2; right part; end time; step.

solution |> Array.map (fun x -> x.X.ToArray())