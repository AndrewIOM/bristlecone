#r "packages/NETStandard.Library.NETFramework/build/net461/lib/netstandard.dll"
#load "packages/Bristlecone/bristlecone.fsx"
// #load "packages/Bristlecone/charts.fsx"
#r "packages/Bristlecone.Dendro/lib/netstandard2.0/Bristlecone.Dendro.dll"

/////////////////////////////////////
/// Bristlecone Model Tests
/////////////////////////////////////

(* To ensure Bristlecone is effective at solving long-term ecological
   problems, we run a model-fitting-model-selection (MFMS) procedure
   for two ecological models. First, simple plant growth models are fit
   to plant growth data. Second, growth-resource models are fit.  *)

open Bristlecone
open Bristlecone.ModelSystem

// Test 1. Simple growth model (no noise)
// _____________________________

// A. Define a logistic growth model 
let vonBertalanffy' eta beta kappa mass =
    eta * mass ** beta - kappa * mass

// B. Define model system
let modelSystem =

    let vonBertalanffy p t x environment =
        vonBertalanffy' 
          (p |> Pool.getEstimate "eta") 
          (p |> Pool.getEstimate "beta") 
          (p |> Pool.getEstimate "kappa") x

    { Equations  = [ code "x", vonBertalanffy ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquares ["x"]
      Measures   = [] |> Map.ofList
      Parameters = [ code "eta",    parameter Unconstrained   0.001 1.00
                     code "beta",   parameter Unconstrained   0.001 1.00
                     code "kappa",  parameter Unconstrained   0.001 1.00 ] |> Map.ofList }

// C. Define model fitting method
let method =
      Bristlecone.mkContinuous
      |> Bristlecone.withCustomOptimisation 
            (Optimisation.MonteCarlo.adaptiveMetropolis 0.250 200)

// D. Define test methodology
let startValues = [code "x", 5.] |> Map.ofList
let generationRules = 
    [ code "x", fun (data:seq<float>) -> data |> Seq.pairwise |> Seq.sumBy (fun (a,b) -> b - a) < 50. ]
let endCondition = Optimisation.EndConditions.afterIteration 5000
let noNoise p x = x

// E. Test method against fake data
let testResult =
    modelSystem
    |> Bristlecone.testModel method 30 startValues endCondition generationRules noNoise
