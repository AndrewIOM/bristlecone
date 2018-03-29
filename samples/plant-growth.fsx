#load "dendrofit.fsx"

///////////////////////////////////
/// Growth Modelling with DendroFit
///////////////////////////////////

(* An example model fit to ring width data. 
   Growth models are defined in continuous time
   as relative growth rate (RGR) on the basis of
   current size. *)

open DendroFit
open Types.ParameterEstimation
open Types
open Time

// 0. Configure Options
// ----------------------------

module Options =
    let resolution = Annual
    let iterations = 1000
    let testSeriesLength = 20
    let bootstrapTimes = 20


// 1. Create Hypotheses
// ----------------------------

let hypotheses =

    let ``logistic (RGR - mass basis)`` =
        let dxdt p _ m _ = (p |> Pool.getEstimate "r") * (1. - (m / (p |> Pool.getEstimate "k")))
        { Equations  = [ ShortCode.create "x", dxdt] |> Map.ofList 
          Parameters = [ ShortCode.create "r", Parameter.create PositiveOnly 0.01 0.5
                         ShortCode.create "k", Parameter.create PositiveOnly 10. 11. ] |> Map.ofList
          Likelihood = ModelLibrary.Likelihood.sumOfSquares ["x"] }

    [ ``logistic (RGR - mass basis)`` ]


// 2. Test Hypotheses Work
// ----------------------------
let test = 
    hypotheses
    |> List.map (DendroFit.test' Options.resolution Options.iterations Options.testSeriesLength)


// 3. Load Real Data and Estimate
// ----------------------------
let ringWidths = DataAccess.Shrub.loadRingWidths (__SOURCE_DIRECTORY__ + "/yuribei-rw.csv") 
let rgr = ringWidths |> List.map PlantIndividual.toRelativeGrowth
let estimate = rgr |> List.map (fun x -> DendroFit.estimate Options.resolution Options.iterations hypotheses.[0] (PlantIndividual.growthSeries x.Growth))


// 4. Generate Confidence Intervals
// ----------------------------
let confidenceIntervals = 
    ringWidths
    |> List.map (DendroFit.bootstrapPlant Options.resolution Options.iterations Options.bootstrapTimes hypotheses.[0])


// 5. Model Selection
// ----------------------------
let aicc =
    estimate
    |> List.map (fun r -> ModelSelection.Akaike.aicc r.Series.Count r.Parameters.Count r.Likelihood)



// Appendix A. R Graphics
// ----------------------------
#load "plotting.fsx"
open RProvider
open RProvider.graphics

let plot =
    R.par(namedParams [ "mfrow", [2;1]] ) |> ignore
    R.plot (estimate.[0].Series.[ShortCode.create "x"].Observed) |> ignore
    R.plot (estimate.[0].Series.[ShortCode.create "x"].Expected) |> ignore