#load "dendrofit.fsx"

///////////////////////////////////
/// Plant - Single Resource Limitation
///////////////////////////////////

(* An example model fit to ring width data with a single
   resource limitation. The models presented are based on
   Tilman (1990). *)

open DendroFit
open Types.ParameterEstimation
open Types
open Time

// 0. Configure Options
// ----------------------------

module Options =
    let resolution = Annual
    let iterations = 1000
    let testSeriesLength = 50
    let bootstrapTimes = 20


// 1. Create Hypotheses
// ----------------------------

/// An N-dependent net growth function
let f r k n =
    r * n / (r + k)

/// A resource supply function
let y n lambda =
    lambda

/// Uptake function
let u n =
    n

/// Rate of biomass change
let dxdt p t x (e:Environment) =
    (f (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k") (e.[ShortCode.create "N"])) - (p |> Pool.getEstimate "m") 
        + (p |> Pool.getEstimate "gammaX") * x

/// Rate of resource change
let dndt (p:CodedMap<Parameter>) _ n (environment:Environment) =
    (y n (p |> Pool.getEstimate "lambda")) - (p |> Pool.getEstimate "q") * (environment.[ShortCode.create "x"]) *
        (f (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k") n)  + (p |> Pool.getEstimate "gammaN") * n

let ``Plant Individual - Single Resource Limitation`` = 
    { Equations  = [ ShortCode.create "x",      dxdt
                     ShortCode.create "N",      dndt ] |> Map.ofList
      Parameters = [ ShortCode.create "r",      Parameter.create PositiveOnly 0.01 0.20     // RGRmax: maximum rate of resource-saturated growth per unit biomass
                     ShortCode.create "k",      Parameter.create PositiveOnly 5.00 5.00     // Half-saturation constant for nutrient-limited growth
                     ShortCode.create "m",      Parameter.create PositiveOnly 0.5 0.5       // Loss rate (all causes)
                     ShortCode.create "q",      Parameter.create PositiveOnly 0.1 1.0       // Nutrient content per unit biomass of species
                     ShortCode.create "lambda", Parameter.create PositiveOnly 0.01 1.00     // External resource supply rate
                     ShortCode.create "gammaX", Parameter.create PositiveOnly 0.01 0.01
                     ShortCode.create "gammaN", Parameter.create PositiveOnly 0.01 0.01
                    ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquares ["x"; "N"] }


// 2. Test Hypotheses Work
// ----------------------------

``Plant Individual - Single Resource Limitation``
|> DendroFit.test' Options.resolution Options.iterations Options.testSeriesLength


// 3. Load Real Data and Estimate
// ----------------------------

let plantIndividuals = 
    let yuribei = DataAccess.Shrub.loadRingWidths (__SOURCE_DIRECTORY__ + "/yuribei-rw.csv")
    let d15N = DataAccess.Shrub.loadLocalEnvironmentVariable (__SOURCE_DIRECTORY__ + "/yuribei-d15N.csv")
    yuribei
    |> Seq.map (fun s -> s.Identifier.Value, s)
    |> Seq.keyMatch d15N
    |> Seq.map (fun (_,plant,d15N) -> PlantIndividual.zipEnv (ShortCode.create "N") plant d15N)
    |> Seq.toList

let estimated =
    plantIndividuals.Head
    |> PlantIndividual.toRelativeGrowth
    |> PlantIndividual.keepCommonYears
    |> DendroFit.estimatePlant Options.resolution Options.iterations ``Plant Individual - Single Resource Limitation``


// 4. Generate Confidence Intervals
// ----------------------------
let confidenceIntervals = 
    plantIndividuals.Head
    |> PlantIndividual.keepCommonYears
    |> DendroFit.bootstrapPlant Options.resolution Options.iterations Options.bootstrapTimes ``Plant Individual - Single Resource Limitation`` 


// 5. Model Selection
// ----------------------------

let aicc =
    ModelSelection.Akaike.aicc estimated.Series.Count estimated.Parameters.Count estimated.Likelihood


// Appendix A. R Graphics
// ----------------------------

#load "plotting.fsx"
open RProvider
open RProvider.graphics

// i. Estimated versus Observed Series
R.par(namedParams [ "mfrow", [2;2]] ) |> ignore
R.plot (namedParams [ "x",box estimated.Series.[ShortCode.create "x"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "RGR (Observed)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.Series.[ShortCode.create "x"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "RGR (Model)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.Series.[ShortCode.create "N"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "Nitrogen (Observed)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.Series.[ShortCode.create "N"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "Nitrogen (Model)" ]) |> ignore
