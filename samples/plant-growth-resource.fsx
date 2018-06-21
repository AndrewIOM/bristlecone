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
    let iterations = 100
    let testSeriesLength = 50
    let bootstrapTimes = 20


// 1. Create Hypotheses
// ----------------------------

/// An N-dependent net growth function
let f r k n =
    r * n / (n + k)

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

/// Allometric Relation
module Allometrics =
    let y2 alpha beta y1 = beta * y1 ** alpha

    let nthroot n A =
        let rec f x =
            let m = n - 1.
            let x' = (m * x + A/x**m) / n
            match abs(x' - x) with
            | t when t < abs(x * 1e-9) -> x'
            | _ -> f x'
        f (A / double n)

    let y1 alpha beta y2 = nthroot alpha (y2 / beta)

let ``Plant Individual - Single Resource Limitation`` = 
    { Equations  = [ ShortCode.create "x",      dxdt
                     ShortCode.create "N",      dndt ] |> Map.ofList
      Parameters = [ ShortCode.create "r",      Parameter.create PositiveOnly 0.10 0.10     // RGRmax: maximum rate of resource-saturated growth per unit biomass
                     ShortCode.create "k",      Parameter.create PositiveOnly 0.50 1.00    // Half-saturation constant for nutrient-limited growth
                     ShortCode.create "m",      Parameter.create PositiveOnly 0.01 0.01   // Loss rate (all causes)
                     ShortCode.create "q",      Parameter.create PositiveOnly 0.50 0.50     // Nutrient content per unit biomass of species
                     ShortCode.create "lambda", Parameter.create PositiveOnly 0.10 0.10     // External resource supply rate
                     ShortCode.create "gammaX", Parameter.create PositiveOnly 0.01 0.01
                     ShortCode.create "gammaN", Parameter.create PositiveOnly 0.01 0.01
                    ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquares ["x"; "N"] }

let ``Basic Tilman with allometric scaling`` =

    let dxdtAllometric p t x (e:Environment) =
        let biomass = Allometrics.y2 (p |> Pool.getEstimate "alpha") (p |> Pool.getEstimate "beta") x
        let tilman = (f (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k") (e.[ShortCode.create "N"])) + (p |> Pool.getEstimate "gammaX") * biomass
        Allometrics.y1 (p |> Pool.getEstimate "alpha") (p |> Pool.getEstimate "beta") tilman

    let dndtAllometric (p:CodedMap<Parameter>) _ n (environment:Environment) =
        (y n (p |> Pool.getEstimate "lambda")) - (p |> Pool.getEstimate "q") * (Allometrics.y2 (p |> Pool.getEstimate "alpha") (p |> Pool.getEstimate "beta") (environment.[ShortCode.create "x"])) *
            (f (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k") n)  + (p |> Pool.getEstimate "gammaN") * n

    { Equations  = [ ShortCode.create "x",      dxdtAllometric
                     ShortCode.create "N",      dndtAllometric ] |> Map.ofList
      Parameters = [ ShortCode.create "r",      Parameter.create PositiveOnly 0.10 0.10     // RGRmax: maximum rate of resource-saturated growth per unit biomass
                     ShortCode.create "k",      Parameter.create PositiveOnly 0.10 10.0     // Half-saturation constant for nutrient-limited growth
                     //ShortCode.create "m",      Parameter.create PositiveOnly 0.01 0.01   // Loss rate (all causes)
                     ShortCode.create "q",      Parameter.create PositiveOnly 1.00 1.00     // Nutrient content per unit biomass of species
                     ShortCode.create "lambda", Parameter.create PositiveOnly 10.00 10.00     // External resource supply rate
                     ShortCode.create "gammaX", Parameter.create PositiveOnly 0.01 0.01
                     ShortCode.create "gammaN", Parameter.create PositiveOnly 0.01 0.01
                     ShortCode.create "alpha",  Parameter.create Unconstrained 1.0 1.0
                     ShortCode.create "beta",   Parameter.create Unconstrained 0.70 0.70
                    ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquares ["x"; "N"] }

// 2. Test Hypotheses Work
// ----------------------------

``Plant Individual - Single Resource Limitation``
|> DendroFit.test' Options.resolution Options.iterations Options.testSeriesLength

``Basic Tilman with allometric scaling``
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

let estimated =
    plantIndividuals.Head
    |> PlantIndividual.toRelativeGrowth
    |> PlantIndividual.keepCommonYears
    |> DendroFit.estimatePlant Options.resolution Options.iterations ``Basic Tilman with allometric scaling``


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






let ``ALLOCATE (Tilman 1988)`` =

    /// Assumption: Nutrient availability to the plant is dependent on the ratio of current root mass to leaf mass.
    let effectiveNutrient n rootMass leafMass : float = 
        n * rootMass / leafMass

    /// Assumption: Plants must meet respiration costs before they can produce new growth.
    /// RR, RS, RL = Respiration rate per unit biomass for roots, stem, and leaves.
    /// BR, BS, BL = Current biomass of root, stem, and leaf.
    let respirationDemand rr rs rl br bs bl : float =
        rr * br + rs * bs + rl * bl

    /// Assumption: Photosynthate is produced by leaf processes, and is limited by a single nutrient.
    /// C = effective availability of soil resource
    /// S = resource concentration in soil
    /// BR, BL, BS = current root, leaf, and stem biomass
    /// R = maximum rate of photosynthesis per unit leaf biomass
    /// K = half-saturation constant for nutrient-limited photosynthesis
    /// RR, RL, RS = Respiration rate per unit biomass for root, leaf, and stem
    let photosynthate r k s br bl = 
        let c = effectiveNutrient s br bl
        r * bl * c / c + k

    /// dB/dt = continuous change in plant total biomass
    let dbdt' b s r k rr rs rl al aStem ar =
        let biomassLeaf = b * al
        let biomassStem = b * aStem
        let biomassRoot = b * ar
        let photosynthate = photosynthate r k s biomassRoot biomassLeaf
        let respiration = respirationDemand rr rs rl biomassRoot biomassStem biomassLeaf
        b * (photosynthate - respiration)

    /// dS/dt = continuous change in soil nutrient concentration
    let dsdt' s b q replenishmentRate r k rr rs rl al aStem ar =
        let biomassLeaf = b * al
        let biomassStem = b * aStem
        let biomassRoot = b * ar
        let photosynthate = photosynthate r k s biomassRoot biomassLeaf
        let respiration = respirationDemand rr rs rl biomassRoot biomassStem biomassLeaf
        replenishmentRate - q * b * (photosynthate - respiration)

    let dxdt p _ x (e:Environment) =
        dbdt' x (e.[ShortCode.create "n"]) (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k") (p |> Pool.getEstimate "rr") 
            (p |> Pool.getEstimate "rs") (p |> Pool.getEstimate "rl") (p |> Pool.getEstimate "al") (p |> Pool.getEstimate "as") (p |> Pool.getEstimate "ar")

    let dndt p _ n (e:Environment) =
        dsdt' n (e.[ShortCode.create "x"]) (p |> Pool.getEstimate "q") (p |> Pool.getEstimate "lambda") (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k") (p |> Pool.getEstimate "rr") 
            (p |> Pool.getEstimate "rs") (p |> Pool.getEstimate "rl") (p |> Pool.getEstimate "al") (p |> Pool.getEstimate "as") (p |> Pool.getEstimate "ar")

    { Equations  = [ ShortCode.create "x",      dxdt
                     ShortCode.create "N",      dndt ] |> Map.ofList
      Parameters = [ ShortCode.create "r",      Parameter.create PositiveOnly 0.10 0.10     // RGRmax: maximum rate of resource-saturated growth per unit biomass
                     ShortCode.create "k",      Parameter.create PositiveOnly 0.10 10.0     // Half-saturation constant for nutrient-limited growth
                     ShortCode.create "q",      Parameter.create PositiveOnly 1.00 1.00     // Nutrient content per unit biomass of species
                     ShortCode.create "lambda", Parameter.create PositiveOnly 1.00 1.00     // External resource supply rate
                     ShortCode.create "rr",     Parameter.create PositiveOnly 0.01 0.01
                     ShortCode.create "rl",     Parameter.create PositiveOnly 0.01 0.01
                     ShortCode.create "rs",     Parameter.create Unconstrained 0.5 1.20
                     ShortCode.create "ar",     Parameter.create Unconstrained 0.1 0.1
                     ShortCode.create "al",     Parameter.create Unconstrained 0.1 0.1
                     ShortCode.create "as",     Parameter.create Unconstrained 0.8 0.8
                    ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquares ["x"; "N"] }
