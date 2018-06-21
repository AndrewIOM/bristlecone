#load "dendrofit.fsx"

///////////////////////////////////
/// Tilman (1988 + 1990)
///////////////////////////////////

(* An example model fit to plant biomass data with a single
   resource limitation. The models presented are based on
   Tilman (1990 / 1988). *)

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

/// Allometric relationship
module Allometric =

    let nthroot n A =
        let rec f x =
            let m = n - 1.
            let x' = (m * x + A/x**m) / n
            match abs(x' - x) with
            | t when t < abs(x * 1e-9) -> x'
            | _ -> f x'
        f (A / double n)

    let forwards alpha beta y1 = beta * y1 ** alpha

    let backward alpha beta y2 = nthroot alpha (y2 / beta)


/// The relative growth rate of biomass is dependent on a single limiting resource
module ResourceRatioTheory =
    let ``nutrient-dependent growth (whole-biomass)`` =

        // Support functions
        let resourceSupply lambda = lambda
        let netGrowthFunction n r k = r * n / ( n + k )
        let toBiomass alpha beta y1 = beta * y1 ** alpha (* Allometric conversion of ring width to total biomass *)

        // Variables - RGR
        let dxdt' n r k = netGrowthFunction n r k
        let dndt' n b lambda q r k = (resourceSupply lambda) - q * b * (netGrowthFunction n r k)

        let dxdt p _ _ (e:Environment) = dxdt' (e.[ShortCode.create "N"]) (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k")
        let dndt p _ n (e:Environment) = dndt' n (toBiomass (p |> Pool.getEstimate "alpha") (p |> Pool.getEstimate "beta") (e.[ShortCode.create "x"])) (p |> Pool.getEstimate "lambda") (p |> Pool.getEstimate "q") (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k")

        { Equations  = [ ShortCode.create "x",      dxdt
                         ShortCode.create "N",      dndt ] |> Map.ofList
          Parameters = [ ShortCode.create "lambda", Parameter.create PositiveOnly 0.05 1.0     // External resource supply rate
                         ShortCode.create "k",      Parameter.create PositiveOnly 0.01 1.00     // Half-saturation constant for nutrient-limited growth
                         ShortCode.create "q",      Parameter.create PositiveOnly 0.10 0.15     // Nutrient content per unit biomass of species
                         ShortCode.create "r",      Parameter.create PositiveOnly 0.01 0.05     // RGRmax: maximum rate of resource-saturated growth per unit biomass
                         ShortCode.create "alpha",  Parameter.create PositiveOnly 1.0 1.0       // Allometric term
                         ShortCode.create "beta",   Parameter.create PositiveOnly 1.0 1.0       // Allometric term
                        ] |> Map.ofList
          Likelihood = ModelLibrary.Likelihood.sumOfSquares ["x"; "N"] }

    /// Resources are partitioned into stem, leaf, and root biomass.
    /// Photosynthesis is a leaf function, and nutrient aquisition is a root function.
    let ``nutrient-dependent growth (stem biomass)`` =

        /// Resource-dependent growth function
        let f bl n r k = (r * bl * n) / (n + k)

        /// Nitrogen replenishment rate.
        let y n lambda = lambda

        /// Relative growth rate (RGR) of stem biomass
        let dBsBsdt' bs n r k al ar respR = 
            let bl = bs * al
            let br = bs * ar
            ((f bl n r k) - (respR * (bs + bl + br))) * (1. / (1. + al + ar ))

        /// Change in soil nitrogen concentration
        let dndt' n bs lambda q r k al ar respR =
            let bl = bs * al
            let br = bs * ar
            (y n lambda) - (q * (bs + bl + br) * ((f bl n r k) - (respR * (bs + bl + br))))

        /// DendroFit function for dBs/Bsdt
        let dBsBsdt p _ bs (e:Environment) =
            dBsBsdt' bs (e.[ShortCode.create "N"]) (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k")
                (p |> Pool.getEstimate "al") (p |> Pool.getEstimate "ar") (p |> Pool.getEstimate "respr")

        /// DendroFit function for dN/dt
        let dndt p _ n (e:Environment) =
            dndt' n (e.[ShortCode.create "x"]) (p |> Pool.getEstimate "lambda") (p |> Pool.getEstimate "q") (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k")
                (p |> Pool.getEstimate "al") (p |> Pool.getEstimate "ar") (p |> Pool.getEstimate "respr")

        { Equations  = [ ShortCode.create "x",      dBsBsdt
                         ShortCode.create "N",      dndt ] |> Map.ofList
          Parameters = [ ShortCode.create "r",      Parameter.create PositiveOnly 0.10 0.15     // RGRmax: maximum rate of resource-saturated growth per unit biomass
                         ShortCode.create "k",      Parameter.create PositiveOnly 0.10 0.50     // Half-saturation constant for nutrient-limited growth
                         ShortCode.create "q",      Parameter.create PositiveOnly 1.00 1.00     // Nutrient content per unit biomass of species
                         ShortCode.create "lambda", Parameter.create PositiveOnly 1.00 1.00     // External resource supply rate
                         ShortCode.create "respr",  Parameter.create PositiveOnly 1.00 1.00     // Respiration rate of all biomass
                         ShortCode.create "ar",     Parameter.create PositiveOnly 0.9 0.9       // Ratio of roots:stem allocation
                         ShortCode.create "al",     Parameter.create PositiveOnly 0.9 0.9       // Ratio of leaves:stem allocation
                        ] |> Map.ofList
          Likelihood = ModelLibrary.Likelihood.sumOfSquares ["x"; "N"] }

    let ``nutrient-dependent growth (stem diameter)`` =

        /// Resource-dependent growth function
        let f bl n r k = (r * bl * n) / (n + k)

        /// Nitrogen replenishment rate.
        let y n lambda = lambda

        /// Relative growth rate (RGR) of stem diameter
        /// All of the B terms are derived using an allometric relationship
        /// The final dDdt is the result run through the reverse allometric relationship
        let dDdt' d n r k al ar respR alpha beta = 
            let bs = Allometric.forwards alpha beta d
            let bl = bs * al
            let br = bs * ar
            ((f bl n r k) - (respR * (bs + bl + br))) * (1. / (1. + al + ar ))

        /// Change in soil nitrogen concentration
        /// All of the B terms are derived using an allometric relationship
        let dndt' n d lambda q r k al ar respR alpha beta =
            let bs = Allometric.forwards alpha beta d
            let bl = bs * al
            let br = bs * ar
            ((y n lambda) - (q * (bs + bl + br) * ((f bl n r k) - (respR * (bs + bl + br)))))
            |> Allometric.backward alpha beta

        /// DendroFit function for dBs/Bsdt
        let dDdt p _ bs (e:Environment) =
            dDdt' bs (e.[ShortCode.create "N"]) (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k")
                (p |> Pool.getEstimate "al") (p |> Pool.getEstimate "ar") (p |> Pool.getEstimate "respr") (p |> Pool.getEstimate "alpha") (p |> Pool.getEstimate "beta")

        /// DendroFit function for dN/dt
        let dndt p _ n (e:Environment) =
            dndt' n (e.[ShortCode.create "x"]) (p |> Pool.getEstimate "lambda") (p |> Pool.getEstimate "q") (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k")
                (p |> Pool.getEstimate "al") (p |> Pool.getEstimate "ar") (p |> Pool.getEstimate "respr") (p |> Pool.getEstimate "alpha") (p |> Pool.getEstimate "beta")

        { Equations  = [ ShortCode.create "x",      dDdt
                         ShortCode.create "N",      dndt ] |> Map.ofList
          Parameters = [ ShortCode.create "r",      Parameter.create PositiveOnly 0.10 0.50     // RGRmax: maximum rate of resource-saturated growth per unit biomass
                         ShortCode.create "k",      Parameter.create PositiveOnly 0.50 0.50     // Half-saturation constant for nutrient-limited growth
                         ShortCode.create "q",      Parameter.create PositiveOnly 1.00 1.00     // Nutrient content per unit biomass of species
                         ShortCode.create "lambda", Parameter.create PositiveOnly 0.10 0.10     // External resource supply rate
                         ShortCode.create "respr",  Parameter.create PositiveOnly 1.00 1.00     // Respiration rate of all biomass
                         ShortCode.create "ar",     Parameter.create PositiveOnly 0.05 0.30       // Ratio of roots:stem allocation
                         ShortCode.create "al",     Parameter.create PositiveOnly 0.05 0.30       // Ratio of leaves:stem allocation
                         ShortCode.create "alpha",  Parameter.create PositiveOnly 1.0 1.0       // Allometric #1
                         ShortCode.create "beta",   Parameter.create PositiveOnly 0.75 0.75     // Allometric #2
                        ] |> Map.ofList
          Likelihood = ModelLibrary.Likelihood.sumOfSquares ["x"; "N"] }


    /// Resources are partitioned into stem, leaf, and root biomass.
    /// Photosynthesis is a leaf function, and nutrient aquisition is a root function.
    let ``nutrient-dependent growth (resource partitioning)`` =

        /// Assumption: Nutrient availability to the plant is dependent on the ratio of current root mass to leaf mass.
        let effectiveNutrient n rootMass leafMass : float = 
            n * rootMass / leafMass

        /// Assumption: Plants must meet respiration costs before they can produce new growth.
        /// RR, RS, RL = Respiration rate per unit biomass for roots, stem, and leaves.
        /// BR, BS, BL = Current biomass of root, stem, and leaf.
        let respirationDemand respr br bs bl : float =
            respr * br + respr * bs + respr * bl

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
        let dbdt' b s r k respr al aStem ar =
            let biomassLeaf = b * al
            let biomassStem = b * aStem
            let biomassRoot = b * ar
            let photosynthate = photosynthate r k s biomassRoot biomassLeaf
            let respiration = respirationDemand respr biomassRoot biomassStem biomassLeaf
            (*b **) 
            (photosynthate - respiration)

        /// dS/dt = continuous change in soil nutrient concentration
        let dsdt' s b q replenishmentRate r k respr al aStem ar =
            let biomassLeaf = b * al
            let biomassStem = b * aStem
            let biomassRoot = b * ar
            let photosynthate = photosynthate r k s biomassRoot biomassLeaf
            let respiration = respirationDemand respr biomassRoot biomassStem biomassLeaf
            replenishmentRate - q * b * (photosynthate - respiration)

        let dxdt p _ x (e:Environment) =
            dbdt' x (e.[ShortCode.create "N"]) (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "k") (p |> Pool.getEstimate "respr") 
                (p |> Pool.getEstimate "al") (p |> Pool.getEstimate "as") (p |> Pool.getEstimate "ar")

        let dndt p _ n (e:Environment) =
            dsdt' n (e.[ShortCode.create "x"]) (p |> Pool.getEstimate "q") (p |> Pool.getEstimate "lambda") (p |> Pool.getEstimate "r") 
                (p |> Pool.getEstimate "k") (p |> Pool.getEstimate "respr") (p |> Pool.getEstimate "al") (p |> Pool.getEstimate "as") (p |> Pool.getEstimate "ar")

        { Equations  = [ ShortCode.create "x",      dxdt
                         ShortCode.create "N",      dndt ] |> Map.ofList
          Parameters = [ ShortCode.create "r",      Parameter.create PositiveOnly 0.10 0.15     // RGRmax: maximum rate of resource-saturated growth per unit biomass
                         ShortCode.create "k",      Parameter.create PositiveOnly 0.10 0.50     // Half-saturation constant for nutrient-limited growth
                         ShortCode.create "q",      Parameter.create PositiveOnly 1.00 1.00     // Nutrient content per unit biomass of species
                         ShortCode.create "lambda", Parameter.create PositiveOnly 0.50 0.50     // External resource supply rate
                         ShortCode.create "respr",  Parameter.create PositiveOnly 0.50 0.50     // Respiration rate of all biomass
                         ShortCode.create "ar",     Parameter.create PositiveOnly 0.1 0.9       // Ratio of roots:stem allocation
                         ShortCode.create "al",     Parameter.create PositiveOnly 0.1 0.9       // Ratio of leaves:stem allocation
                        ] |> Map.ofList
          Likelihood = ModelLibrary.Likelihood.sumOfSquares ["x"; "N"] }


// 2. Test Hypotheses Work
// ----------------------------

let startValues = [ ShortCode.create "x", 0.038647343; ShortCode.create "N", 3.64] |> Map.ofList

ResourceRatioTheory.``nutrient-dependent growth (stem biomass)``
|> DendroFit.test' Options.resolution Options.iterations Options.testSeriesLength startValues

ResourceRatioTheory.``nutrient-dependent growth (stem diameter)``
|> DendroFit.test' Options.resolution Options.iterations Options.testSeriesLength startValues


// 3. Load Real Data and Estimate
// ----------------------------

let shrubs = 
    let yuribei = DataAccess.Shrub.loadRingWidths (__SOURCE_DIRECTORY__ + "/yuribei-rw.csv")
    let d15N = DataAccess.Shrub.loadLocalEnvironmentVariable (__SOURCE_DIRECTORY__ + "/yuribei-filled.csv")
    yuribei
    |> Seq.map (fun s -> s.Identifier.Value, s)
    |> Seq.keyMatch d15N
    |> Seq.map (fun (_,plant,d15N) -> PlantIndividual.zipEnv (ShortCode.create "N") plant d15N)
    |> Seq.toList

let estimated =
    shrubs
    |> List.map (PlantIndividual.toRelativeGrowth 
                 >> PlantIndividual.keepCommonYears 
                 >> (DendroFit.estimatePlant Options.resolution Options.iterations ResourceRatioTheory.``nutrient-dependent growth (stem diameter)`` ))


// 4. Generate Confidence Intervals
// ----------------------------
let confidenceIntervals = 
    shrubs.Head
    |> PlantIndividual.keepCommonYears
    |> DendroFit.bootstrapPlant Options.resolution Options.iterations Options.bootstrapTimes ResourceRatioTheory.``nutrient-dependent growth (stem diameter)``



// Appendix A. R Graphics
// ----------------------------

#load "plotting.fsx"
open RProvider
open RProvider.graphics

// i. Estimated versus Observed Series
R.par(namedParams [ "mfrow", [2;2]] ) |> ignore
R.plot (namedParams [ "x",box estimated.[0].Series.[ShortCode.create "x"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "RGR (Observed)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.[0].Series.[ShortCode.create "x"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "RGR (Model)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.[0].Series.[ShortCode.create "N"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "Nitrogen (Observed)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.[0].Series.[ShortCode.create "N"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "Nitrogen (Model)" ]) |> ignore


// ii. Plot shrubs and N in relative increment form
let rPlots =
    shrubs
    |> List.map PlantIndividual.keepCommonYears
    |> List.map (fun s -> 
        let g = s.Growth |> PlantIndividual.growthSeries |> GrowthSeries.growthToTime
        let n = s.Environment.[ShortCode.create "N"].Values
        g.Values, n )

let x,n = rPlots.[2]

R.plot (namedParams [ "x",box x; "type", box "l"; "xlab", box "Year"; "ylab", box "Relative Increment" ]) |> ignore
R.plot (namedParams [ "x",box n; "type", box "l"; "xlab", box "Year"; "ylab", box "d15N" ]) |> ignore
