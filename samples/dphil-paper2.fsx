#load "dendrofit.fsx"

////////////////////////////////////////////////////
/// Yamal Salix lanata Shrub - Nitrogen Interactions
////////////////////////////////////////////////////

(* Shrub ring width modelled with a single
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
    let iterations = 10000
    let testSeriesLength = 50


module Constants =

    // Empirically-derived parameters:
    let k5 = 80.200334 // Allometric fit to Yamal shrub BD-length data #1
    let k6 = 9.918091 // Allometric fit to Yamal shrub BD-length data #2

    // Constants from the literature:
    let a = 2. // the number of child branches added to previous branches (including the tops of the stems) for a shrub
    let p = 0.5 // the length of a child branch as a proportion of its parent branch/stem
    let lmin = 20. //cm. the length at which a stem or branch gets child branches
    let rtip = 0.1 //cm. the radius of the outermost tip of a stem or branch
    let b = 0.0075 // the ratio of the basal radius of a stem or branch and its length
    let salixWoodDensity = 0.5 // g / cm3 (from internet)
    let numberOfStems = 2.2


// 1. Setup model components
// ----------------------------

module ModelComponents =

    let pi = System.Math.PI

    module Götmark2016_ShrubModel =

        /// Gives the basal radius in centimeters of a stem/branch given its length in centimeters. Function from Niklas and Spatz (2004). 
        let basalRadius k5 k6 stemLength =
            100. * (( 0.01 * stemLength + k6) / k5) ** (3. / 2.) / 2.

        /// Total shrub volume given height and number of stems
        let shrubVolume b a rtip p lmin k5 k6 n h =

            let radius = basalRadius k5 k6
            let mainStemVolume =
                match radius h with
                | r when r > rtip -> n * pi * h * ((radius h) ** 2. + (radius h) * rtip + rtip ** 2.) / 3.
                | _ -> n * pi * h * rtip ** 2.

            let mutable volume = mainStemVolume
            let mutable k = 0.

            while (p ** k * h > lmin * 2./3.) do
                let volToAdd =
                    match (p ** k * h < lmin) with
                    | true ->
                        match (b * 3. * p * (p ** k * h - 2. * lmin / 3.) > rtip) with
                        | true ->
                            n * a * (a + 1.) ** (float k) * pi * 3. * p * (p ** k * h - 2. * lmin / 3.) * ((radius (3. * p * (p ** k * h - 2. * lmin / 3.))) * (radius (3. * p * (p ** k * h - 2. * lmin / 3.)) * rtip + rtip ** 2.)) / 3.
                        | false ->
                            n * a * (a + 1.) ** (float k) * 3. * p * (p ** k * h - 2. * lmin / 3.) * pi * rtip ** 2.
                    | false ->
                        match (radius (p ** (k + 1.) * h) > rtip) with
                        | true ->
                            n * a * (a + 1.) ** (float k) * pi * p ** (k + 1.) * h * ((radius (p ** (k+1.) * h)) ** 2. + (radius (p ** (k + 1.) * h)) * rtip + rtip ** 2.) / 3.
                        | false ->
                            n * a * (a + 1.) ** (float k) * p ** (k + 1.) * h * pi * rtip ** 2.
                
                volume <- volume + volToAdd
                k <- k + 1.

            k, volume


    module Allometrics =

        open Götmark2016_ShrubModel

        let stemLength k5 k6 radius =
            k5 * (2. * radius) ** (2. / 3.) - k6

        let mass woodDensity volume =
            volume * woodDensity

        let massToVolume woodDensity mass =
            mass / woodDensity

        let shrubBiomass b a rtip p lmin k5 k6 n woodDensity radius =
            radius
            |> stemLength k5 k6
            |> shrubVolume b a rtip p lmin k5 k6 n |> snd
            |> mass woodDensity

        let shrubRadius b a rtip p lmin k5 k6 n woodDensity mass =
            let findRadius volume =
                let v x = x |> stemLength k5 k6 |> shrubVolume b a rtip p lmin k5 k6 n |> snd
                let f = (fun x -> 
                    //printfn "~ %f / %f" (v x) x
                    (v x) - volume )
                //printfn "Finding solution to inverse allometric - volume is %f" volume
                MathNet.Numerics.RootFinding.Bisection.FindRoot(System.Func<float,float> f, 0.01, 1000000., 1e-8, 1000)
                //ODE.RootFinding.bisect 0 200 f 0.01 1000.00 1e-8 // Assumption that shrub radius is between 0.01 and 20.0cm.
            mass
            |> massToVolume woodDensity
            |> findRadius

    module GrowthRate =

        /// **Description**
        /// A monotonically increasing function of a resource `r`.
        /// **Parameters**
        ///   * `fMax` - maximum specific growth rate
        ///   * `h` - soil resource concentration required for growth at half the maximum rate
        ///   * `r` - the current resource concentration
        let michaelisMenten fMax h (r:float) =
            fMax * (r / (h + r))

    module AbioticResource =

        /// **Description**
        /// A standard chemostat-type model for replenshment of an abiotic resource.
        /// **Parameters**
        ///   * `d` - a rate constant
        ///   * `s` - resource concentration of the inflow
        ///   * `n` - the current resource concentration
        let chemostat d s (n:float) =
            d * (s - n)


// 2. Create Hypotheses
// ----------------------------

let ``nutrient-dependent growth`` =

    /// Radius in millimetres
    let toBiomass radius = 
        radius / 10. |> ModelComponents.Allometrics.shrubBiomass Constants.b Constants.a Constants.rtip Constants.p Constants.lmin Constants.k5 Constants.k6 Constants.numberOfStems Constants.salixWoodDensity

    /// Biomass in grams
    let toRadius biomass = 
        let radiusCm = biomass |> ModelComponents.Allometrics.shrubRadius Constants.b Constants.a Constants.rtip Constants.p Constants.lmin Constants.k5 Constants.k6 Constants.numberOfStems Constants.salixWoodDensity
        radiusCm * 10.

    // /// Conversion of d15N proxy into bioavailable N - explicit fractionation
    // let isotopeToBioavailableN fractionationEffect proxyError d15N : float =
    //     d15N + fractionationEffect + proxyError

    /// Uptake rate (modelled as 'effective nutrient availability')
    let effectiveNutrient n rootMass leafMass : float = 
        n * rootMass / leafMass

    /// Resource-dependent growth function
    /// AKA Specific growth rate
    /// Maximum rate of photosynthesis per unit of photosynthetic tissue under optimal conditions
    let f = ModelComponents.GrowthRate.michaelisMenten

    /// Nitrogen replenishment rate.
    let y = ModelComponents.AbioticResource.chemostat
    
    /// Cumulative stem biomass [bs].
    let dbsdt' bs n fMax h al ar r m = 
        let bl = bs * al
        let br = bs * ar
        let totalGrowth = bl * (f fMax h (effectiveNutrient n br bl)) - r * (bs + bl + br) - m * (bs + bl + br)
        // printfn "Start biomass is (Stem %f) (Leaf %f) (Root %f). Growth during this time is %fg" bs bl br totalGrowth
        totalGrowth * (1. / (1. + al + ar ))

    /// Bioavailable soil nitrogen [N]
    let dndt' (n:float) (bs:float) d s q fMax h al ar respR : float =
        let bl = bs * al
        let br = bs * ar
        (y d s n) - q * ((f fMax h (effectiveNutrient n br bl)) - respR * (bs + bl + br))

    /// Measurement variable: stem radius [rw].
    /// If there has been biomass accumulation during the time interval, wood accumulation occurs, according to shrub allometric relationships. 
    let drwdt' bs n r k al ar respR eb = 
        let biomassStemChange = dbsdt' bs n r k al ar respR eb
        // printfn "Bs %f; N %f; Bs/dt %f" bs n biomassStemChange
        if biomassStemChange > 0.
            then 
                let oldRadius = bs |> toRadius
                let newRadius = (bs + biomassStemChange) |> toRadius
                // printfn "Biomass stem change is positive: %f, %f, %f" biomassStemChange newRadius oldRadius
                newRadius - oldRadius
            else 0.

    /// DendroFit function for dBs/dt
    let dbsdt p _ bs (e:Environment) =
        dbsdt' bs (e.[ShortCode.create "N"]) (p |> Pool.getEstimate "fMax") (p |> Pool.getEstimate "h")
            (p |> Pool.getEstimate "al") (p |> Pool.getEstimate "ar") (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "m")

    /// DendroFit function for dN/dt
    let dndt p _ n (e:Environment) =
        dndt' n (e.[ShortCode.create "x"]) (p |> Pool.getEstimate "d") (p |> Pool.getEstimate "s") (p |> Pool.getEstimate "q") (p |> Pool.getEstimate "fMax")
            (p |> Pool.getEstimate "h") (p |> Pool.getEstimate "al") (p |> Pool.getEstimate "ar") (p |> Pool.getEstimate "r")

    /// DendroFit function for dr/dt
    let drwdt p _ _ (e:Environment) =
        drwdt' (e.[ShortCode.create "bs"]) (e.[ShortCode.create "N"]) (p |> Pool.getEstimate "fMax") (p |> Pool.getEstimate "h")
            (p |> Pool.getEstimate "al") (p |> Pool.getEstimate "ar") (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "m")

    { Equations  = [ ShortCode.create "x",      drwdt
                     ShortCode.create "bs",     dbsdt
                     ShortCode.create "N",      dndt ] |> Map.ofList
      Parameters = [ // for growth function
                     ShortCode.create "fMax",   Parameter.create PositiveOnly   0.10 0.20   // Maximum rate of resource-saturated growth per unit biomass
                     ShortCode.create "h",      Parameter.create PositiveOnly   0.50 1.00   // Soil resource concentration for growth at half of maximum rate
                     // for nitrogen replenishment
                     ShortCode.create "d",      Parameter.create PositiveOnly   0.90 1.10   // Rate of nitrogen replenishment
                     ShortCode.create "s",      Parameter.create PositiveOnly   0.90 1.10   // Nitrogen concentration of the environmental inflow
                     // for shrub allocation and physiology
                     ShortCode.create "al",     Parameter.create PositiveOnly   0.45 0.55   // Allocation fraction to leaves, relative to stem
                     ShortCode.create "ar",     Parameter.create PositiveOnly   1.00 2.00   // Allocation fraction to roots, relative to stem
                     ShortCode.create "r",      Parameter.create PositiveOnly   0.01 0.20   // Respiration rate per unit biomass
                     ShortCode.create "m",      Parameter.create PositiveOnly   0.01 0.10   // Biomass loss rate (e.g. from herbivory)
                     ShortCode.create "q",      Parameter.create PositiveOnly   0.25 0.35   // Nutrient concentration per unit biomass
                     // for likelihood function
                     ShortCode.create "sigmax", Parameter.create Unconstrained -0.25 0.25   // Standard deviation of x (biomass)
                     ShortCode.create "sigmay", Parameter.create Unconstrained -0.25 0.25   // Standard deviation of y (nitrogen)
                     ShortCode.create "rho",    Parameter.create Unconstrained -0.15 0.15   // Covariance between growth and nitrogen
                    ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquaresNegativeLog [ "x"; "N" ] }// ModelLibrary.Likelihood.bivariateGaussian "x" "N" }


// 2. Test Hypotheses Work
// ----------------------------

let startValues = [ ShortCode.create "x", 0.23; ShortCode.create "N", 4.64; ShortCode.create "bs", 471.5475542] |> Map.ofList

``nutrient-dependent growth``
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

// Start value configuration
let getStartValues startDate (plant:Types.PlantIndividual.PlantIndividual) =
    let initialRadius =
        match plant.Growth with
        | Types.PlantIndividual.PlantGrowth.RingWidth s -> 
            match s with
            | GrowthSeries.Absolute c -> c.Head |> fst |> removeUnit
            | GrowthSeries.Cumulative c -> 6.21 //c.Head |> fst |> removeUnit
            | _ -> invalidOp "Not implemented"
        | _ -> invalidOp "Not implemented 2"
    let initialMass = initialRadius |> removeUnit //|> toBiomass
    let initialNitrogen = plant.Environment.[ShortCode.create "N"].Head |> fst
    [ ShortCode.create "x", initialRadius
      ShortCode.create "N", initialNitrogen 
      ShortCode.create "bs", initialMass ] |> Map.ofList

// Match environment and growth lengths
// If n-1 time point has a value in the original set, append to new set

let estimated =
    [shrubs.[0]] |> List.map (fun s ->
        let shrub = s |> PlantIndividual.toCumulativeGrowth
        let common = shrub |> PlantIndividual.keepCommonYears
        let startDate = common.Environment.[ShortCode.create "N"] |> TimeSeries.start
        let startConditions = getStartValues startDate shrub
        common
        |> DendroFit.estimatePlant Options.resolution 10000 ``nutrient-dependent growth`` (Custom startConditions))

// 4. Plot Results (using R)
// ----------------------------

#load "plotting.fsx"
open RProvider
open RProvider.graphics
open RProvider.ggplot2

// i. Plot likelihood by iteration (for MCMC)
R.plot( namedParams [ "x", box (estimated.[0].Trace |> fst |> List.rev |> List.skip 100) ; "type", box "l"; "xlab", box "Iteration"; "ylab", box "-log likelihood" ]) |> ignore

// i. Estimated versus Observed Series
R.par(namedParams [ "mfrow", [4;4]] ) |> ignore
R.plot (namedParams [ "x",box estimated.[0].Series.[ShortCode.create "x"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "Stem Radius (Observed)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.[0].Series.[ShortCode.create "x"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "Stem Radius (Model)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.[0].Series.[ShortCode.create "N"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "Nitrogen (Observed)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.[0].Series.[ShortCode.create "N"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "Nitrogen (Model)" ]) |> ignore

// iii. For each parameter, find mean and standard deviation, and plot graph with histogram behind it

let parameterValues = 
    [0 .. ``nutrient-dependent growth``.Parameters.Count - 1]
    |> List.map(fun i ->
            estimated.[0].Trace
            |> snd
            |> List.map (fun x -> x.[i]) )

[0 .. ``nutrient-dependent growth``.Parameters.Count - 1]
|> List.map (fun i -> R.plot( namedParams [ "x", box (parameterValues.[i] |> List.rev |> List.skip 100) ; "type", box "l"; "xlab", box "Iteration"; "ylab", box "parameter value" ]) |> ignore)

// Alternative: kernel density plot for each parameter
open RProvider.stats

[0 .. ``nutrient-dependent growth``.Parameters.Count - 1]
|> List.map (fun i -> 
    let d = parameterValues.[i] |> List.rev |> List.skip 100 |> R.density
    R.plot(d) )
