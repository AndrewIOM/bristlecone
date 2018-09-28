#load "../src/bristlecone.fsx"

////////////////////////////////////////////////////
/// Yamal Salix lanata Shrub - Nitrogen Interactions
////////////////////////////////////////////////////

// Shrub ring width modelled with a single
// resource limitation. The models presented are based on
// Tilman (1990 / 1988).

open Bristlecone
open Bristlecone.ModelSystem
open Bristlecone.PlantIndividual

// 0. Configure Options
// ----------------------------

module Options =

    let resolution = Annual
    let iterations = 500
    let burn = 500
    let chains = 5
    let testSeriesLength = 40


module Constants =

    // Empirically-derived parameters:
    let k5 = 19.98239 // Allometric fit to Yamal shrub BD-length data #1 (in centimetres)
    let k6 = 0.42092 // Allometric fit to Yamal shrub BD-length data #2 (in centimetres)

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

    module NiklasAndSpatz_Allometry =

        let nthroot n A =
            let rec f x =
                let m = n - 1.
                let x' = (m * x + A/x**m) / n
                match abs(x' - x) with
                | t when t < abs(x * 1e-9) -> x'
                | _ -> f x'
            f (A / double n)

        /// Gives the basal radius in centimeters of a stem/branch given its length in centimeters. Function from Niklas and Spatz (2004). 
        let basalRadius k5 k6 stemLength =
            100. * (( 0.01 * stemLength + k6) / k5) ** (3. / 2.) / 2.

        /// Inverse equation of basalRadius, rearranged using Wolfram Alpha
        /// http://www.wolframalpha.com/input/?i=solve+r+%3D+100*((0.01*h%2Bk_6)%2Fk_5)%5E(3%2F2)%2F2+for+h
        let stemLength k5 k6 radius =
            2. * ((nthroot 3. 2.) * 5. ** (2./3.) * k5 * radius ** (2./3.) - 50. * k6)

    module Götmark2016_ShrubModel =

        /// Total shrub volume given height and number of stems
        let shrubVolume b a rtip p lmin k5 k6 n h =

            let radius = NiklasAndSpatz_Allometry.basalRadius k5 k6
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

        let mass woodDensity volume =
            volume * woodDensity

        let massToVolume woodDensity mass =
            mass / woodDensity

        let shrubBiomass b a rtip p lmin k5 k6 n woodDensity radius =
            radius
            |> NiklasAndSpatz_Allometry.stemLength k5 k6
            |> shrubVolume b a rtip p lmin k5 k6 n |> snd
            |> mass woodDensity

        let shrubRadius b a rtip p lmin k5 k6 n woodDensity mass =
            let findRadius volume =
                let v x = x |> NiklasAndSpatz_Allometry.stemLength k5 k6 |> shrubVolume b a rtip p lmin k5 k6 n |> snd
                let f = (fun x -> (v x) - volume )
                Optimisation.RootFinding.bisect 0 200 f 0.01 1000000.00 1e-8 // Assumption that shrub radius is between 0.01 and 100.0cm.
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

        /// From Jabot and Pottier 2012
        let monod k (r:float) =
            r / (k + r)

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

/// Radius in millimetres
let toBiomass radius = 
    radius / 10. |> ModelComponents.Allometrics.shrubBiomass Constants.b Constants.a Constants.rtip Constants.p Constants.lmin Constants.k5 Constants.k6 Constants.numberOfStems Constants.salixWoodDensity

let ``nutrient-dependent growth`` =

    /// d15N to N availability. From Craine 2009, as shown in Craine 2015 (Plant and Soil).
    /// Assuming d15N is a linear index of N availability, the minimum supported value of d15N is -3.09, as 0 N availability.
    let d15NtoAvailability d15N =
        (100. * d15N + 309.) / 359.

    /// Biomass in grams
    let toRadius biomass = 
        let radiusCm = biomass |> ModelComponents.Allometrics.shrubRadius Constants.b Constants.a Constants.rtip Constants.p Constants.lmin Constants.k5 Constants.k6 Constants.numberOfStems Constants.salixWoodDensity
        radiusCm * 10.

    /// Resource-dependent growth function
    /// AKA Specific growth rate
    /// Maximum rate of photosynthesis per unit of photosynthetic tissue under optimal conditions
    let f = ModelComponents.GrowthRate.michaelisMenten
    
    /// Cumulative stem biomass [bs].
    let dbsdt' bs n fMax h m r = 
        let photosyntheticCapacity = bs * (f fMax h n) - r * bs
        photosyntheticCapacity - m * bs

    /// Bioavailable soil nitrogen [N]
    let dndt' (bs:float) (n:float) lambda gamman q fMax h r : float =
        let photosyntheticCapacity = bs * (f fMax h n) - r * bs
        lambda - q * photosyntheticCapacity - gamman * n

    /// Measurement variable: stem radius [rw].
    /// If there has been biomass accumulation during the time interval, wood accumulation occurs, according to shrub allometric relationships. 
    let drwdt' bs n fMax h m r = 
        let biomassStemChange = dbsdt' bs n fMax h m r
        if biomassStemChange > 0.
            then
                let oldRadius = bs |> toRadius
                let newRadius = (bs + biomassStemChange) |> toRadius
                newRadius - oldRadius
            else 0.

    /// Bristlecone function for dBs/dt
    let dbsdt p _ bs (e:Environment) =
        dbsdt' bs ((e.[ShortCode.create "N"]) |> d15NtoAvailability) (p |> Pool.getEstimate "fMax") (p |> Pool.getEstimate "h")
            (p |> Pool.getEstimate "gammab") (p |> Pool.getEstimate "r")

    /// Bristlecone function for dN/dt
    let dndt p _ n (e:Environment) =
        dndt' (e.[ShortCode.create "x"]) (n |> d15NtoAvailability) (p |> Pool.getEstimate "lambda") (p |> Pool.getEstimate "gamman") 
            (p |> Pool.getEstimate "q") (p |> Pool.getEstimate "fMax") (p |> Pool.getEstimate "h") (p |> Pool.getEstimate "r")

    /// Bristlecone function for dr/dt
    let drwdt p _ _ (e:Environment) =
        drwdt' (e.[ShortCode.create "bs"]) ((e.[ShortCode.create "N"]) |> d15NtoAvailability) (p |> Pool.getEstimate "fMax") 
            (p |> Pool.getEstimate "h") (p |> Pool.getEstimate "gammab") (p |> Pool.getEstimate "r")

    { Equations  = [ ShortCode.create "x",      drwdt
                     ShortCode.create "bs",     dbsdt
                     ShortCode.create "N",      dndt ] |> Map.ofList
      Parameters = [ // for growth function
                     ShortCode.create "fMax",   Parameter.create Unconstrained   0.500 1.000   // Maximum rate of resource-saturated growth per unit biomass
                     ShortCode.create "h",      Parameter.create Unconstrained   0.100 1.000   // Soil resource availability for growth at half of maximum rate
                     // for nitrogen replenishment
                     ShortCode.create "lambda", Parameter.create Unconstrained   0.010 0.200   // Rate of nitrogen replenishment
                     ShortCode.create "gamman", Parameter.create Unconstrained   0.200 0.300   // Loss rate of nitrogen
                     // for shrub allocation and physiology
                     ShortCode.create "r",      Parameter.create Unconstrained   0.001 0.500   // Respiration cost per unit biomass
                     ShortCode.create "q",      Parameter.create Unconstrained   0.001 0.100  // Nutrient requirement per unit biomass
                     ShortCode.create "gammab", Parameter.create Unconstrained   0.001 0.200  // Loss rate of biomass
                     // for likelihood function
                     ShortCode.create "rho",    Parameter.create Unconstrained  -0.20 0.20   // Covariance between growth and nitrogen
                     ShortCode.create "sigmax", Parameter.create Unconstrained   0.10 0.50   // Standard deviation of x (biomass)
                     ShortCode.create "sigmay", Parameter.create Unconstrained   0.10 0.50   // Standard deviation of y (nitrogen)
                    ] |> Map.ofList
      Likelihood = ModelLibrary.Likelihood.bivariateGaussian "x" "N" }


// 2. Test Hypotheses Work
// ----------------------------

let engine = 
    Bristlecone.mkContinuous 
    |> Bristlecone.withConditioning RepeatFirstDataPoint

let startValues = [ ShortCode.create "x", 0.23; ShortCode.create "N", 4.64; ShortCode.create "bs", 471.5475542] |> Map.ofList

``nutrient-dependent growth``
|> Bristlecone.testModel engine Options.testSeriesLength startValues Options.iterations Options.burn


// 3. Load Real Data and Estimate
// ----------------------------

let shrubs = 
    let yuribei = DataAccess.Shrub.loadRingWidths (__SOURCE_DIRECTORY__ + "/data/yuribei-rw.csv")
    let d15N = DataAccess.Shrub.loadLocalEnvironmentVariable (__SOURCE_DIRECTORY__ + "/data/yuribei-d15N-imputed.csv")
    yuribei
    |> Seq.map (fun s -> s.Identifier.Value, s)
    |> Seq.keyMatch d15N
    |> Seq.map (fun (_,plant,d15N) -> PlantIndividual.zipEnv (ShortCode.create "N") plant d15N)
    |> Seq.toList

// Start value configuration
let getStartValues startDate (plant:PlantIndividual) =
    let initialRadius =
        match plant.Growth with
        | PlantIndividual.PlantGrowth.RingWidth s -> 
            match s with
            | GrowthSeries.Absolute c -> c.Head |> fst |> removeUnit
            | GrowthSeries.Cumulative c -> 6.3 //c.Head |> fst |> removeUnit
            | _ -> invalidOp "Not implemented"
        | _ -> invalidOp "Not implemented 2"
    let initialMass = initialRadius |> removeUnit |> toBiomass
    let initialNitrogen = plant.Environment.[ShortCode.create "N"].Head |> fst
    [ ShortCode.create "x", initialRadius
      ShortCode.create "N", initialNitrogen 
      ShortCode.create "bs", initialMass ] |> Map.ofList

// Match environment and growth lengths
// If n-1 time point has a value in the original set, append to new set

// Run 4 chains at once to assess convergence

let estimated =
    [| 1 .. Options.chains |]
    |> Array.Parallel.map(fun _ ->
        [shrubs.[0]] |> List.map (fun s ->
            let shrub = s |> PlantIndividual.toCumulativeGrowth
            let common = shrub |> PlantIndividual.keepCommonYears
            let startDate = common.Environment.[ShortCode.create "N"] |> TimeSeries.start
            let startConditions = getStartValues startDate shrub
            common
            |> Bristlecone.PlantIndividual.fit engine Options.iterations Options.burn ``nutrient-dependent growth`` ))


// 4. Plot Results (using R)
// ----------------------------

#load "plotting.fsx"
open RProvider
open RProvider.graphics
open RProvider.ggplot2
open RProvider.grDevices
R.x11()

// i. Plot likelihood by iteration (for MCMC)
R.plot( namedParams [ "x", box (estimated.[0].[0].Trace |> fst |> List.rev |> List.skip 1000) ; "type", box "l"; "xlab", box "Iteration"; "ylab", box "-log likelihood" ]) |> ignore

// i. Estimated versus Observed Series
R.par(namedParams [ "mfrow", [2;2]] ) |> ignore
R.plot (namedParams [ "x",box estimated.[0].[0].Series.[ShortCode.create "x"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "Stem Radius (Observed)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.[0].[0].Series.[ShortCode.create "x"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "Stem Radius (Model)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.[0].[0].Series.[ShortCode.create "N"].Observed; "type", box "l"; "xlab", box "Year"; "ylab", box "Nitrogen (Observed)" ]) |> ignore
R.plot (namedParams [ "x",box estimated.[0].[0].Series.[ShortCode.create "N"].Expected; "type", box "l"; "xlab", box "Year"; "ylab", box "Nitrogen (Model)" ]) |> ignore

// iii. For each parameter, find mean and standard deviation, and plot graph with histogram behind it

let burnin = 0

let everyNth n seq = 
    seq |> Seq.mapi (fun i el -> el, i)              // Add index to element
        |> Seq.filter (fun (el, i) -> i % n = n - 1) // Take every nth element
        |> Seq.map fst                               // Drop index from the result

let chainsDataFrame =
    estimated
    |> Array.mapi (fun chainNumber chain -> 
        chain.[0].Trace
        |> snd
        |> List.rev
        |> List.skip burnin // Skip burn in period
        |> everyNth 5 // Thinning by this amount
        |> Seq.mapi (fun iterationNumber values ->
            chain.[0].Parameters
            |> Map.toList
            |> List.mapi(fun i (name,p) -> 
                iterationNumber + 1,
                chainNumber + 1,
                name.Value,
                values.[i] )
        )
        |> List.concat
        |> Seq.toList
    )
    |> Array.toList
    |> List.concat

let df =
    namedParams [
        "Iteration", chainsDataFrame |> List.map(fun (a,_,_,_) -> a) |> box 
        "Chain", chainsDataFrame |> List.map(fun (_,a,_,_) -> a) |> box
        "Parameter", chainsDataFrame |> List.map(fun (_,_,a,_) -> a) |> box
        "value", chainsDataFrame |> List.map(fun (_,_,_,a) -> a) |> box
    ]
    |> R.data_frame

// // Generate MCMC report
// open RProvider.ggmcmc
// df.SetAttribute("nParameters", R.integer ``nutrient-dependent growth``.Parameters.Count )
// df.SetAttribute("nChains", R.integer Options.chains)
// df.SetAttribute("nIterations", R.integer Options.iterations)
// df.SetAttribute("nBurnin", R.integer burnin)
// df.SetAttribute("nThin", R.integer 5)
// df.SetAttribute("description", R.toString "Bristlecone MCMC result")
// R.ggmcmc(df)

// Save out R data frame of MCMC results (for use later from R etc.)
R.assign("chains", df)
R.save(list = ["chains"], file = "dphil-shrub-chain.rdata")


/// Plot allometrics

/// Radius in millimetres
let toBiomass stems radius = 
    radius |> ModelComponents.Allometrics.shrubBiomass Constants.b Constants.a Constants.rtip Constants.p Constants.lmin Constants.k5 Constants.k6 stems Constants.salixWoodDensity

/// Biomass in grams
let toRadius biomass = 
    let radiusCm = biomass |> ModelComponents.Allometrics.shrubRadius Constants.b Constants.a Constants.rtip Constants.p Constants.lmin Constants.k5 Constants.k6 Constants.numberOfStems Constants.salixWoodDensity
    radiusCm

let r,b,s =
    [ 2.]
    |> List.collect(fun stems ->
        [ 1. .. 5. ]
        |> List.map(fun r -> r, toBiomass (float stems) r, float stems ) )
    |> List.unzip3

let df = 
    namedParams [
        "StemRadius", r |> box
        "Biomass", b |> box
        "StemCount", s |> box ]
    |> R.data_frame

let (++) (plot1:RDotNet.SymbolicExpression) (plot2:RDotNet.SymbolicExpression) = 
    R.``+``(plot1, plot2)


R.ggplot(df, R.aes__string(namedParams [ "x", box "StemRadius"; "y", box "Biomass"; "color", box "StemCount" ])) 
++ R.geom__line()

1000000. |> toRadius