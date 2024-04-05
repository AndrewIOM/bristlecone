module Allometric

#load "../bristlecone.fsx"

open Bristlecone

let pi = System.Math.PI

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

module NiklasAndSpatz_Allometry =

    let nthroot n A =
        let rec f x =
            let m = n - 1.
            let x' = (m * x + A/x**m) / n
            match abs(x' - x) with
            | t when t < abs(x * 1e-9) -> x'
            | _ -> f x'
        f (A / double n)

    /// Gives the basal radius in centimetres of a stem/branch given its length in centimetres. Function from Niklas and Spatz (2004). 
    /// The basal radius is always positive.
    let basalRadius k5 k6 stemLength =
        max (100. * (( 0.01 * stemLength + k6) / k5) ** (3. / 2.) / 2.) 1e-06

    /// Inverse equation of basalRadius.
    let stemLength k5 k6 radius =
        max (2. * ((nthroot 3. 2.) * 5. ** (2./3.) * k5 * radius ** (2./3.) - 50. * k6)) 1e-06

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
    open Bristlecone.Dendro

    let private removeUnit (x:float<_>) = float x

    let mass woodDensity volume =
        volume * woodDensity

    let massToVolume woodDensity mass =
        mass / woodDensity

    let shrubBiomass b a rtip p lmin k5 k6 n woodDensity (radius:float<mm>) =
        radius
        |> removeUnit
        |> NiklasAndSpatz_Allometry.stemLength k5 k6
        |> shrubVolume b a rtip p lmin k5 k6 n |> snd
        |> mass woodDensity

    let shrubRadius b a rtip p lmin k5 k6 n woodDensity mass =
        let findRadius volume =
            let v x = x |> NiklasAndSpatz_Allometry.stemLength k5 k6 |> shrubVolume b a rtip p lmin k5 k6 n |> snd
            let f = (fun x -> (v x) - volume )
            Statistics.RootFinding.bisect 0 200 f 0.01 100.00 1e-8 // Assumption that shrub radius is between 0.01 and 100.0cm.
        mass
        |> massToVolume woodDensity
        |> findRadius

    let shrubHeight k5 k6 radius =
        radius |> NiklasAndSpatz_Allometry.stemLength k5 k6

module Proxies =

    open Bristlecone.Dendro

    /// Radius in millimetres
    let toBiomassMM (radius:float<mm>) = 
        radius / 10. |> Allometrics.shrubBiomass Constants.b Constants.a Constants.rtip Constants.p Constants.lmin Constants.k5 Constants.k6 Constants.numberOfStems Constants.salixWoodDensity

    /// Biomass in grams.
    let toRadiusMM biomassGrams = 
        if System.Double.IsNaN biomassGrams || System.Double.IsInfinity biomassGrams || System.Double.IsNegativeInfinity biomassGrams then nan
        else
            let radiusCm = biomassGrams |> Allometrics.shrubRadius Constants.b Constants.a Constants.rtip Constants.p Constants.lmin Constants.k5 Constants.k6 Constants.numberOfStems Constants.salixWoodDensity
            radiusCm * 10.
