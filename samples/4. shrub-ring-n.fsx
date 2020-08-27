#load "bristlecone.fsx"

////////////////////////////////////////////////////
/// Growth of wood rings in response to nitrogen
////////////////////////////////////////////////////

open Bristlecone
open Bristlecone.Language

// 1. Base model
// ________________________________

module BaseModel =

    /// Transform δ15N to N availability.
    let nAvailability =
        (Constant 100. * Environment "N" + Constant 309.) / Constant 359.

    /// Plant uptake of N from soil, which may be turned on or off
    let uptake f geom =
        (geom This) * This * (f nAvailability)

    /// Cumulative stem biomass
    let biomass geom nLimitation =
        let dbdt f = Parameter "r" * (uptake f geom) - Parameter "γ[b]" * This
        match nLimitation with
        | Some f -> dbdt f
        | None -> dbdt (fun _ -> Constant 1.)

    let soilNitrogen geom feedback nLimitation =          
        let dndt = Parameter "λ" - Parameter "γ[N]" * nAvailability + feedback This
        match nLimitation with
        | Some f -> dndt - (uptake f geom)
        | None -> dndt

    /// Measurement (Size) variable: stem radius
    let stemRadius lastRadius lastEnv env =
        let oldCumulativeMass = lookup lastEnv "bs"
        let newCumulativeMass = lookup env "bs"
        if (newCumulativeMass - oldCumulativeMass) > 0.
        then newCumulativeMass |> toRadiusMM
        else lastRadius

    let baseModel geom feedback nLimitation =
        Model.empty
        |> Model.addEquation        "bs"    (biomass geom nLimitation)
        |> Model.addEquation        "N"     (soilNitrogen geom feedback nLimitation)
        |> Model.includeMeasure     "x"     stemRadius
        |> Model.estimateParameter  "λ"     notNegative 0.001 0.500
        |> Model.estimateParameter  "γ[N]"  notNegative 0.001 0.200
        |> Model.estimateParameter  "γ[b]"  notNegative 0.001 0.200
        |> Model.useLikelihoodFunction sumOfSquares // (bivariateGaussian "x" "N")
        |> Model.estimateParameter  "ρ"     noConstraints -0.500 0.500
        |> Model.estimateParameter  "σ[x]"  notNegative 0.001 0.100
        |> Model.estimateParameter  "σ[y]"  notNegative 0.001 0.100



let hypothesis =

    /// The universal gas constant in J mol−1 K−1
    let gasConstant = 8.314

    /// An Arrhenius function to represent temperature limitation on growth.
    /// Form of equation from paper: https://pubag.nal.usda.gov/download/13565/PDF
    let temperatureLimitation activationEnergy temperature =
        //System.Math.E ** (- ((activationEnergy * 1000.) / (8.314 * temperature)))
        System.Math.E ** ((1000. * activationEnergy * (temperature - 298.)) / (298. * gasConstant * temperature))

    /// Plant growth is a function of net photosynthesis minus environmental losses.
    /// Photosynthesis is limited by light and temperature.
    let dbdt' b r gammab tempEffect : float =
        b * r * tempEffect - gammab * b

    /// This function plugs the parameter estimates and environmental data into
    /// the basic mathematical function.
    let dbdt p t biomass (e:Environment) =
        dbdt' biomass (p |> Pool.getEstimate "r") (p |> Pool.getEstimate "gamma[b]") 
            (temperatureLimitation (p |> Pool.getEstimate "ae") (lookup e "t"))

    /// Bristlecone function for dr/dt
    { Equations  = [ code "b",        dbdt      ] |> Map.ofList
      Measures   = [                            ] |> Map.ofList
      Parameters = [ code "ae",         parameter Unconstrained  40.00 50.00     // Activation energy
                     code "gamma[b]",   parameter Unconstrained  0.001 0.400     // Loss rate of biomass
                     code "r",          parameter Unconstrained  0.500 10.00     // Intrinsic growth rate
                   ] |>  Map.ofList
      Likelihood = ModelLibrary.Likelihood.sumOfSquares ["b"] }


module TemperatureData =

    open FSharp.Data

    [<Literal>] 
    let TemperatureUrl = __SOURCE_DIRECTORY__ + "/data/yuribei-meantemp.csv"

    let monthly =
        let maxTemperatures = CsvProvider<TemperatureUrl>.Load TemperatureUrl
        maxTemperatures.Rows
        |> Seq.map(fun r -> ( (if System.Double.IsNaN r.``T[avg]`` then None else Some (r.``T[avg]`` + 273.15)), r.Date))
        |> TimeSeries.fromObservations
        |> TimeSeries.interpolate
        |> TimeSeries.generalise (Months 1) (fun x -> x |> Seq.averageBy fst)


module Test =

    open Bristlecone.Test

    module GenerationRules =

        /// Ensures that all generated values are less than i
        let alwaysLessThan i variable : GenerationRule =
            code variable, fun data -> data |> Seq.max < i

        /// Ensures that all generated values are greater than i
        let alwaysMoreThan i variable : GenerationRule =
            code variable, fun data -> data |> Seq.min > i

        /// Ensures that there is always a positive change in values of a variable
        let monotonicallyIncreasing variable : GenerationRule =
            code variable, fun data -> 
                data |> Seq.pairwise |> Seq.map(fun (x1,x2) -> (x2 - x1) > 0.) |> Seq.contains false 


    let testSettings = {
        Resolution = Years 1
        TimeSeriesLength = 30
        StartValues = [ code "b", 5.
                        code "t", 255. ] |> Map.ofList
        EndCondition = Settings.endWhen
        GenerationRules = [ "b" |> GenerationRules.alwaysLessThan 1000000.
                            "b" |> GenerationRules.alwaysMoreThan 0.
                            code "b", fun data -> (data |> Seq.max) - (data |> Seq.min) > 100. ]
        NoiseGeneration = fun p data -> data
        EnvironmentalData = [ code "t", TemperatureData.monthly ] |> Map.ofList
        Random = MathNet.Numerics.Random.MersenneTwister()
        StartDate = System.DateTime(1970,01,01)  
        Attempts = 50000 }

    let run () =
        hypothesis
        |> Bristlecone.testModel Settings.engine testSettings


let result = Test.run()
