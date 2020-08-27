#load "bristlecone.fsx"

////////////////////////////////////////////////////
/// Growth of tree rings in response to temperature
////////////////////////////////////////////////////

// This Bristlecone example demonstrates model-fitting
// using high-resolution monthly temperature data with
// annual ring width increments.

open Bristlecone
open Bristlecone.ModelSystem


module Settings =

    /// Bristlecone uses latitude and longitude to compute day length.
    let latitude, longitude = 63.2, 15.2

    let endWhen = Optimisation.EndConditions.afterIteration 1

    let logger = Logging.RealTimeTrace.graphWithConsole 15. 1000

    let engine =
        Bristlecone.mkContinuous 
        |> Bristlecone.withContinuousTime Integration.MathNet.integrate
        |> Bristlecone.withOutput logger
        |> Bristlecone.withTunedMCMC [ Optimisation.MonteCarlo.TuneMethod.CovarianceWithScale 0.200, 250, Optimisation.EndConditions.afterIteration 20000 ]


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
