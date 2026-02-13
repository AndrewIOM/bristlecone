/// Test functions for optimisation problems implemented in F#. Source:
///
/// Surjanovic, S. and Bingham, D. (2013). Virtual Library of
/// Simulation Experiments: Test Functions and Datasets.
/// Retrieved June 25, 2020, from http://www.sfu.ca/~ssurjano.
module TestFunctions

let private pi = System.Math.PI

/// Bounds a parameter to be NaN if outside of the specified bounds
let inline (|WithinBounds|) low high v =
    match v with
    | v when v < low -> nan
    | v when v > high -> nan
    | _ -> v

module LocalMinima =

    let ackley' a b c x =
        let d = x |> Array.length |> float
        let exp1 = -b * sqrt ((1. / d) * (x |> Array.sumBy (fun x -> x ** 2.)))
        let exp2 = (1. / d) * (x |> Array.sumBy (fun x -> cos (c * x)))
        -a * exp exp1 - exp exp2 + a + exp 1.

    let ackley x =
        ackley' 20. 0.2 (2. * pi) (x |> Array.map ((|WithinBounds|) -32.768 32.768))

    let bukinSixth (WithinBounds -15.0 5.0 x1) (WithinBounds -3.0 3.0 x2) =
        100. * sqrt (abs (x2 - 0.01 * x1 ** 2.)) + 0.01 * abs (x1 + 10.)

    let crossInTray (WithinBounds -10.0 10.0 x1) (WithinBounds -10.0 10.0 x2) =
        -0.0001
        * (abs (sin (x1) * sin (x2) * exp (abs (100. - sqrt (x1 ** 2. + x2 ** 2.) / pi)))
           + 1.)
          ** 0.1

    let dropWave (WithinBounds -5.12 5.12 x1) (WithinBounds -5.12 5.12 x2) =
        -(1. + cos (12. * sqrt (x1 ** 2. + x2 ** 2.)))
        / (0.5 * (x1 ** 2. + x2 ** 2.) + 2.)

    let eggHolder (WithinBounds -512.0 512.0 x1) (WithinBounds -512.0 512.0 x2) =
        -(x2 + 47.) * sin (sqrt (abs (x2 + (x1 / 2.) + 47.)))
        - x1 * sin (sqrt (abs (x1 - (x2 + 47.))))

    let gramacyLee (WithinBounds 0.5 2.5 x) =
        (sin (10. * pi * x) / (2. * x)) + (x - 1.) ** 4.

    let griewank x =
        let x = x |> Array.map ((|WithinBounds|) -600. 600.)
        let f x = (x ** 2. / 4000.)
        let g i x = cos (x / sqrt (float (i + 1)))
        1. + (x |> Array.sumBy f) - (x |> Array.mapi g |> Array.fold (*) 1.)

    let holderTable (WithinBounds -10.0 10.0 x1) (WithinBounds -10.0 10.0 x2) =
        -abs(sin x1 * cos x2 * exp (abs (1. - (sqrt (x1 ** 2. + x2 ** 2.) / pi))))

    let langermann' m (a: float[,]) (c: float[]) x =
        let x = x |> Array.map ((|WithinBounds|) 0. 10.)
        let d = x |> Array.length

        [ 1..m ]
        |> List.sumBy (fun i ->
            let z1 = ([ 1..d ] |> List.sumBy (fun j -> (x.[j - 1] - a.[i - 1, j - 1]) ** 2.))
            c.[i - 1] * exp ((-1. / pi) * z1) * cos (pi * z1))
        |> (*) -1.

    /// 2D Langermann function using recommended values of A and c
    let langermann x1 x2 =
        let a =
            [| [| 3.; 5. |]; [| 5.; 2. |]; [| 2.; 1. |]; [| 1.; 4. |]; [| 7.; 9. |] |]
            |> array2D

        let c = [| 1.; 2.; 5.; 2.; 3. |]
        langermann' 5 a c [| x1; x2 |]

    let rastrigin x =
        let x = x |> Array.map ((|WithinBounds|) -5.12 5.12)
        let a = 10.
        let z = x |> Array.sumBy (fun x -> x ** 2. - a * cos (2. * pi * x))
        a * float x.Length + z


module Timeseries =

    open Bristlecone
    open Bristlecone.Language
    open Bristlecone.Time
    open Bristlecone.ModelLibrary

    /// Classical Lotka–Volterra model to test identifiability of parameters
    /// in a non-linear ODE system based on count data.
    module PredatorPrey =

        [<Measure>] type prey = 1
        [<Measure>] type predator = 1

        // States
        let H = state<1> "prey" // dimensionless count
        let L = state<1> "predator" // dimensionless count

        let predatorPreyBase =

            // Parameters
            let α = parameter "α" Positive 0.5<1/year> 1.5<1/year> // Maximum prey per‑capita growth rate
            let β = parameter "β" Positive 0.01<1/(predator * year)> 0.05<1/(predator * year)> // Predation rate coefficient
            let γ = parameter "γ" Positive 0.1<1/year> 1.0<1/year> // Predator natural mortality rate
            let δ = parameter "δ" Positive 0.01<1/(prey * year)> 0.1<1/(prey * year)> // Predator growth efficiency

            let ``dH/dt``: ModelExpression<prey / year> =
                (P α - P β * State L) * This<prey>

            let ``dL/dt``: ModelExpression<predator / year> =
                (-P γ + P δ * State H) * This<predator>

            Model.empty<year>
            |> Model.addRateEquation H ``dH/dt``
            |> Model.addRateEquation L ``dL/dt``
            |> Model.estimateParameter α // Natural growth rate of hares in absence of predation
            |> Model.estimateParameter β // Death rate per encounter of hares due to predation
            |> Model.estimateParameter γ // Natural death rate of lynx in the absence of food
            |> Model.estimateParameter δ // Efficiency of turning predated hares into lynx

        let deterministic =
            predatorPreyBase
            |> Model.useLikelihoodFunction (
                NegLogLikelihood.SumOfSquares [ Require.state H; Require.state L ]
            )
            |> Model.compile

        let σH = parameter "σ[H]" Positive 0.001 0.100
        let σL = parameter "σ[L]" Positive 0.001 0.100

        let noisy =

            let NLL =
                NegLogLikelihood.LogNormal (Require.state H) σH +
                NegLogLikelihood.LogNormal (Require.state L) σL

            predatorPreyBase
            |> Model.useLikelihoodFunction NLL
            |> Model.compile

        let test withNoise logger optimise endCondition =
            
            let engine =
                Bristlecone.mkContinuous ()
                |> Bristlecone.withCustomOptimisation optimise
                |> Bristlecone.withOutput logger
                |> Bristlecone.withTimeConversion DateMode.Conversion.Annual.toYears

            let settings =
                Test.annualSettings
                |> Test.seriesLength 30
                |> Test.withObservationError (Require.state H) (Test.Error.normal σH)
                |> Test.withObservationError (Require.state L) (Test.Error.normal σL)
                |> Test.rule (Require.state H) (Test.GenerationRules.between 10. 10000.)
                |> Test.rule (Require.state L) (Test.GenerationRules.between 10. 10000.)
                |> Test.t1 (Require.state H) 1.0
                |> Test.t1 (Require.state L) 1.0

            Bristlecone.tryTestModel engine endCondition settings (if withNoise then noisy else deterministic)


    /// A two-compartment soil carbon model that represents the fast turnover
    /// litter pool versus the slower (recalcitrant) litter pool.
    module SoilCarbon =

        [<Measure>] type gram
        [<Measure>] type metre
        [<Measure>] type area = metre^2
        [<Measure>] type celsius

        // States
        let Cf = state<gram/area> "Cf" // fast pool
        let Cs = state<gram/area> "Cs" // slow pool

        // Environmental input: soil temperature
        let Ts = environment "T_soil"

        // Measurement variables
        let R = measure "respiration"

        let soilCarbonBase =

            // Parameters
            let kf = parameter "kf" Positive 0.001< / day> 0.1< / day> // fast pool base decay rate
            let ks = parameter "ks" Positive 0.0001< / day> 0.01< / day> // slow pool base decay rate
            let Q10 = parameter "Q10" Positive 1.5 3.0 // temperature sensitivity
            let alpha = parameter "alpha" Positive 0.1 0.9 // efficiency of humification

            // Temperature modifier (Q10 function relative to 10 °C)
            let tempEffect = P Q10 ** ((Environment Ts - Constant 10.0<celsius>) / Constant 10.0<celsius>)

            // Rate equations
            let ``dCf/dt``: ModelExpression<(gram/area)/day> = -(P kf * tempEffect) * This
            let ``dCs/dt``: ModelExpression<(gram/area)/day> =
                P alpha * (P kf * tempEffect) * Environment Cf - (P ks * tempEffect) * This

            // Derived measurement: soil respiration flux
            let respiration: ModelExpression<(gram/area)/day> =
                (P kf * tempEffect) * State Cf + (P ks * tempEffect) * State Cs

            Model.empty
            |> Model.addRateEquation Cf ``dCf/dt``
            |> Model.addRateEquation Cs ``dCs/dt``
            |> Model.addMeasure R respiration
            |> Model.estimateParameter kf
            |> Model.estimateParameter ks
            |> Model.estimateParameter Q10
            |> Model.estimateParameter alpha

        // Deterministic fit
        let deterministic =
            soilCarbonBase
            |> Model.useLikelihoodFunction (NegLogLikelihood.SumOfSquares [ Require.measure R ])
            |> Model.compile

        let sigma = parameter "σ" Positive 0.001<(gram/area)/day> 10.0<(gram/area)/day>

        // With Gaussian observation noise
        let noisy =
            let NLL = NegLogLikelihood.Normal (Require.measure R) sigma
            soilCarbonBase
            |> Model.useLikelihoodFunction NLL
            |> Model.compile

        let test withNoise logger optimise endCondition =
            
            let engine =
                Bristlecone.mkContinuous ()
                |> Bristlecone.withCustomOptimisation optimise
                |> Bristlecone.withOutput logger
                |> Bristlecone.withTimeConversion DateMode.Conversion.CalendarDates.toDays

            let mkTemperature =
                Dendro.Environment.Synthetic.genTemperatureSeasonal
                    10.<celsius> 10.<celsius> 365.<day> 0.

            let settings =
                Test.defaultSettings
                |> Test.resolution (Daily 1<day>)
                |> Test.seriesLength 30
                |> Test.withObservationError (Require.measure R) (Test.Error.normal sigma)
                |> Test.rule (Require.measure R) (Test.GenerationRules.alwaysLessThan 1000.)
                |> Test.withEnvironmentGenBySpan Ts mkTemperature (fun x -> float x.Days * 1.<day>)

            Bristlecone.tryTestModel engine endCondition settings (if withNoise then noisy else deterministic)


    /// A discrete-time test model, which represents the temperature-dependence
    /// of population growth.
    module RickerTemperature =

        [<Measure>] type individuals = 1
        [<Measure>] type celsius

        let N = state<individuals> "N"
        let logN = measure "logN"
        let temperatureAnomaly = environment<celsius> "T"

        let rickerBase: ModelBuilder.ModelBuilder<day> =

            // Parameters
            let r = parameter "r" NoConstraints 0.1 2.0 // maximum per‑capita growth rate (intrinsic growth rate)
            let K = parameter "K" Positive 50.<individuals> 500.<individuals> // carrying capacity
            let beta = parameter "beta" Positive -1.0</celsius> 1.0</celsius> // temperature effect

            let ``N[t+1]``: ModelExpression<individuals> =
                This * exp (P r * (Constant 1. - This / P K) + P beta * Environment temperatureAnomaly)

            let eqLogNM = State N |> Logarithm

            Model.discrete
            |> Model.addDiscreteEquation N ``N[t+1]``
            |> Model.addMeasure logN eqLogNM
            |> Model.estimateParameter r
            |> Model.estimateParameter K
            |> Model.estimateParameter beta

        let deterministic =
            rickerBase
            |> Model.useLikelihoodFunction (NegLogLikelihood.SumOfSquares [ Require.measure logN ])
            |> Model.compile

        let sigma = parameter "σ" Positive 0.001 10.0

        let noisy =
            let NLL = NegLogLikelihood.Normal (Require.measure logN) sigma

            rickerBase
            |> Model.useLikelihoodFunction NLL
            |> Model.compile

        let test withNoise logger optimise endCondition =
            
            let engine =
                Bristlecone.mkDiscrete ()
                |> Bristlecone.withCustomOptimisation optimise
                |> Bristlecone.withOutput logger
                |> Bristlecone.withTimeConversion DateMode.Conversion.CalendarDates.toDays

            let settings =
                Test.create
                |> Test.seriesLength 30
                |> Test.withObservationError (Require.measure logN) (Test.Error.normal sigma)
                |> Test.rule (Require.measure logN) (Test.GenerationRules.alwaysLessThan 1000.)
                |> Test.t1 (Require.measure logN) 1.0

            Bristlecone.tryTestModel engine endCondition settings (if withNoise then noisy else deterministic)
