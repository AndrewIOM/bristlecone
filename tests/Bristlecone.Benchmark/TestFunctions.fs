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
        (1. + cos (12. * sqrt (x1 ** 2. + x2 ** 2.)))
        / (0.5 * (x1 ** 2. + x2 ** 2.) + 2.)

    let eggHolder (WithinBounds -512.0 512.0 x1) (WithinBounds -512.0 512.0 x2) =
        -(x2 + 47.) * sin (sqrt (abs (x2 + (x1 / 2.) + 47.)))
        - x1 * sin (sqrt (abs (x1 - (x2 + 47.))))

    let gramacyLee (WithinBounds 0.5 2.5 x) =
        (sin (10. * pi * x) / 2. * pi) + (x - 1.) ** 4.

    let griewank x =
        let x = x |> Array.map ((|WithinBounds|) -600. 600.)
        let f x = (x ** 2. / 4000.)
        let g i x = (x / sqrt (float i)) + 1.
        (x |> Array.sumBy f) - (x |> Array.mapi g |> Array.fold (*) 1.)

    let holderTable (WithinBounds -10.0 10.0 x1) (WithinBounds -10.0 10.0 x2) =
        - abs(sin x1 * cos x2 * exp (abs (1. - (sqrt (x1 ** 2. + x2 ** 2.) / pi))))

    let langermann' m (a: float[,]) (c: float[]) x =
        let x = x |> Array.map ((|WithinBounds|) 0. 10.)
        let d = x |> Array.length

        [ 1..m ]
        |> List.sumBy (fun i ->
            let z1 = ([ 1..d ] |> List.sumBy (fun j -> (x.[j - 1] - a.[i - 1, j - 1]) ** 2.))
            c.[i - 1] * exp ((-1. / pi) * z1) * cos (pi * z1))

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

    open Bristlecone.Language

    module PredatorPrey =

        open Bristlecone.Time

        [<Measure>] type prey
        [<Measure>] type predator
        [<Measure>] type km
        [<Measure>] type area = km^2

        // States
        let H = state<prey/area>        "prey"
        let L = state<predator/area>    "predator"

        let predatorPreyBase =

            // Parameters
            let α           = parameter "α" noConstraints 0.75</year> 1.25</year> // Maximum prey per capita growth rate
            let β  = parameter "β" noConstraints 0.01<1/(predator/area * year)> 0.20<1/(predator/area * year)> // Effect of the presence of predators on the prey death rate
            let δ  = parameter "δ" noConstraints 0.75<1/(prey/area * year)> 1.25<1/(prey/area * year)> // Natural death rate of lynx in the absence of food
            let γ           = parameter "γ" noConstraints 0.01</year> 0.20</year> // Efficiency of turning predated hares into lynx

            let ``dH/dt``: ModelExpression<(prey/area)/year>        = P α * This<prey/area> - P β * This<prey/area> * Environment L
            let ``dL/dt``: ModelExpression<(predator/km^2)/year>    = -P γ * This<predator/area> + P δ * Environment H * This<predator/area>

            Model.empty
            |> Model.addRateEquation H ``dH/dt``
            |> Model.addRateEquation L ``dL/dt``
            |> Model.estimateParameter α // Natural growth rate of hares in absence of predation
            |> Model.estimateParameter β // Death rate per encounter of hares due to predation
            |> Model.estimateParameter δ // Natural death rate of lynx in the absence of food
            |> Model.estimateParameter γ // Efficiency of turning predated hares into lynx

            // continuousModel<year> {
            //     equationRate H ``dH/dt``
            //     equationRate L ``dL/dt``
            //     likelihood (Bristlecone.ModelLibrary.Likelihood.sumOfSquares [ "hare"; "lynx" ])
            // }

        let ``predator-prey`` () =
            predatorPreyBase
            |> Model.useLikelihoodFunction (Bristlecone.ModelLibrary.Likelihood.sumOfSquares [ H.Code; L.Code ])
            |> Model.compile

        let ``predator-prey [with noise]`` () =
            predatorPreyBase
            |> Model.estimateParameterOld "ρ" noConstraints -0.500 0.500
            |> Model.estimateParameterOld "σ[x]" notNegative 0.001 0.100
            |> Model.estimateParameterOld "σ[y]" notNegative 0.001 0.100
            |> Model.useLikelihoodFunction (Bristlecone.ModelLibrary.Likelihood.bivariateGaussian H L)
            |> Model.compile

    module SoilCarbon =

        open Bristlecone.Time

        [<Measure>] type carbon   // e.g. g C
        [<Measure>] type area     // e.g. m^2
        [<Measure>] type temp     // °C

        // States
        let Cf = state "Cf"   // fast pool
        let Cs = state "Cs"   // slow pool

        // Environmental input: soil temperature
        let Ts = environment "T"

        // Measurement variables
        let R = measure "Respiration"

        let soilCarbonBase =

            // Parameters
            let kf = parameter "kf" notNegative 0.001</day> 0.1</day>   // base decay rate fast pool
            let ks = parameter "ks" notNegative 0.0001</day> 0.01</day> // base decay rate slow pool
            let Q10 = parameter "Q10" notNegative 1.5 3.0               // temperature sensitivity
            let alpha = parameter "alpha" notNegative 0.1 0.9           // fraction of fast decay to slow pool

            // Temperature modifier (Q10 function relative to 10 °C)
            let tempEffect =
                (P Q10) ** ((Environment Ts - Constant 10.0<temp>) / Constant 10.0<temp>)

            // Rate equations
            let ``dCf/dt`` : ModelExpression<(carbon/area)/day> =
                - (P kf * tempEffect) * This

            let ``dCs/dt`` : ModelExpression<(carbon/area)/day> =
                (P alpha) * (P kf * tempEffect) * Environment Cf
                - (P ks * tempEffect) * This

            // Derived measurement: soil respiration flux            
            let respiration : ModelExpression<(carbon/area)/day> =
                (P kf * tempEffect) * State Cf
                + (P ks * tempEffect) * State Cs

            Model.empty
            |> Model.addRateEquation Cf ``dCf/dt``
            |> Model.addRateEquation Cs ``dCs/dt``
            |> Model.addMeasure R respiration
            |> Model.estimateParameter kf
            |> Model.estimateParameter ks
            |> Model.estimateParameter Q10
            |> Model.estimateParameter alpha

        // Deterministic fit
        let ``soil-carbon`` () =
            soilCarbonBase
            |> Model.useLikelihoodFunction (Bristlecone.ModelLibrary.Likelihood.sumOfSquares [ R.Code ])
            |> Model.compile

        // With Gaussian observation noise
        let ``soil-carbon [with noise]`` () =
            soilCarbonBase
            |> Model.estimateParameterOld "σ" notNegative 0.001 10.0
            |> Model.compile


    module RickerTemperature =

        open Bristlecone.Time

        [<Measure>] type individuals
        [<Measure>] type degC

        // State: population size
        let N = state<individuals> "N"

        // Measures
        let logN = measure "logN"

        // Environmental input: temperature anomaly
        let T = environment "T" // -5.0<degC> 5.0<degC>

        let rickerBase =

            // Parameters
            let r     = parameter "r" noConstraints 0.1 2.0        // intrinsic growth rate
            let K     = parameter "K" notNegative 50.<individuals> 500.<individuals> // carrying capacity
            let beta  = parameter "beta" noConstraints -1.0 1.0    // temperature effect

            // Discrete‑time update:
            // N_{t+1} = N_t * exp(r * (1 - N_t/K) + beta * T_t)
            let update : ModelExpression<individuals> =
                This *
                exp ( P r * (Constant 1.0 - This / P K)
                    + P beta * Environment T )

            let eqLogNM = State N // |> Logarithm

            Model.discrete
            |> Model.addDiscreteEquation N update
            |> Model.addMeasure logN eqLogNM
            |> Model.estimateParameter r
            |> Model.estimateParameter K
            |> Model.estimateParameter beta

        // Deterministic fit
        let ``ricker-temperature`` () =
            rickerBase
            |> Model.useLikelihoodFunction (Bristlecone.ModelLibrary.Likelihood.sumOfSquares [ logN.Code ])
            |> Model.compile

        // With Gaussian observation noise
        let ``ricker-temperature [with noise]`` () =
            rickerBase
            |> Model.estimateParameterOld "σ" notNegative 0.001 10.0
            |> Model.compile



    module LogisticHarvest =

        open Bristlecone.Time

        [<Measure>] type biomass // e.g. tonnes of fish
        [<Measure>] type km
        [<Measure>] type area = km^2

        let B = state "B"

        let logisticHarvestBase =

            // Parameters
            let r = parameter "r" noConstraints 0.1</year> 1.0</year> // intrinsic per‑capita growth rate
            let K = parameter "K" notNegative 50.<biomass/area> 500.<biomass/area> // carrying capacity
            let h = parameter "h" notNegative 0.01<biomass/area/year> 50.<biomass/area/year> // constant harvest rate

            // Logistic growth with harvest: dB/dt = r * B * (1 - B/K) - h
            let ``dB/dt`` : ModelExpression<(biomass/area)/year> =
                P r * This<biomass/area> * (Constant 1.0 - This<biomass/area> / P K)
                - P h

            Model.empty<year>
            |> Model.addRateEquation B``dB/dt``
            |> Model.estimateParameter r
            |> Model.estimateParameter K
            |> Model.estimateParameter h

        // Deterministic fit
        let ``logistic-harvest`` () =
            logisticHarvestBase
            |> Model.useLikelihoodFunction (Bristlecone.ModelLibrary.Likelihood.sumOfSquares [ B.Code ])
            |> Model.compile

        // Stochastic fit with Gaussian observation noise
        let ``logistic-harvest [with noise]`` () =
            logisticHarvestBase
            |> Model.estimateParameterOld "σ" notNegative 0.001 10.0 // observation noise s.d.
            // |> Model.useLikelihoodFunction (Bristlecone.ModelLibrary.Likelihood.gaussian "biomass")
            |> Model.compile


    module PlantSoilMonod =

        open Bristlecone.Time

        // Units
        [<Measure>] type biomass          // e.g., t of plant material
        [<Measure>] type nutrient         // e.g., kg N
        [<Measure>] type km
        [<Measure>] type area = km^2

        // States
        let B = state<biomass/area>  "biomass"   // plant biomass density
        let S = state<nutrient/area> "soilN"     // soil nutrient availability

        // Base model: plant biomass limited by soil nutrient via Monod uptake
        let plantSoilBase =

            // Parameters
            let q   = parameter "q"   notNegative 0.01<nutrient/(biomass*year)>  2.0<nutrient/(biomass*year)> // uptake capacity (nutrient per biomass per time)
            let e   = parameter "e"   notNegative 0.10<biomass/nutrient>          2.0<biomass/nutrient> // conversion efficiency of nutrient to biomass
            let m   = parameter "m"   notNegative 0.01</year>                     1.0</year> // plant mortality/turnover
            let I   = parameter "I"   notNegative 0.001<nutrient/area/year>        100.0<nutrient/area/year> // external nutrient input (deposition, fertiliser)
            let l   = parameter "l"   notNegative 0.01</year>                     2.0</year> // nutrient loss (leaching/mineralisation balance)
            let Ks  = parameter "Ks"  notNegative 1.0<nutrient/area>            100.0<nutrient/area> // half-saturation constant for uptake

            // Monod (Holling type II) limitation: f(S) = S / (Ks + S)  (dimensionless)
            let fS : ModelExpression<1> =
                Environment S / (P Ks + Environment S)

            // Plant biomass dynamics:
            // dB/dt = e * q * f(S) * B  -  m * B
            let ``dB/dt`` : ModelExpression<(biomass/area)/year> =
                P e * P q * fS * This<biomass/area> - P m * This<biomass/area>

            // Soil nutrient dynamics:
            // dS/dt = I  -  q * f(S) * B  -  l * S
            let ``dS/dt`` : ModelExpression<(nutrient/area)/year> =
                P I - P q * fS * This<biomass/area> - P l * This<nutrient/area>

            Model.empty<year>
            |> Model.addRateEquation B ``dB/dt``
            |> Model.addRateEquation S   ``dS/dt``
            |> Model.estimateParameter q
            |> Model.estimateParameter e
            |> Model.estimateParameter m
            |> Model.estimateParameter I
            |> Model.estimateParameter l
            |> Model.estimateParameter Ks

        // Deterministic fit
        let ``plant-soil monod`` () =
            plantSoilBase
            |> Model.useLikelihoodFunction (Bristlecone.ModelLibrary.Likelihood.sumOfSquares [ B.Code; S.Code ])
            |> Model.compile

        // With Gaussian observation noise
        let ``plant-soil monod [with noise]`` () =
            plantSoilBase
            |> Model.estimateParameterOld "ρ" noConstraints -0.500 0.500
            |> Model.estimateParameterOld "σ[x]" notNegative 0.001 0.100
            |> Model.estimateParameterOld "σ[y]" notNegative 0.001 0.100
            |> Model.useLikelihoodFunction (Bristlecone.ModelLibrary.Likelihood.bivariateGaussian B S)
            |> Model.compile


    module Housing =

        open Bristlecone.Time

        // Units
        [<Measure>] type person
        [<Measure>] type dwelling
        [<Measure>] type household

        // Parameters
        let theta = parameter "theta" noConstraints 0.1<dwelling/person> 0.2<dwelling/person>
        let h65_slope = parameter "h65_slope" noConstraints 0.1<(household/person)/year> 0.2<(household/person)/year>
        let r_sec = parameter "r_sec" noConstraints 0.1</year> 0.2</year>
        let r_str = parameter "r_str" noConstraints 0.1</year> 0.2</year>
        let str_sat = parameter "str_sat" noConstraints 0.1 0.2
        let k_reno = parameter "k_reno" noConstraints 0.1</year> 0.2</year>
        let reno_target = parameter "reno_target" noConstraints 0.1 0.2

        // States
        let H = state<household/person> "headship"
        let SH = state<1> "second home share"
        let STR = state<1> "STR?"
        let R = state<1> "renovation"
        let O = state<dwelling> "occupied dwellings"


        // External forcings
        let age65plus: StateId<person> = environment "Age65Plus"
        

        // State equations (ODEs)
        let ``dH/dt`` = P h65_slope * Environment age65plus
        let ``dSH/dt``: ModelExpression</year> = P r_sec + State SH * (Constant 1. - State SH)
        let ``dSTR/dt``: ModelExpression</year> = P r_str + State STR * (P str_sat - State STR)
        let ``dR/dt`` = - P k_reno * (State R - P reno_target)
        let ``dO/dt``: ModelExpression<dwelling/year> =
            P theta * State H * (Constant 1. - State SH)
                * (Constant 1. - State STR) * (Constant 1. - State R)

        
