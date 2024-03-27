
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
        let exp1 = -b * sqrt ((1./d) * (x |> Array.sumBy(fun x -> x ** 2.)))
        let exp2 = (1./d) * (x |> Array.sumBy(fun x -> cos(c * x)))
        -a * exp exp1 - exp exp2 + a + exp 1.

    let ackley x =
        ackley' 20. 0.2 (2. * pi) (x |> Array.map((|WithinBounds|) -32.768 32.768))

    let bukinSixth (WithinBounds -15.0 5.0 x1) (WithinBounds -3.0 3.0 x2) =
        100. * sqrt(abs(x2 - 0.01 * x1 ** 2.)) + 0.01 * abs(x1 + 10.)

    let crossInTray (WithinBounds -10.0 10.0 x1) (WithinBounds -10.0 10.0 x2) =
        -0.0001 * (abs(sin(x1) * sin(x2) * exp(abs(100. - sqrt(x1 ** 2. + x2 ** 2.) / pi))) + 1.) ** 0.1

    let dropWave (WithinBounds -5.12 5.12 x1) (WithinBounds -5.12 5.12 x2) =
        (1. + cos(12. * sqrt(x1 ** 2. + x2 ** 2.))) / (0.5 * (x1 ** 2. + x2 ** 2.) + 2.)

    let eggHolder (WithinBounds -512.0 512.0 x1) (WithinBounds -512.0 512.0 x2) =
        -(x2 + 47.) * sin (sqrt(abs(x2 + (x1 / 2.) + 47.))) - x1 * sin(sqrt(abs(x1 - (x2 + 47.))))

    let gramacyLee (WithinBounds 0.5 2.5 x) =
        (sin(10. * pi * x) / 2. * pi) + (x - 1.) ** 4.

    let griewank x =
        let x = x |> Array.map((|WithinBounds|) -600. 600.)
        let f x = (x ** 2. / 4000.)
        let g i x = (x / sqrt(float i)) + 1.
        (x |> Array.sumBy f) - (x |> Array.mapi g |> Array.fold (*) 1.)

    let holderTable (WithinBounds -10.0 10.0 x1) (WithinBounds -10.0 10.0 x2) =
        - abs(sin x1 * cos x2 * exp(abs(1. - (sqrt(x1 ** 2. + x2 ** 2.) / pi))))

    let langermann' m (a:float[,]) (c:float[]) x =
        let x = x |> Array.map((|WithinBounds|) 0. 10.)
        let d = x |> Array.length
        [ 1 .. m ] |> List.sumBy(fun i ->
            let z1 = ([ 1 .. d ] |> List.sumBy(fun j -> (x.[j-1] - a.[i-1,j-1]) ** 2.))
            c.[i-1] * exp((-1./pi) * z1) * cos(pi * z1))

    /// 2D Langermann function using recommended values of A and c
    let langermann x1 x2 =
        let a = 
            [|[|3.; 5.|]
              [|5.; 2.|]
              [|2.; 1.|]
              [|1.; 4.|]
              [|7.; 9.|]|] |> array2D
        let c = [|1.; 2.; 5.; 2.; 3.|]
        langermann' 5 a c [|x1; x2|]
    
    let rastrigin x =
        let x = x |> Array.map((|WithinBounds|) -5.12 5.12)
        let a = 10.
        let z = x |> Array.sumBy(fun x -> x ** 2. - a * cos(2. * pi * x))
        a * float x.Length + z

module Timeseries =

    open Bristlecone.Language

    let predatorPreyBase =

        let ``dh/dt`` = Parameter "α" * This - Parameter "β" * This * Environment "lynx"
        let ``dl/dt`` = - Parameter "γ" * This + Parameter "Δ" * Environment "hare" * This

        Model.empty
        |> Model.addEquation       "hare"   ``dh/dt``
        |> Model.addEquation       "lynx"   ``dl/dt``
        |> Model.estimateParameter "α"      noConstraints 0.10 0.60    // Natural growth rate of hares in absence of predation
        |> Model.estimateParameter "β"      noConstraints 0.10 0.60    // Death rate per encounter of hares due to predation
        |> Model.estimateParameter "Δ"      noConstraints 0.10 0.60    // Efficiency of turning predated hares into lynx
        |> Model.estimateParameter "γ"      noConstraints 0.10 0.60    // Natural death rate of lynx in the absence of food

    let ``predator-prey`` =
        predatorPreyBase
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ "hare"; "lynx" ])
        |> Model.compile

    let ``predator-prey [with noise]`` =
        predatorPreyBase
        |> Model.estimateParameter      "σ[x]"  notNegative 0.01 0.5
        |> Model.estimateParameter      "σ[y]"  notNegative 0.01 0.5
        |> Model.estimateParameter      "ρ"     noConstraints -0.5 0.5
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.bivariateGaussian "hare" "lynx")
        |> Model.compile
