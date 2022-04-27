/// Pre-built model parts for use in Bristlecone.
module ModelLibrary

/// Likelihood functions to represent a variety of distributions
/// and data types.
module Likelihood =

    open Bristlecone
    open Bristlecone.ModelSystem

    let private pi = System.Math.PI
    let private getData s (predictions:CodedMap<PredictedSeries>) =
        match predictions |> Map.tryFindBy (fun k -> k.Value = s) with
        | Some p -> p
        | None -> failwithf "Predicted data was required for the variable '%s' but did not exist." s

    let internal sumOfSquares' exp obs =
        Array.zip obs exp
        |> Array.sumBy (fun d -> ((fst d) - (snd d)) ** 2.)

    let sumOfSquares keys _ (data:CodedMap<PredictedSeries>) =
        keys
        |> List.sumBy (fun k -> 
            let d = data |> getData k
            sumOfSquares' d.Expected d.Observed )

    /// Negative log likelihood for a bivariate normal distribution.
    /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
    let internal gaussian' sigmax obsx expx = 
        let diffx = obsx - expx
        -0.5 * (log(2. * pi) + log(sigmax) + diffx ** 2.0 / sigmax)

    /// <summary>
    /// Log likelihood function for single equation system, assuming Gaussian error for x.
    /// </summary> 
    let gaussian key pool data = 
        let x = data |> getData key
        let sigmax = pool |> Parameter.Pool.tryGetTransformedValue "σ[x]" |> Option.get
        [1 .. (Array.length x.Observed) - 1] 
        |> List.sumBy (fun i -> (gaussian' sigmax x.Observed.[i]  x.Expected.[i]))

    /// Negative log likelihood for a bivariate normal distribution.
    /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
    let internal bivariateGaussian' sigmax sigmay rho obsx obsy expx expy = 
        let diffx = obsx - expx
        let diffy = obsy - expy
        let zta1 = (diffx / sigmax) ** 2.
        let zta2 = 2. * rho * ((diffx / sigmax) ** 1.) * ((diffy / sigmay) ** 1.)
        let zta3 = (diffy / sigmay) ** 2.
        let vNegLog = 2. * pi * sigmax * sigmay * sqrt (1. - rho ** 2.)
        let q = (1. / (1. - rho ** 2.)) * (zta1 - zta2 + zta3)
        vNegLog + (1./2.) * q

    /// <summary>
    /// Log likelihood function for dual simultaneous system, assuming Gaussian error for both x and y.
    /// </summary> 
    let bivariateGaussian key1 key2 pool data = 
        let x = data |> getData key1
        let y = data |> getData key2
        let sigmax = pool |> Parameter.Pool.tryGetTransformedValue "σ[x]" |> Option.get
        let sigmay = pool |> Parameter.Pool.tryGetTransformedValue "σ[y]" |> Option.get
        let rho = pool |> Parameter.Pool.tryGetTransformedValue "ρ" |> Option.get
        [1 .. (Array.length x.Observed) - 1] 
        |> List.sumBy (fun i -> (bivariateGaussian' sigmax sigmay rho x.Observed.[i] y.Observed.[i] x.Expected.[i] y.Expected.[i])) 

    // How to refactor likelihood functions?
    // - Need to access parameters?

    type L = Parameter.Pool -> CodedMap<PredictedSeries> -> float

