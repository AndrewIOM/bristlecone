namespace Bristlecone.ModelLibrary

/// <summary>Likelihood functions to represent a variety of distributions and data types.</summary>
///
/// <namespacedoc>
///   <summary>Pre-built model parts for use in Bristlecone</summary>
/// </namespacedoc>
module Likelihood =

    open Bristlecone
    open Bristlecone.ModelSystem

    let private pi = System.Math.PI

    let private getData s (predictions: CodedMap<PredictedSeries>) =
        match predictions |> Map.tryFindBy (fun k -> k.Value = s) with
        | Some p -> p
        | None -> failwithf "Predicted data was required for the variable '%s' but did not exist." s

    let internal sumOfSquares' exp obs =
        Array.zip obs exp |> Array.sumBy (fun d -> ((fst d) - (snd d)) ** 2.)

    /// Residual sum of squares. Provides a simple metric of distance between
    /// observed data and model predictions.
    let sumOfSquares keys : LikelihoodFn =
        fun _ (data: CodedMap<PredictedSeries>) ->
            keys
            |> List.sumBy (fun k ->
                let d = data |> getData k
                sumOfSquares' d.Expected d.Observed)

    /// Negative log likelihood for a bivariate normal distribution.
    /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
    let internal gaussian' sigmax obsx expx =
        let diffx = obsx - expx
        -0.5 * (log (2. * pi) + log (sigmax) + diffx ** 2.0 / sigmax)

    /// <summary>
    /// Log likelihood function for single equation system, assuming Gaussian error for x.
    /// Requires a parameter 'σ[x]' to be included in any `ModelSystem` that uses it.
    /// </summary>
    let gaussian key : LikelihoodFn =
        fun paramAccessor data ->
            let x = data |> getData key
            let sigmax = paramAccessor.Get "σ[x]"

            [ 1 .. (Array.length x.Observed) - 1 ]
            |> List.sumBy (fun i -> (gaussian' sigmax x.Observed.[i] x.Expected.[i]))

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
        vNegLog + (1. / 2.) * q

    /// <summary>
    /// Log likelihood function for dual simultaneous system, assuming Gaussian error for both x and y.
    /// Requires parameters 'σ[x]', 'σ[y]' and 'ρ' to be included in any `ModelSystem` that uses it.
    /// </summary>
    let bivariateGaussian key1 key2 : LikelihoodFn =
        fun paramAccessor data ->
            let x = data |> getData key1
            let y = data |> getData key2
            let sigmax = paramAccessor.Get "σ[x]"
            let sigmay = paramAccessor.Get "σ[y]"
            let rho = paramAccessor.Get "ρ"

            [ 1 .. (Array.length x.Observed) - 1 ]
            |> List.sumBy (fun i ->
                (bivariateGaussian' sigmax sigmay rho x.Observed.[i] y.Observed.[i] x.Expected.[i] y.Expected.[i]))
