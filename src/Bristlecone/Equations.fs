module ModelLibrary

module Likelihood =

    open Bristlecone
    open Bristlecone.ModelSystem
    let private pi = System.Math.PI

    let private getData s (predictions:CodedMap<PredictedSeries>) = predictions.Item (ShortCode.create s)

    let sumOfSquares' exp obs =
        Array.zip obs exp
        |> Array.sumBy (fun d -> ((fst d) - (snd d)) ** 2.)

    let sumOfSquares keys _ (data:CodedMap<PredictedSeries>) =
        keys
        |> List.sumBy (fun k -> 
            let d = data |> getData k
            sumOfSquares' d.Expected d.Observed )

    /// Negative log likelihood for a bivariate normal distribution.
    /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
    let bivariateGaussian' (p:ParameterPool) obsx obsy expx expy = 
        let diffx = obsx - expx
        let diffy = obsy - expy
        let sigmax = p |> Pool.getEstimate "sigma[x]"
        let sigmay = p |> Pool.getEstimate "sigma[y]"
        let rho = p |> Pool.getEstimate "rho"
        let zta1 = (diffx / sigmax) ** 2.
        let zta2 = 2. * rho * ((diffx / sigmax) ** 1.) * ((diffy / sigmay) ** 1.)
        let zta3 = (diffy / sigmay) ** 2.
        let vNegLog = 2. * pi * sigmax * sigmay * sqrt (1. - rho ** 2.)
        let q = (1. / (1. - rho ** 2.)) * (zta1 - zta2 + zta3)
        vNegLog + (1./2.) * q

    /// <summary>
    /// Log likelihood function for dual simultaneous system, assuming Gaussian error for both x and y.
    /// </summary> 
    let bivariateGaussian key1 key2 p data = 
        let x = data |> getData key1
        let y = data |> getData key2
        [1 .. (Array.length x.Observed) - 1] 
        |> List.sumBy (fun i -> (bivariateGaussian' p x.Observed.[i] y.Observed.[i] x.Expected.[i] y.Expected.[i])) 
