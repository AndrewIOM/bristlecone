module ModelLibrary

module Likelihood =

    open Types
    open Types.ParameterEstimation

    let sumOfSquares' exp obs =
        Array.zip obs exp
        |> Array.sumBy (fun d -> ((fst d) - (snd d)) ** 2.)

    let private getData s (predictions:CodedMap<PredictedSeries>) = predictions.Item (ShortCode.create s)

    let sumOfSquares keys _ (data:CodedMap<PredictedSeries>) =
        keys
        |> List.sumBy (fun k -> 
            let d = data |> getData k
            sumOfSquares' d.Expected d.Observed )

    let bivariateGaussian' (p:ParameterPool) obsx obsy expx expy = 
        let diffx = obsx - expx
        let diffy = obsy - expy
        let sigmax = p |> Pool.getEstimate "sigmax"
        let sigmay = p |> Pool.getEstimate "sigmay"
        let rho = p |> Pool.getEstimate "rho"
        let zta1 = diffx ** 2. / sigmax ** 2.
        let zta2 = 2. * rho * ((diffx * diffy) / sigmax * sigmay)
        let zta3 = diffy ** 2. / sigmay ** 2.
        let z = zta1 - zta2 + zta3
        -log((1./(2.*System.Math.PI*sigmax*sigmay*sqrt(1.-(rho*rho)))) * exp (-z / (2. * (1. - (rho * rho)))))

    /// <summary> 
    /// Log likelihood function for dual simultaneous system, assuming Gaussian error for both x and y.
    /// </summary> 
    let bivariateGaussian key1 key2 p data = 
        let x = data |> getData key1
        let y = data |> getData key2
        [1 .. (Array.length x.Observed) - 1] 
        |> List.sumBy (fun i -> (bivariateGaussian' p x.Observed.[i] y.Observed.[i] x.Expected.[i] y.Expected.[i])) 
