namespace Bristlecone.Statistics

module Distributions =

    open MathNet.Numerics.LinearAlgebra

    module ContinuousUniform =

        let draw random min max =
            let distribution = MathNet.Numerics.Distributions.ContinuousUniform(min,max,random)
            fun () -> distribution.Sample()

    module Normal =

        let draw random mean stdev =
            let distribution = MathNet.Numerics.Distributions.Normal(mean,stdev,random)
            fun () -> distribution.Sample()


    module MutlivariateNormal =

        let mapply (m:Matrix<float>) f = 
            m.EnumerateIndexed() |> Seq.iter(fun (i,j,v) -> m.[i,j] <- f v ); m

        // See: https://stackoverflow.com/questions/11277146/multivariate-normal-distribution-with-math-net
        let sample' (rnd:System.Random) = 
            fun () ->
                let rec randomNormal () = 
                    let u1, u2 = rnd.NextDouble(),rnd.NextDouble()
                    let r = sqrt (-2. * (log u1))
                    let theta = 2. * System.Math.PI * u2  
                    seq { 
                        yield r * sin theta
                        yield r * cos theta 
                        yield! randomNormal() }
                randomNormal ()

        let sample (cov:Matrix<float>) rnd = 
            let R = 
                if cov.Determinant() = 0. 
                then
                    let t = cov.Svd true
                    let u,d = t.U, t.W
                    let A = (mapply d sqrt) * u.Transpose()
                    let qr = A.QR() in qr.R.Transpose()
                else cov.Cholesky().Factor
            fun () -> 
                let v = vector ( sample' rnd () |> Seq.take cov.ColumnCount |> List.ofSeq )
                R * v


module Regression =

    open Accord.Statistics.Analysis

    let pValueForLinearSlopeCoefficient (x:float[]) y =
        try
            let x' = x |> Array.map (fun a -> [|a|])
            let mlr = MultipleLinearRegressionAnalysis(true)
            let _ = mlr.Learn(x',y)
            (mlr.Coefficients |> Seq.head).TTest.PValue
        with | _ -> nan


module RootFinding =

    /// Secant method for finding root of non-linear equations. This method is faster than bisection, but may not converge on a root.
    let rec secant n N f x0 x1 x2 : float =
        if n >= N then x0
        else
            let x = x1 - (f(x1))*((x1 - x0)/(f(x1) - f(x0)))
            secant (n + 1) N f x x0 x2

    /// Bisect method for finding root of non-linear equations. A "strong and stable" algorithm.
    let rec bisect n N f a b t : float =
        if n >= N then nan
        else
            let c = (a + b) / 2.
            if (f c) = 0.0 || (b - a) / 2. < t 
                then c
                else 
                    if sign(f c) = sign (f a)
                    then bisect (n + 1) N f c b t
                    else bisect (n + 1) N f a c t


module Convergence =

    module GelmanRubin =

        let w m sj2s = 
            (1. / m) * (Seq.sum sj2s)

        let b n m overallMean chainMeans =
            (n / (m - 1.)) * (chainMeans |> Seq.sumBy (fun xm -> (xm - overallMean) ** 2.))

        let sjSquared chainMean chainValues =
            let n = chainValues |> Seq.length |> float
            (1. / (n - 1.)) * (chainValues |> Seq.sumBy(fun thetai -> (thetai - chainMean) ** 2. ))

        let varianceHat W B n =
            (1. - (1. / n)) * W + (1. / n) * B

        let rHat' varianceHat w =
            sqrt (varianceHat / w)

        let rHat (chains:float seq seq) =

            if chains |> Seq.map Seq.length |> Seq.distinct |> Seq.length > 1 
            then failwith "Chains were different lengths"

            let overallMean = chains |> Seq.concat |> Seq.average   // Mean for all chains when combined
            let chainMeans = chains |> Seq.map Seq.average          // Per-chain mean
            let m = chains |> Seq.length |> float                   // Number of chains
            let n = chains |> Seq.head |> Seq.length |> float       // Iterations per chain

            let b = b n m overallMean chainMeans
            let sSquared = chainMeans |> Seq.zip chains |> Seq.map(fun (history,mean) -> sjSquared mean history)
            let w = w m sSquared
            let varHat = varianceHat w b n
            rHat' varHat w