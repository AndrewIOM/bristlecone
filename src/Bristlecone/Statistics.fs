namespace Bristlecone.Statistics

/// <summary>Statistics returned during n-step ahead analysis</summary>
type NStepStatistics = { RMSE: float }

module Distributions =

    open MathNet.Numerics.LinearAlgebra

    [<RequireQualifiedAccess>]
    module ContinuousUniform =

        let draw<[<Measure>] 'u> random (min: float<'u>) (max: float<'u>) : unit -> float<'u> =
            let min, max  = Bristlecone.Units.removeUnitFromFloat min, Bristlecone.Units.removeUnitFromFloat max
            let distribution =
                MathNet.Numerics.Distributions.ContinuousUniform(min, max, random)

            fun () ->
                distribution.Sample()
                |> LanguagePrimitives.FloatWithMeasure<'u>

    [<RequireQualifiedAccess>]
    module DiscreteUniform =

        let draw<[<Measure>] 'u> random (min: int<'u>) (max: int<'u>) : unit -> int<'u> =
            let min, max  = Bristlecone.Units.removeUnitFromInt min, Bristlecone.Units.removeUnitFromInt max
            let distribution =
                MathNet.Numerics.Distributions.DiscreteUniform(min, max, random)

            fun () ->
                distribution.Sample()
                |> LanguagePrimitives.Int32WithMeasure<'u>

    [<RequireQualifiedAccess>]
    module Normal =

        /// Unit-generic normal draw
        let draw<[<Measure>] 'u>
            (random: System.Random)
            (mean: float<'u>)
            (stdev: float<'u>) : unit -> float<'u> =

            // Strip units for MathNet
            let meanF  = float mean
            let stdevF = float stdev

            let distribution = MathNet.Numerics.Distributions.Normal(meanF, stdevF, random)

            // Return a function that samples and reapplies the unit
            fun () ->
                let sample = distribution.Sample()
                LanguagePrimitives.FloatWithMeasure<'u> sample


    [<RequireQualifiedAccess>]
    module MultivariateNormal =

        // Helpers to strip/apply units for matrices
        let private stripUnitsM (m: Matrix<float<'u>>) : Matrix<float> =
            m.Map float

        let private applyUnitsM<[<Measure>] 'u> (m: Matrix<float>) : Matrix<float<'u>> =
            m.Map LanguagePrimitives.FloatWithMeasure<'u>

        /// Compute factor A such that A * A^T = cov
        let private factorFromCov<[<Measure>] 'u>
            (cov: Matrix<float<'u^2>>) : Matrix<float<'u>> =

            let covF = stripUnitsM cov
            let factorF =
                if covF.Determinant() = 0.0 then
                    let svd = covF.Svd(true)
                    let u, d = svd.U, svd.W
                    let sqrtD = d.Map(fun x -> if x > 0.0 then sqrt x else 0.0)
                    (sqrtD * u.Transpose()).QR().R.Transpose()
                else
                    covF.Cholesky().Factor

            applyUnitsM<'u> factorF

        /// Unit-generic multivariate normal sampler
        let sample<[<Measure>] 'u>
            (cov: Matrix<float<'u^2>>)
            (random: System.Random) : unit -> Vector<float<'u>> =

            let factor = factorFromCov<'u> cov
            let dim = cov.ColumnCount

            fun () ->
                // Use unit-generic Normal.draw to get each component
                let stdDraw = Normal.draw random (LanguagePrimitives.FloatWithMeasure<'u> 0.) (LanguagePrimitives.FloatWithMeasure<'u> 1.)
                let z = Array.init dim (fun _ -> stdDraw()) |> vector
                factor * z


    [<RequireQualifiedAccess>]
    module Cauchy =

        /// Unit-generic Cauchy draw
        let draw<[<Measure>] 'u>
            (random: System.Random)
            (location: float<'u>)
            (scale: float<'u>) : unit -> float<'u> =

            // Strip units for MathNet
            let locF   = float location
            let scaleF = float scale

            let dist = MathNet.Numerics.Distributions.Cauchy(locF, scaleF, random)

            fun () ->
                dist.Sample()
                |> LanguagePrimitives.FloatWithMeasure<'u>


module LinearAlgebra =

        open MathNet.Numerics.LinearAlgebra

        /// Generate a covariance matrix
        let covarianceMatrix parameterCount (scale: float) =
            let m4 = DiagonalMatrix.identity<float> parameterCount
            (m4 * scale) / sqrt (float parameterCount)

        /// Calculates the covariance of a given matrix
        let computeCovariance<[<Measure>] 'u> (matrix: Matrix<float<'u>>) =
            let columnAverages = matrix.ColumnSums() / LanguagePrimitives.FloatWithMeasure<'u> (float matrix.RowCount)

            let centredColumns =
                matrix.EnumerateColumns()
                |> Seq.zip columnAverages
                |> Seq.map (fun (col, avg) -> col - avg)
                |> Seq.toList

            let centred = DenseMatrix.ofColumns centredColumns

            let normalisationFactor =
                float <| if matrix.RowCount = 1 then 1 else matrix.RowCount - 1

            // MathNet does not propagate units correctly during multiply. Manual override.
            let rawCov = centred.TransposeThisAndMultiply(centred)
            let covWithUnits = rawCov.Map(fun x -> LanguagePrimitives.FloatWithMeasure<'u> 1. * x)
            covWithUnits / LanguagePrimitives.FloatWithMeasure<'u^2> normalisationFactor


module Interpolate =

    open Bristlecone.Time

    /// Interpolates between two data points, for a given time `t`.
    let bilinear ((t1: float<``time index``>, v1)) ((t2: float<``time index``>, v2)) t =
        v1 + (t - t1) * ((v2 - v1) / (t2 - t1))

    /// Use the previous point
    let lower ((t1: float<``time index``>, v1)) ((t2: float<``time index``>, v2)) t = v1

    /// Use the next point
    let upper ((t1: float<``time index``>, v1)) ((t2: float<``time index``>, v2)) t = v2


module Regression =

    open MathNet.Numerics.Statistics
    open MathNet.Numerics.Distributions

    /// Returns (slope, p-value) for the slope coefficient in simple linear regression.
    /// p-value is NaN if the data is perfectly flat.
    let slopeAndPValue (x: float[]) (y: float[]) =
        let n = float x.Length
        if n < 3 then nan, nan
        else
            let meanX = Statistics.Mean x
            let meanY = Statistics.Mean y

            let ssX  = Array.sumBy (fun xi -> (xi - meanX) ** 2.0) x
            let ssXY = Array.map2 (fun xi yi -> (xi - meanX) * (yi - meanY)) x y |> Array.sum

            let slope     = ssXY / ssX
            let intercept = meanY - slope * meanX

            let residuals = Array.map2 (fun xi yi -> yi - (slope * xi + intercept)) x y
            let sse       = Array.sumBy (fun r -> r ** 2.0) residuals
            let seSlope   = sqrt (sse / (n - 2.0)) / sqrt ssX

            if seSlope = 0.0 then slope, nan
            else
                let tStat = slope / seSlope
                let tDist = MathNet.Numerics.Distributions.StudentT(0.0, 1.0, n - 2.0)
                let pVal  = 2.0 * (1.0 - tDist.CumulativeDistribution(abs tStat))
                slope, pVal


/// Statistics to determine whether there are trends within series.
module TrendAnalysis =

    open Bristlecone.Time

    /// TODO Finish implementation
    let theilSen (timeDiff: System.TimeSpan -> 'a) (ts: TimeSeries<'a, 'date, 'timeunit, 'timespan>) =
        let allPoints = ts |> TimeSeries.toObservations

        let allSlopes =
            allPoints
            |> Seq.allPairs allPoints // Calculate the product of points
            |> Seq.filter (fun ((_, t1), (_, t2)) -> t1 <> t2) // Sen (1968): remove duplicate ts
            |> Seq.map (fun ((y1, x1), (y2, x2)) -> (y2 - y1) / (timeDiff (x2 - x1))) // Calculate slope (x = t)

        let medianSlope =
            allSlopes
            |> Seq.sortBy id
            |> Seq.skip ((allSlopes |> Seq.length) / 2)
            |> Seq.head

        // Determine line by setting y-intercept b to median of values yi - mxi

        medianSlope

// Median of the slopes determined by all pairs of sample points.

// For every point
// Calculate slope to every other point (only at other times)
//

//Take the median of the slopes defined from pairs of points that have distinct x coordinates.
//x =

/// Statistics for determining the root of non-linear equations.
module RootFinding =

    /// Secant method for finding root of non-linear equations. This method is faster than bisection, but may not converge on a root.
    let rec secant n N f x0 x1 x2 : float =
        if n >= N then
            x0
        else
            let x = x1 - (f (x1)) * ((x1 - x0) / (f (x1) - f (x0)))
            secant (n + 1) N f x x0 x2

    /// Bisect method for finding root of non-linear equations.
    let rec bisect n N f a b t : float =
        if n >= N then
            nan
        else
            let c = (a + b) / 2.

            if (f c) = 0.0 || (b - a) / 2. < t then
                c
            else if sign (f c) = sign (f a) then
                bisect (n + 1) N f c b t
            else
                bisect (n + 1) N f a c t

    module Tensor =

        open DiffSharp

        /// Bisect method for finding root of non-linear equations.
        let bisect
            (f: Tensor -> Tensor)
            (target: Tensor) (lo: Tensor) (hi: Tensor)
            (tol: float) (maxIter: int) : Tensor =
            
            let tolT = dsharp.tensor(tol, dtype = Float64)
            
            let rec loop (a: Tensor) (b: Tensor) i =
                let c = (a + b) * dsharp.tensor(0.5, dtype = Float64) // Midpoint
                let fc = f c - target
                
                // 1.0 if either condition is true:
                let stopMask =
                    dsharp.lt(dsharp.abs fc, tolT) + // Is midpoint close enough to root?
                    dsharp.lt((b - a) * dsharp.tensor 0.5, tolT) // Is interval small enough?
                
                if i >= maxIter then c
                else
                    let fa   = f a - target // left side of bracket
                    let prod = fa * fc

                    let mask = dsharp.cast(dsharp.gt(prod, dsharp.tensor(0.0, dtype = Float64)), a.dtype)
                    let invMask = 1.0 - mask

                    let stopF = dsharp.cast(stopMask, a.dtype)
                    let contF = 1.0 - stopF
                    let a' = a * stopF + (a * invMask + c * mask) * contF
                    let b' = b * stopF + (b * mask    + c * invMask) * contF

                    loop a' b' (i+1)
            loop lo hi 0


/// Statistics to measure the convergence of multiple trajectories,
/// for example for chains in a Monte Carlo analysis.
module Convergence =

    module GelmanRubin =

        let internal w m sj2s = (1. / m) * (Seq.sum sj2s)

        let internal b n m overallMean chainMeans =
            (n / (m - 1.)) * (chainMeans |> Seq.sumBy (fun xm -> (xm - overallMean) ** 2.))

        let internal sjSquared chainMean chainValues =
            let n = chainValues |> Seq.length |> float

            (1. / (n - 1.))
            * (chainValues |> Seq.sumBy (fun thetai -> (thetai - chainMean) ** 2.))

        let internal varianceHat W B n = (1. - (1. / n)) * W + (1. / n) * B

        let internal rHat' varianceHat w = sqrt (varianceHat / w)

        /// R-hat tends downwards to one as convergence increases. It is often
        /// accepted that a value below 1.1 indicates convergence for chains
        /// within a Monte Carlo Markov Chain analysis.
        let rHat (chains: float seq seq) =
            if chains |> Seq.map Seq.length |> Seq.distinct |> Seq.length > 1 then
                failwith "Chains were different lengths"

            let overallMean = chains |> Seq.concat |> Seq.average // Mean for all chains when combined
            let chainMeans = chains |> Seq.map Seq.average // Per-chain mean
            let m = chains |> Seq.length |> float // Number of chains
            let n = chains |> Seq.head |> Seq.length |> float // Iterations per chain
            let b = b n m overallMean chainMeans

            let sSquared =
                chainMeans
                |> Seq.zip chains
                |> Seq.map (fun (history, mean) -> sjSquared mean history)

            let w = w m sSquared
            let varHat = varianceHat w b n
            rHat' varHat w
