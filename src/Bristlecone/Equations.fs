namespace Bristlecone.ModelLibrary

/// <summary>Likelihood functions to represent a variety of distributions and data types.</summary>
///
/// <namespacedoc>
///   <summary>Pre-built model parts for use in Bristlecone</summary>
/// </namespacedoc>
module Likelihood =

    open Bristlecone
    open Bristlecone.ModelSystem
    open Bristlecone.Tensors

    let internal getData key (pairs: CodedMap<SeriesPair<'u>>) =
        match pairs |> Map.tryFindBy (fun k -> k.Value = key) with
        | Some p -> p
        | None -> failwithf "Predicted data was required for the variable '%s' but did not exist." key

    /// Residual sum of squares. Provides a simple metric of distance between
    /// observed data and model predictions.
    let sumOfSquares (keys: ShortCode.ShortCode list) : Likelihood<'u> =
        fun _ data ->
            keys
            |> List.map (fun k ->
                let d = data |> getData k.Value
                let obs = Typed.tail d.Observed
                let exp = Typed.tail d.Expected // I don't think my original did tail?
                let diff = obs - exp
                Typed.squareVector diff
            )
            |> List.reduce ( + )
            |> Typed.sumVector
            |> Typed.retype

    /// Negative log likelihood for a bivariate normal distribution.
    /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
    let internal gaussianVec
        (sigmax: TypedTensor<Scalar,'u>)
        (obsx:   TypedTensor<Vector,'u>)
        (expx:   TypedTensor<Vector,'u>)
        : TypedTensor<Vector,``-logL``> =

        let half  = Typed.ofScalar 0.5
        let twoPi = Typed.ofScalar (2.0 * System.Math.PI)

        let diffx = obsx - expx
        let term1Scalar = half * Typed.logScalar twoPi
        let term2Scalar = Typed.logScalar sigmax

        let n = Typed.length obsx
        let term1 = Typed.broadcastScalarToVector term1Scalar n
        let term2 = Typed.broadcastScalarToVector term2Scalar n
        let term3 = half * Typed.squareVector (diffx / sigmax)

        term1 + term2 + term3
        |> Typed.retype

    /// <summary>
    /// Log likelihood function for single equation system, assuming Gaussian error for x.
    /// Requires a parameter 'σ[x]' to be included in any `ModelSystem` that uses it.
    /// </summary>
    let gaussian key : Likelihood<parameter> =
        fun paramAccessor data ->
            let x = data |> getData key
            let sigmax = paramAccessor.Get "σ[x]"

            let obsx = Typed.tail x.Observed
            let expx = Typed.tail x.Expected

            gaussianVec sigmax obsx expx
            |> Tensors.Typed.sumVector

    /// Negative log likelihood for a bivariate normal distribution.
    /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
    let internal bivariateGaussianVec
        (sigmax: TypedTensor<Scalar,'u>)
        (sigmay: TypedTensor<Scalar,'u>)
        (rho:    TypedTensor<Scalar,1>)
        (obsx:   TypedTensor<Vector,'u>)
        (obsy:   TypedTensor<Vector,'u>)
        (expx:   TypedTensor<Vector,'u>)
        (expy:   TypedTensor<Vector,'u>)
        : TypedTensor<Vector,``-logL``> =

        let one  = Typed.ofScalar 1.0
        let two  = Typed.ofScalar 2.0
        let half = Typed.ofScalar 0.5
        let piT  = Typed.ofScalar System.Math.PI

        let diffx = obsx - expx
        let diffy = obsy - expy
        let zta1  = Typed.squareVector (diffx / sigmax)
        let zta2  = two * rho * (diffx / sigmax) * (diffy / sigmay)
        let zta3  = Typed.squareVector (diffy / sigmay)

        let vNegLog = two * piT * sigmax * sigmay * Typed.sqrtScalar (one - Typed.square rho)
        let vNegLogVec = Typed.broadcastScalarToVector vNegLog (Typed.length obsx) |> Typed.retype
        let q = (one / (one - Typed.square rho)) * (zta1 - zta2 + zta3 : TypedTensor<Vector,1>)

        (vNegLogVec + half * q) |> Typed.retype

    /// <summary>
    /// Log likelihood function for dual simultaneous system, assuming Gaussian error for both x and y.
    /// Requires parameters 'σ[x]', 'σ[y]' and 'ρ' to be included in any `ModelSystem` that uses it.
    /// </summary>
    let bivariateGaussian (key1: Language.StateId<_>) (key2:Language.StateId<_>) : Likelihood<'u> =
        fun paramAccessor data ->
            let x = data |> getData key1.Code.Value
            let y = data |> getData key2.Code.Value

            let sigmax = paramAccessor.Get "σ[x]" |> Typed.retype<parameter,'u,Scalar>
            let sigmay = paramAccessor.Get "σ[y]" |> Typed.retype<parameter,'u,Scalar>
            let rho    = paramAccessor.Get "ρ" |> Typed.retype<parameter,1,Scalar>

            let obsx = Typed.tail x.Observed
            let obsy = Typed.tail y.Observed
            let expx = Typed.tail x.Expected
            let expy = Typed.tail y.Expected

            bivariateGaussianVec sigmax sigmay rho obsx obsy expx expy
            |> Typed.sumVector

    // let internal poissonVec
    //     (lambda: TypedTensor<Vector,'count>)
    //     (y:      TypedTensor<Vector,'count>)
    //     : TypedTensor<Vector,``-logL``> =

    //     let term1 = y * Typed.logVector lambda
    //     let term2 = -lambda
    //     let term3 = -Typed.logFactorialVector y
    //     (term1 + term2 + term3) |> Typed.retype

    // /// Negative log likelihood for a Poisson distribution.
    // /// Requires expected values λ to be in the data's Expected field.
    // let poisson key : Likelihood<'count> =
    //     fun _ data ->
    //         let d = data |> getData key
    //         let lambda = Typed.tail d.Expected
    //         let y      = Typed.tail d.Observed
    //         poissonVec lambda y
    //         |> Typed.sumVector

    // /// Negative log likelihood for a Negative Binomial distribution.
    // /// Requires dispersion parameter k in the parameter set.
    // let internal negBinomialVec
    //     (mu: TypedTensor<Vector,'count>)
    //     (k:  TypedTensor<Scalar,'count>)
    //     (y:  TypedTensor<Vector,'count>)
    //     : TypedTensor<Vector,``-logL``> =

    //     let one = Typed.ofScalar 1.0<'count>
    //     let kVec = Typed.broadcastScalarToVector k (Typed.length y)
    //     let term1 = Typed.logGammaVector (y + kVec)
    //     let term2 = -Typed.logGammaVector kVec
    //     let term3 = -Typed.logGammaVector (y + one)
    //     let term4 = kVec * Typed.logVector (kVec / (kVec + mu))
    //     let term5 = y * Typed.logVector (mu / (kVec + mu))
    //     (term1 + term2 + term3 + term4 + term5) |> Typed.retype

    // let negBinomial key : Likelihood<'count> =
    //     fun paramAccessor data ->
    //         let d = data |> getData key
    //         let mu = Typed.tail d.Expected
    //         let y  = Typed.tail d.Observed
    //         let k  = paramAccessor.Get "k"
    //         negBinomialVec mu k y
    //         |> Typed.sumVector

    // /// Negative log likelihood for a Log-Normal distribution.
    // /// Requires σ in the parameter set.
    // let internal logNormalVec
    //     (sigma: TypedTensor<Scalar,'u>)
    //     (obs:   TypedTensor<Vector,'u>)
    //     (exp:   TypedTensor<Vector,'u>)
    //     : TypedTensor<Vector,``-logL``> =

    //     let half  = Typed.ofScalar 0.5
    //     let twoPi = Typed.ofScalar (2.0 * System.Math.PI)
    //     let logObs = Typed.logVector obs
    //     let logExp = Typed.logVector exp
    //     let diff   = logObs - logExp
    //     let term1  = half * Typed.logScalar twoPi
    //     let term2  = Typed.logScalar sigma
    //     let term3  = half * Typed.squareVector (diff / sigma)

    //     let n = Typed.length obs
    //     let term1v = Typed.broadcastScalarToVector term1 n
    //     let term2v = Typed.broadcastScalarToVector term2 n

    //     (term1v + term2v + term3) |> Typed.retype

    // let logNormal key : Likelihood<'u> =
    //     fun paramAccessor data ->
    //         let d = data |> getData key
    //         let sigma = paramAccessor.Get "σ"
    //         let obs = Typed.tail d.Observed
    //         let exp = Typed.tail d.Expected
    //         logNormalVec sigma obs exp
    //         |> Typed.sumVector

    // /// Negative log likelihood for a Gamma distribution.
    // /// Requires shape α and rate β in the parameter set.
    // let internal gammaVec
    //     (alpha: TypedTensor<Scalar,dimensionless>)
    //     (beta:  TypedTensor<Scalar,'u^-1>)
    //     (obs:   TypedTensor<Vector,'u>)
    //     : TypedTensor<Vector,``-logL``> =

    //     let term1 = alpha * Typed.logScalar beta
    //     let term2 = -Typed.logGammaScalar alpha
    //     let term3 = (alpha - Typed.ofScalar 1.0<dimensionless>) * Typed.logVector obs
    //     let term4 = -(beta * obs)

    //     let n = Typed.length obs
    //     let term1v = Typed.broadcastScalarToVector term1 n
    //     let term2v = Typed.broadcastScalarToVector term2 n

    //     (term1v + term2v + term3 + term4) |> Typed.retype

    // let gamma key : Likelihood<'u> =
    //     fun paramAccessor data ->
    //         let d = data |> getData key
    //         let alpha = paramAccessor.Get "α"
    //         let beta  = paramAccessor.Get "β"
    //         let obs   = Typed.tail d.Observed
    //         gammaVec alpha beta obs
    //         |> Typed.sumVector

    // /// Zero-Inflated Poisson.
    // /// Requires zero-inflation probability π in the parameter set.
    // let internal zipVec
    //     (pi0:   TypedTensor<Scalar,1>)
    //     (lambda:TypedTensor<Vector,'count>)
    //     (y:     TypedTensor<Vector,'count>)
    //     : TypedTensor<Vector,``-logL``> =

    //     let one = Typed.ofScalar 1.0
    //     let isZeroMask =
    //         y
    //         |> Typed.mapVector (fun yi -> if yi = 0.0<_> then 1.0<dimensionless> else 0.0<dimensionless>)

    //     let logProbZero =
    //         Typed.logVector (pi0 + (one - pi0) * Typed.expVector (-lambda))

    //     let logProbNonZero =
    //         Typed.logScalar (one - pi0)
    //         + (y * Typed.logVector lambda)
    //         - lambda
    //         - Typed.logGammaVector (y + Typed.ofScalar 1.0<'count>)

    //     (isZeroMask * logProbZero) + ((one - isZeroMask) * logProbNonZero)
    //     |> Typed.retype

    // let zip key : Likelihood<'count> =
    //     fun paramAccessor data ->
    //         let d = data |> getData key
    //         let pi0    = paramAccessor.Get "π"
    //         let lambda = Typed.tail d.Expected
    //         let y      = Typed.tail d.Observed
    //         zipVec pi0 lambda y
    //         |> Typed.sumVector

    // /// Zero-Inflated Negative Binomial.
    // /// Requires zero-inflation probability π and dispersion k.
    // let internal zinbVec
    //     (pi0: TypedTensor<Scalar,1>)
    //     (mu:  TypedTensor<Vector,'count>)
    //     (k:   TypedTensor<Scalar,'count>)
    //     (y:   TypedTensor<Vector,'count>)
    //     : TypedTensor<Vector,``-logL``> =

    //     let one = Typed.ofScalar 1.0
    //     let kVec = Typed.broadcastScalarToVector k (Typed.length y)

    //     let isZeroMask =
    //         y
    //         |> Typed.mapVector (fun yi -> if yi = 0.0<_> then 1.0 else 0.0)

    //     let logProbZero =
    //         Typed.logVector (pi0 + (one - pi0) * ((kVec / (kVec + mu)) ** k))

    //     let logProbNonZero =
    //         Typed.logScalar (one - pi0)
    //         + Typed.logGammaVector (y + kVec)
    //         - Typed.logGammaVector kVec
    //         - Typed.logGammaVector (y + Typed.ofScalar 1.0<'count>)
    //         + (kVec * Typed.logVector (kVec / (kVec + mu)))
    //         + (y * Typed.logVector (mu / (kVec + mu)))

    //     (isZeroMask * logProbZero) + ((one - isZeroMask) * logProbNonZero)
    //     |> Typed.retype

    // let zinb key : Likelihood<'count> =
    //     fun paramAccessor data ->
    //         let d = data |> getData key
    //         let pi0 = paramAccessor.Get "π"
    //         let mu  = Typed.tail d.Expected
    //         let y   = Typed.tail d.Observed
    //         let k   = paramAccessor.Get "k"
    //         zinbVec pi0 mu k y
    //         |> Typed.sumVector

