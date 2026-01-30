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

    /// <summary>Functions representing how variance is handled
    /// within likelihood functions.</summary>
    module Variance =

        /// A variance function maps expected values to per-point σ
        type VarianceFunction<[<Measure>] 'u, [<Measure>] 'v> =
            TypedTensor<Vector,'v> -> TypedTensor<Vector,'u>

        /// Variance is constant within through time.
        let constant sigma : VarianceFunction<'u, 'v> =
            fun expected -> Typed.broadcastScalarToVector sigma (Typed.length expected)

        /// Variance is proportional to the expected value (σ = σ0 * x).
        let proportional (sigma0: TypedTensor<Scalar,'u/'v>) : VarianceFunction<'u, 'v> =
            fun (expected: TypedTensor<Vector,'v>) -> expected * sigma0

        /// Variance is exponential to the expected value (σ = σ0 * exp(σ1 * x)),
        /// where sigma1 is the baseline variance and sigma2 is the rate of growth
        /// in variance per units of expx.
        let exponential (sigma0: TypedTensor<Scalar,'u>) (sigma1: TypedTensor<Scalar,1/'v>) : VarianceFunction<'u,'v> =
            fun expected -> sigma0 * Typed.expVector (sigma1 * expected)

        // / Variance follows a power law in the expected value (σ = σ0 * x^α).
        // let power (sigma0: TypedTensor<Scalar,'u>) (alpha: TypedTensor<Scalar,1>) : VarianceFunction<'u,'v> =
        //     fun (expected: TypedTensor<Vector,'v>) -> sigma0 * (Typed.powVector expected alpha)


    let internal getData key (pairs: CodedMap<SeriesPair<'u>>) =
        match pairs |> Map.tryFindBy (fun k -> k.Value = key) with
        | Some p -> p
        | None -> failwithf "Predicted data was required for the variable '%s' but did not exist." key

    let internal reqToKey req =
        match req with
        | Measure r -> r
        | State r -> r

    /// Residual sum of squares. Provides a simple metric of distance between
    /// observed data and model predictions.
    let sumOfSquares (keys: ModelSystem.LikelihoodRequirement list) : Likelihood<'u> =
        fun _ data ->
            keys
            |> List.map (fun k ->
                let d = data |> getData (reqToKey k).Value
                let obs = Typed.tail d.Observed
                let exp = Typed.tail d.Expected // I don't think my original did tail?
                let diff = obs - exp
                Typed.squareVector diff)
            |> List.reduce (+)
            |> Typed.sumVector
            |> Typed.retype
        |> fun f -> { Evaluate = f; RequiredCodes = keys }

    let private one = Typed.ofScalar 1.0
    let private two = Typed.ofScalar 2.0
    let private half = Typed.ofScalar 0.5
    let private piT = Typed.ofScalar System.Math.PI
    let private twoPi = Typed.ofScalar (2.0 * System.Math.PI)

    /// Negative log likelihood for a bivariate normal distribution.
    /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
    let internal gaussianVec
        (varFun: Variance.VarianceFunction<'v, 'u>)
        (obsx: TypedTensor<Vector, 'u>)
        (expx: TypedTensor<Vector, 'u>)
        : TypedTensor<Vector, ``-logL``> =

        let sigma = varFun expx
        let diffx = obsx - expx

        let term1Scalar = half * Typed.logScalar twoPi
        let n = Typed.length obsx

        let term1 = Typed.broadcastScalarToVector term1Scalar n
        let term2 = Typed.logVector sigma
        let term3 = half * Typed.squareVector (diffx / sigma)

        term1 + term2 + term3 |> Typed.retype

    let internal gaussian' mkVariance (key: ModelSystem.LikelihoodRequirement) : Likelihood<'u> =
        fun (paramAccessor: ParameterValueAccessor) data ->
            let x = data |> getData (reqToKey key).Value
            let obsx = Typed.tail x.Observed
            let expx = Typed.tail x.Expected

            gaussianVec (mkVariance paramAccessor) obsx expx |> Tensors.Typed.sumVector
        |> fun f ->
            { Evaluate = f
              RequiredCodes = [ key ] }

    /// <summary>
    /// Log likelihood function for single equation system, assuming constant (homoscedastic) Gaussian error for x.
    /// Requires a parameter 'σ[x]' to be included in any `ModelSystem` that uses it.
    /// </summary>
    let gaussian (key: ModelSystem.LikelihoodRequirement) : Likelihood<'u> =
        let variance (paramAccessor: ParameterValueAccessor) =
            let sigmax = paramAccessor.Get "σ[x]" |> Typed.retype<parameter, 'u, Scalar>
            Variance.constant sigmax
        gaussian' variance key

    /// <summary>
    /// Log likelihood function for single equation system, assuming Gaussian error for x
    /// that increases exponentially dependent on a variance growth rate (σ1).
    /// Requires parameters 'σ0[x]' and 'σ1[x]' to be included in any `ModelSystem` that uses it.
    /// </summary>
    let gaussianWithExponentialVariance key =
        let variance (paramAccessor: ParameterValueAccessor) =
            let sigma0 = paramAccessor.Get "σ0[x]" |> Typed.retype<parameter,'u,Scalar>
            let sigma1 = paramAccessor.Get "σ1[x]" |> Typed.retype<parameter,1/'v,Scalar>
            Variance.exponential sigma0 sigma1
        gaussian' variance key

    /// <summary>
    /// Log likelihood function for single equation system, assuming Gaussian error for x
    /// that increases proportionally with quantity.
    /// Requires parameter 'σ[x]' to be included in any `ModelSystem` that uses it.
    /// </summary>
    let gaussianWithProportionalVariance key =
        let variance (paramAccessor: ParameterValueAccessor) =
            let sigma0 = paramAccessor.Get "σ[x]" |> Typed.retype<parameter,'u,Scalar>
            Variance.proportional sigma0
        gaussian' variance key

    // /// <summary>
    // /// Log likelihood function for single equation system, assuming Gaussian error for x
    // /// that increases on a power law with quantity; ⍺[x] is the exponent.
    // /// Requires parameter 'σ[x]' and '⍺[x]' to be included in any `ModelSystem` that uses it.
    // /// </summary>
    // let gaussianWithPowerVariance key =
    //     let variance (paramAccessor: ParameterValueAccessor) =
    //         let sigma0 = paramAccessor.Get "σ[x]" |> Typed.retype<parameter,'u,Scalar>
    //         let alpha = paramAccessor.Get "⍺[x]" |> Typed.retype<parameter,'u,Scalar>
    //         Variance.power sigma0 alpha
    //     gaussian' variance key


    /// Negative log likelihood for a bivariate normal distribution.
    /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
    let internal bivariateGaussianVec
        (sigmax: TypedTensor<Scalar, 'u>)
        (sigmay: TypedTensor<Scalar, 'u>)
        (rho: TypedTensor<Scalar, 1>)
        (obsx: TypedTensor<Vector, 'u>)
        (obsy: TypedTensor<Vector, 'u>)
        (expx: TypedTensor<Vector, 'u>)
        (expy: TypedTensor<Vector, 'u>)
        : TypedTensor<Vector, ``-logL``> =

        let diffx = obsx - expx
        let diffy = obsy - expy
        let zta1 = Typed.squareVector (diffx / sigmax)
        let zta2 = two * rho * (diffx / sigmax) * (diffy / sigmay)
        let zta3 = Typed.squareVector (diffy / sigmay)

        let vNegLog =
            two * piT * sigmax * sigmay * Typed.sqrtScalar (one - Typed.square rho)

        let vNegLogVec =
            Typed.broadcastScalarToVector vNegLog (Typed.length obsx) |> Typed.retype

        let q =
            (one / (one - Typed.square rho)) * (zta1 - zta2 + zta3: TypedTensor<Vector, 1>)

        (vNegLogVec + half * q) |> Typed.retype

    /// <summary>
    /// Log likelihood function for dual simultaneous system, assuming Gaussian error for both x and y.
    /// Requires parameters 'σ[x]', 'σ[y]' and 'ρ' to be included in any `ModelSystem` that uses it.
    /// </summary>
    let bivariateGaussian
        (key1: ModelSystem.LikelihoodRequirement)
        (key2: ModelSystem.LikelihoodRequirement)
        : Likelihood<'u> =
        fun (paramAccessor: ParameterValueAccessor) data ->
            let x = data |> getData (reqToKey key1).Value
            let y = data |> getData (reqToKey key2).Value

            let sigmax = paramAccessor.Get "σ[x]" |> Typed.retype<parameter, 'u, Scalar>
            let sigmay = paramAccessor.Get "σ[y]" |> Typed.retype<parameter, 'u, Scalar>
            let rho = paramAccessor.Get "ρ" |> Typed.retype<parameter, 1, Scalar>

            let obsx = Typed.tail x.Observed
            let obsy = Typed.tail y.Observed
            let expx = Typed.tail x.Expected
            let expy = Typed.tail y.Expected

            bivariateGaussianVec sigmax sigmay rho obsx obsy expx expy |> Typed.sumVector
        |> fun f ->
            { Evaluate = f
              RequiredCodes = [ key1; key2 ] }
