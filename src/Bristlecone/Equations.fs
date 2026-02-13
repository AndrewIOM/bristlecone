namespace Bristlecone.ModelLibrary

/// <summary>Negative log likelihood (-logL) functions to represent
/// a variety of distributions and data types.</summary>
///
/// <namespacedoc>
///   <summary>Pre-built model parts for use in Bristlecone</summary>
/// </namespacedoc>
module NegLogLikelihood =

    open Bristlecone
    open Bristlecone.ModelSystem
    open Bristlecone.Tensors

    /// A small helper for unit-safety when working with units-of-measure-based Language types.
    let internal getParamValue<[<Measure>] 'u>
        (paramAccessor: ParameterValueAccessor)
        (par: Language.IncludedParameter<'u>)
        : TypedTensor<Scalar, 'u> =
        paramAccessor.Get par.ParamId.Inner.Value |> Typed.retype

    let internal paramToAny<[<Measure>] 'u> (p: Language.IncludedParameter<'u>) =
        let boxed = Parameter.Pool.boxParam<'u> p.ParamId.Inner.Value p.Parameter
        p.ParamId.Inner, boxed

    let inline internal getData<[<Measure>] 'u, [<Measure>] 'state>
        (obsKey: Language.Require.ObsForLikelihood<'u>)
        (pairs: CodedMap<SeriesPair<'state>>)
        : SeriesPair<'u> =
        let key =
            match obsKey with
            | Language.Require.StateObs r -> r.Code
            | Language.Require.MeasureObs r -> r.Code

        match pairs |> Map.tryFindBy (fun k -> k = key) with
        | Some d ->
            { Expected = d.Expected |> Typed.retype<'state, 'u, Tensors.Vector>
              Observed = d.Observed |> Typed.retype<'state, 'u, Tensors.Vector> }
        | None -> failwithf "Predicted data was required for the variable '%s' but did not exist." key.Value


    let internal obsKeyToLikelihoodKey obsKey =
        match obsKey with
        | Language.Require.StateObs s -> LikelihoodRequirement.State s.Code
        | Language.Require.MeasureObs s -> LikelihoodRequirement.Measure s.Code

    let internal requirePositiveParam label (p: Language.IncludedParameter<'u>) =
        if p.Parameter |> Parameter.getConstraint <> Parameter.Constraint.PositiveOnly then
            failwithf "The specified %s parameter must be positive-only" label

    let private likelihoodTag = Typed.ofScalar 1.<``-logL``>


    /// <summary>Functions representing how variance is handled
    /// within likelihood functions.</summary>
    module Variance =

        /// A variance function maps expected values to per-point σ
        type VarianceFunction<[<Measure>] 'sigma, [<Measure>] 'x> =
            { Evaluate: ParameterValueAccessor -> TypedTensor<Vector, 'x> -> TypedTensor<Vector, 'sigma>
              RequiredParameters: (ShortCode.ShortCode * Parameter.Pool.AnyParameter) list }

        /// Variance is constant within through time.
        let constant sigma : VarianceFunction<'x, 'x> =
            { Evaluate =
                fun accessor expected ->
                    let sigmaVal = getParamValue accessor sigma
                    Typed.broadcastScalarToVector sigmaVal (Typed.length expected)
              RequiredParameters = [ paramToAny sigma ] }

        /// Variance is proportional to the expected value (σ = σ0 * x).
        let proportional (sigma0: Language.IncludedParameter<'sigma / 'x>) : VarianceFunction<'sigma, 'x> =
            { Evaluate =
                fun accessor expected ->
                    let sigma0Val = getParamValue accessor sigma0
                    expected * sigma0Val
              RequiredParameters = [ paramToAny sigma0 ] }

        /// Variance is exponential to the expected value (σ = σ0 * exp(σ1 * x)),
        /// where sigma1 is the baseline variance and sigma2 is the rate of growth
        /// in variance per units of expx.
        let exponential
            (sigma0: Language.IncludedParameter<'u>)
            (sigma1: Language.IncludedParameter<1 / 'v>)
            : VarianceFunction<'u, 'v> =
            { Evaluate =
                fun accessor expected ->
                    let sigma0Val = getParamValue accessor sigma0
                    let sigma1Val = getParamValue accessor sigma1
                    sigma0Val * Typed.expVector (sigma1Val * expected)
              RequiredParameters = [ paramToAny sigma0; paramToAny sigma1 ] }


    module Internal =

        /// Residual sum of squares. Provides a simple metric of distance between
        /// observed data and model predictions.
        let sumOfSquares (keys: Language.Require.ObsForLikelihood<'s> list) : Likelihood<'state> =
            fun _ data ->
                keys
                |> List.map (fun k ->
                    let d = data |> getData k
                    let obs = Typed.tail d.Observed
                    let exp = Typed.tail d.Expected
                    let diff = obs - exp
                    Typed.squareVector diff)
                |> List.reduce (+)
                |> Typed.sumVector
                |> Typed.retype
            |> fun f ->
                { Evaluate = f
                  RequiredCodes = List.map obsKeyToLikelihoodKey keys
                  RequiredParameters = [] }

        let private one = Typed.ofScalar 1.0
        let private two = Typed.ofScalar 2.0
        let private half = Typed.ofScalar 0.5
        let private twoPi = Typed.ofScalar (2.0 * System.Math.PI)

        /// Negative log likelihood for a bivariate normal distribution.
        /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
        let internal gaussianVec
            (sigma: TypedTensor<Vector, 'u>)
            (obsx: TypedTensor<Vector, 'u>)
            (expx: TypedTensor<Vector, 'u>)
            : TypedTensor<Vector, ``-logL``> =

            let diffx = obsx - expx

            let term1Scalar = half * Typed.logScalar twoPi
            let n = Typed.length obsx

            let term1 = Typed.broadcastScalarToVector term1Scalar n
            let term2 = Typed.logVector sigma
            let term3 = half * Typed.squareVector (diffx / sigma)

            (term1 + term2 + term3) * likelihoodTag

        let internal gaussian'
            (mkVariance: Variance.VarianceFunction<'s, 's>)
            (key: Language.Require.ObsForLikelihood<'s>)
            : Likelihood<'state> =
            fun (paramAccessor: ParameterValueAccessor) data ->
                let x = data |> getData key
                let obsx = Typed.tail x.Observed
                let expx = Typed.tail x.Expected
                let sigma = mkVariance.Evaluate paramAccessor expx

                gaussianVec sigma obsx expx |> Tensors.Typed.sumVector
            |> fun f ->
                { Evaluate = f
                  RequiredParameters = mkVariance.RequiredParameters
                  RequiredCodes = [ obsKeyToLikelihoodKey key ] }

        /// <summary>
        /// Log likelihood function for single equation system, assuming constant (homoscedastic) Gaussian error for x.
        /// Requires a parameter 'σ[x]' to be included in any `ModelSystem` that uses it.
        /// </summary>
        let gaussian (key: Language.Require.ObsForLikelihood<'s>) sigmax : Likelihood<'state> =
            gaussian' (Variance.constant sigmax) key

        /// Negative log likelihood for a bivariate normal distribution.
        /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
        let internal bivariateGaussianVec
            (sigmax: TypedTensor<Scalar, 'ux>)
            (sigmay: TypedTensor<Scalar, 'uy>)
            (rho: TypedTensor<Scalar, 1>)
            (obsx: TypedTensor<Vector, 'ux>)
            (obsy: TypedTensor<Vector, 'uy>)
            (expx: TypedTensor<Vector, 'ux>)
            (expy: TypedTensor<Vector, 'uy>)
            : TypedTensor<Vector, 1> =

            let diffx = obsx - expx
            let diffy = obsy - expy
            let zta1 = Typed.squareVector (diffx / sigmax)
            let zta2 = two * rho * (diffx / sigmax) * (diffy / sigmay)
            let zta3 = Typed.squareVector (diffy / sigmay)

            let q =
                half
                * (one / (one - Typed.square rho))
                * (zta1 - zta2 + zta3: TypedTensor<Vector, 1>)

            let logNorm =
                Typed.logScalar twoPi
                + Typed.logScalar sigmax
                + Typed.logScalar sigmay
                + half * Typed.logScalar (one - Typed.square rho)

            let logNormVec = Typed.broadcastScalarToVector logNorm (Typed.length obsx)

            logNormVec + q

        /// <summary>
        /// Log likelihood function for dual simultaneous system, assuming Gaussian error for both x and y.
        /// Requires parameters 'σ[x]', 'σ[y]' and 'ρ' to be defined and passed as arguments.
        /// </summary>
        let bivariateGaussian
            (key1: Language.Require.ObsForLikelihood<'ux>)
            (key2: Language.Require.ObsForLikelihood<'uy>)
            (sigmax: Language.IncludedParameter<'ux>)
            (sigmay: Language.IncludedParameter<'uy>)
            (rho: Language.IncludedParameter<1>)
            : Likelihood<'state> =

            requirePositiveParam "σ[x]" sigmax
            requirePositiveParam "σ[y]" sigmay

            fun (paramAccessor: ParameterValueAccessor) (data: CodedMap<SeriesPair<'state>>) ->
                let x = data |> getData key1
                let y = data |> getData key2

                let sigmax = getParamValue paramAccessor sigmax
                let sigmay = getParamValue paramAccessor sigmay
                let rho = getParamValue paramAccessor rho

                let obsx = Typed.tail x.Observed
                let obsy = Typed.tail y.Observed
                let expx = Typed.tail x.Expected
                let expy = Typed.tail y.Expected

                bivariateGaussianVec sigmax sigmay rho obsx obsy expx expy
                |> Typed.sumVector
                |> (*) likelihoodTag
            |> fun f ->
                { Evaluate = f
                  RequiredParameters = [ paramToAny sigmax; paramToAny sigmay; paramToAny rho ]
                  RequiredCodes = List.map obsKeyToLikelihoodKey [ key1; key2 ] }

        let internal logGaussianVec
            (sigma: TypedTensor<Vector, 'u>)
            (obsx: TypedTensor<Vector, 'u>)
            (expx: TypedTensor<Vector, 1>)
            : TypedTensor<Vector, ``-logL``> =

            let logx = Typed.logVector obsx
            let diff = logx - expx

            let term1Scalar = half * Typed.logScalar twoPi
            let n = Typed.length obsx

            let term1 = Typed.broadcastScalarToVector term1Scalar n
            let term2 = Typed.logVector sigma
            let term3 = Typed.logVector obsx
            let term4 = half * Typed.squareVector (diff / sigma)

            (term1 + term2 + term3 + term4) * likelihoodTag

        let internal logGaussian'
            (mkVariance: Variance.VarianceFunction<1, 1>)
            (key: Language.Require.ObsForLikelihood<1>)
            : Likelihood<'state> =
            fun (paramAccessor: ParameterValueAccessor) data ->
                let x = data |> getData key
                let obsx = Typed.tail x.Observed
                let expx = Typed.tail x.Expected |> Typed.logVector
                let sigma = mkVariance.Evaluate paramAccessor expx
                logGaussianVec sigma obsx expx |> Typed.sumVector
            |> fun f ->
                { Evaluate = f
                  RequiredParameters = mkVariance.RequiredParameters
                  RequiredCodes = [ obsKeyToLikelihoodKey key ] }

        let logGaussian key sigma : Likelihood<'state> =
            logGaussian' (Variance.constant sigma) key


    let Normal obs sigma = Internal.gaussian obs sigma
    let NormalWithVariance obs varianceFn = Internal.gaussian' varianceFn obs
    let LogNormal obs sigma = Internal.logGaussian obs sigma

    let BivariateNormal key1 key2 sigmax sigmay rho =
        Internal.bivariateGaussian key1 key2 sigmax sigmay rho

    let SumOfSquares obs = Internal.sumOfSquares obs
