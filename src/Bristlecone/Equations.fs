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
    open Bristlecone.Numerics
    open Bristlecone.Numerics.Typed

    /// A small helper for unit-safety when working with units-of-measure-based Language types.
    let internal getParamValue<'S,[<Measure>] 'u>
        (paramAccessor: ParameterValueAccessor<'S>)
        (par: Language.IncludedParameter<'u>)
        : TypedScalar<'S,'u> =
        paramAccessor.Get par.ParamId.Inner.Value |> Scalar.retype

    let internal paramToAny<[<Measure>] 'u> (p: Language.IncludedParameter<'u>) =
        let boxed = Parameter.Pool.stripUnit<'u> p.ParamId.Inner.Value p.Parameter
        p.ParamId.Inner, boxed

    let inline internal getData<[<Measure>] 'u, [<Measure>] 'state>
        (obsKey: Language.Require.ObsForLikelihood<'u>)
        (pairs: CodedMap<SeriesPair<'S,'V,'state>>)
        : SeriesPair<'S,'V,'u> =
        let key =
            match obsKey with
            | Language.Require.StateObs r -> r.Code
            | Language.Require.MeasureObs r -> r.Code

        match pairs |> Map.tryFindBy (fun k -> k = key) with
        | Some d ->
            { Expected = d.Expected |> Vector.retype
              Observed = d.Observed |> Vector.retype }
        | None -> failwithf "Predicted data was required for the variable '%s' but did not exist." key.Value


    let internal obsKeyToLikelihoodKey obsKey =
        match obsKey with
        | Language.Require.StateObs s -> LikelihoodRequirement.State s.Code
        | Language.Require.MeasureObs s -> LikelihoodRequirement.Measure s.Code

    let internal requirePositiveParam label (p: Language.IncludedParameter<'u>) =
        match p.Parameter |> Parameter.getConstraint with
        | Parameter.Constraint.PositiveOnly -> ()
        | Parameter.Constraint.Bounded(l, _) ->
            if l >= Units.tagUnit<'u> 0. then
                ()
            else
                failwithf "The specified %s parameter must be positive-only" label
        | Parameter.Constraint.Unconstrained -> failwithf "The specified %s parameter must be positive-only" label

    let private likelihoodTag b = Typed.ofScalar b 1.<``-logL``>


    /// <summary>Functions representing how variance is handled
    /// within likelihood functions.</summary>
    module Variance =

        /// A variance function maps expected values to per-point σ
        type VarianceFunction<'S,'V,'M, [<Measure>] 'sigma, [<Measure>] 'x> =
            { Evaluate: NumericEngine<'S,'V,'M> -> ParameterValueAccessor<'S> -> TypedVector<'S,'V,'x> -> TypedVector<'S,'V,'sigma>
              RequiredParameters: (ShortCode.ShortCode * Parameter.Pool.ParameterNoUnit) list }

        /// Variance is constant within through time.
        let constant sigma : VarianceFunction<'S,'V,'M, 'x, 'x> =
            { Evaluate =
                fun numEngine accessor expected ->
                    let sigmaVal = getParamValue accessor sigma
                    Scalar.broadcast numEngine sigmaVal (Vector.length expected)
              RequiredParameters = [ paramToAny sigma ] }

        /// Variance is proportional to the expected value (σ = σ0 * x).
        let proportional (sigma0: Language.IncludedParameter<'sigma / 'x>) : VarianceFunction<'S,'V,'M,'sigma, 'x> =
            { Evaluate =
                fun _ accessor expected ->
                    let sigma0Val = getParamValue accessor sigma0
                    expected * sigma0Val
              RequiredParameters = [ paramToAny sigma0 ] }

        /// Variance is exponential to the expected value (σ = σ0 * exp(σ1 * x)),
        /// where sigma1 is the baseline variance and sigma2 is the rate of growth
        /// in variance per units of expx.
        let exponential
            (sigma0: Language.IncludedParameter<'u>)
            (sigma1: Language.IncludedParameter<1 / 'v>)
            : VarianceFunction<'S,'V,'M,'u, 'v> =
            { Evaluate =
                fun _ accessor expected ->
                    let sigma0Val = getParamValue accessor sigma0
                    let sigma1Val = getParamValue accessor sigma1
                    Vector.exp (expected * sigma1Val) * sigma0Val
              RequiredParameters = [ paramToAny sigma0; paramToAny sigma1 ] }


    module Internal =

        /// Residual sum of squares. Provides a simple metric of distance between
        /// observed data and model predictions.
        let sumOfSquares (keys: Language.Require.ObsForLikelihood<'s> list) : Likelihood<'S,'V,'state> =
            fun _ data ->
                keys
                |> List.map (fun k ->
                    let d = data |> getData k
                    let obs = Vector.tail d.Observed
                    let exp = Vector.tail d.Expected
                    let diff = obs - exp
                    Vector.square diff)
                |> List.reduce (+)
                |> Vector.sum
                |> Scalar.retype
            |> fun f ->
                { Evaluate = f
                  RequiredCodes = List.map obsKeyToLikelihoodKey keys
                  RequiredParameters = [] }

        /// Negative log likelihood for a bivariate normal distribution.
        /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
        let internal gaussianVec
            (eng: NumericEngine<'S,'V,'M>)
            (sigma: TypedVector<'S,'V,'u>)
            (obsx: TypedVector<'S,'V,'u>)
            (expx: TypedVector<'S,'V,'u>)
            : TypedVector<'S,'V,``-logL``> =

            let diffx = obsx - expx
            let constants = Scalar.constants eng
            let term1Scalar = constants.half * Scalar.log (constants.two * constants.pi)
            let n = Vector.length obsx

            let term1 = Scalar.broadcast eng term1Scalar n
            let term2 = Vector.log sigma
            let term3 = constants.half * Vector.square (diffx / sigma)

            (term1 + term2 + term3) * likelihoodTag eng.scalarBackend

        let internal gaussian'
            (numEng: NumericEngine<'S,'V,'M>)
            (mkVariance: Variance.VarianceFunction<'S,'V,'M,'u,'u>)
            (key: Language.Require.ObsForLikelihood<'u>)
            : Likelihood<'S,'V,'state> =
            
            let fn paramAccessor data =
                let x = data |> getData key
                let obsx = Vector.tail x.Observed
                let expx = Vector.tail x.Expected
                let sigma = mkVariance.Evaluate numEng paramAccessor expx
                gaussianVec numEng sigma obsx expx |> Vector.sum
            
            { Evaluate = fn
              RequiredParameters = mkVariance.RequiredParameters
              RequiredCodes = [ obsKeyToLikelihoodKey key ] }

        /// <summary>
        /// Log likelihood function for single equation system, assuming constant (homoscedastic) Gaussian error for x.
        /// Requires a parameter 'σ[x]' to be included in any `ModelSystem` that uses it.
        /// </summary>
        let gaussian numEng (key: Language.Require.ObsForLikelihood<'s>) sigmax : Likelihood<'S,'V,'state> =
            gaussian' numEng (Variance.constant sigmax) key

        /// Negative log likelihood for a bivariate normal distribution.
        /// For two random variables with bivariate normal N(u1,u2,sigma1,sigma2,rho).
        let internal bivariateGaussianVec
            (eng: NumericEngine<'S,'V,'M>)
            (sigmax: TypedScalar<'S,'ux>)
            (sigmay: TypedScalar<'S,'uy>)
            (rho: TypedScalar<'S,1>)
            (obsx: TypedVector<'S,'V,'ux>)
            (obsy: TypedVector<'S,'V,'uy>)
            (expx: TypedVector<'S,'V,'ux>)
            (expy: TypedVector<'S,'V,'uy>)
            : TypedVector<'S,'V,1> =

            let constants = Scalar.constants eng

            let diffx = obsx - expx
            let diffy = obsy - expy
            let zta1 = Vector.square (diffx / sigmax)
            let zta2 = constants.two * rho * (diffx / sigmax) * (diffy / sigmay)
            let zta3 = Vector.square (diffy / sigmay)

            let q =
                constants.half
                * (constants.one / (constants.one - Scalar.square rho))
                * (zta1 - zta2 + zta3: TypedVector<'S,'V,1>)

            let logNorm =
                Scalar.log (constants.pi * constants.two)
                + Scalar.log sigmax
                + Scalar.log sigmay
                + constants.half * Scalar.log (constants.one - Scalar.square rho)

            let logNormVec = Scalar.broadcast eng logNorm (Vector.length obsx)

            logNormVec + q

        /// <summary>
        /// Log likelihood function for dual simultaneous system, assuming Gaussian error for both x and y.
        /// Requires parameters 'σ[x]', 'σ[y]' and 'ρ' to be defined and passed as arguments.
        /// </summary>
        let bivariateGaussian<[<Measure>] 'ux, [<Measure>] 'uy, [<Measure>] 'state>
            numEngine
            (key1: Language.Require.ObsForLikelihood<'ux>)
            (key2: Language.Require.ObsForLikelihood<'uy>)
            (sigmax: Language.IncludedParameter<'ux>)
            (sigmay: Language.IncludedParameter<'uy>)
            (rho: Language.IncludedParameter<1>)
            : Likelihood<'S,'V,'state> =

            requirePositiveParam "σ[x]" sigmax
            requirePositiveParam "σ[y]" sigmay

            let fn (paramAccessor: ParameterValueAccessor<'S>) (data: CodedMap<SeriesPair<'S,'V,'state>>) =
                let x = data |> getData key1
                let y = data |> getData key2

                let sigmax = getParamValue paramAccessor sigmax
                let sigmay = getParamValue paramAccessor sigmay
                let rho = getParamValue paramAccessor rho

                let obsx = Vector.tail x.Observed
                let obsy = Vector.tail y.Observed
                let expx = Vector.tail x.Expected
                let expy = Vector.tail y.Expected

                bivariateGaussianVec numEngine sigmax sigmay rho obsx obsy expx expy
                |> Vector.sum
                |> (*) (likelihoodTag numEngine.scalarBackend)
            
            { Evaluate = fn
              RequiredParameters = [ paramToAny sigmax; paramToAny sigmay; paramToAny rho ]
              RequiredCodes = [ obsKeyToLikelihoodKey key1; obsKeyToLikelihoodKey key2 ] }

        let internal logGaussianVec
            numEngine
            (sigma: TypedVector<'S,'V,'u>)
            (obsx: TypedVector<'S,'V,'u>)
            (expx: TypedVector<'S,'V,1>)
            : TypedVector<'S,'V,``-logL``> =

            let constants = Scalar.constants numEngine

            let logx = Vector.log obsx
            let diff = logx - expx

            let term1Scalar = constants.half * Scalar.log (constants.two * constants.pi)
            let n = Vector.length obsx

            let term1 = Scalar.broadcast numEngine term1Scalar n
            let term2 = Vector.log sigma
            let term3 = Vector.log obsx
            let term4 = constants.half * Vector.square (diff / sigma)
            
            (term1 + term2 + term3 + term4) * likelihoodTag numEngine.scalarBackend

        let internal logGaussian'
            eng
            (mkVariance: Variance.VarianceFunction<'S,'V,'M,1, 1>)
            (key: Language.Require.ObsForLikelihood<1>)
            : Likelihood<'S,'V,'state> =
            fun (paramAccessor: ParameterValueAccessor<'S>) data ->
                let x = data |> getData key
                let obsx = Vector.tail x.Observed
                let expx = Vector.tail x.Expected |> Vector.log
                let sigma = mkVariance.Evaluate eng paramAccessor expx
                logGaussianVec eng sigma obsx expx |> Vector.sum
            |> fun f ->
                { Evaluate = f
                  RequiredParameters = mkVariance.RequiredParameters
                  RequiredCodes = [ obsKeyToLikelihoodKey key ] }

        let logGaussian eng key sigma : Likelihood<'S,'V,'state> =
            logGaussian' eng (Variance.constant sigma) key


    let Normal obs sigma = Internal.gaussian obs sigma
    let NormalWithVariance obs varianceFn = Internal.gaussian' varianceFn obs
    let LogNormal obs sigma = Internal.logGaussian obs sigma

    let BivariateNormal key1 key2 sigmax sigmay rho =
        Internal.bivariateGaussian key1 key2 sigmax sigmay rho

    let SumOfSquares obs = Internal.sumOfSquares obs
