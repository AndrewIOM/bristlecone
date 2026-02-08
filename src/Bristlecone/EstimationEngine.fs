namespace Bristlecone

open Bristlecone.Time
open Bristlecone.Tensors

/// Represents an ordinary differential equation model system and
/// its likelihood as an objective function that may be optimised.
module ModelSystem =

    [<Measure>]
    type ``environment``

    [<Measure>]
    type ``state``

    /// The external environment at any time t.
    type ExternalEnvironment = CodedMap<TypedTensor<Scalar, environment>>

    /// An equation that may require fixed or free parameters,
    /// the current time t, the current response value, and / or
    /// external environmental time series.
    type GenericModelEquation<[<Measure>] 'timeUnit, [<Measure>] 'returnUnit> =
        TypedTensor<Vector, ``parameter``>
            -> CodedMap<TypedTensor<Scalar, ``environment``>>
            -> TypedTensor<Scalar, 'timeUnit>
            -> TypedTensor<Scalar, state>
            -> TypedTensor<Scalar, 'returnUnit>


    type RateEquation<[<Measure>] 'timeUnit> = GenericModelEquation<'timeUnit, state / 'timeUnit>
    type StateEquation<[<Measure>] 'timeUnit> = GenericModelEquation<'timeUnit, state>

    /// The model system may make use of either stepped or integrated-time
    /// equations. Each are both require the same inputs, but differ in
    /// whether a solver is applied or not. Here, the DU's purpose is for
    /// additional type safety.
    /// TODO Allow state to differ between equations.
    type ModelForm<[<Measure>] 'timeUnit> =
        | DifferenceEqs of CodedMap<StateEquation<'timeUnit>>
        | DifferentialEqs of CodedMap<RateEquation<'timeUnit>>

    /// Predicted time-series for a single variable.
    /// 'u is the state unit of measure.
    type PredictedSeries<[<Measure>] 'u> = TypedTensor<Vector, 'u>
    // type PredictedSeries<'data,'date,'timeunit,'timespan> =
    //     TimeSeries.TimeSeries<ModelFitToPoint<'data>, 'date, 'timeunit, 'timespan>

    /// A function that returns a parameter's current value by its name.
    type ParameterValueAccessor =
        | ParameterValueAccessor of (string -> TypedTensor<Scalar, ``parameter``>)

        member this.Get name =
            let (ParameterValueAccessor v) = this in v name

    /// A single variable’s observed vs expected values
    /// after model-fitting.
    type SeriesPair<[<Measure>] 'u> =
        { Observed: TypedTensor<Vector, 'u>
          Expected: TypedTensor<Vector, 'u> }

    /// The negative log likelihood given the predicted and observed
    /// per-variable time-series and a function to retrieve parameters
    /// required by the likelihood function.
    type LikelihoodEval<[<Measure>] 'u> =
        ParameterValueAccessor -> CodedMap<SeriesPair<'u>> -> TypedTensor<Scalar, ``-logL``>

    type LikelihoodRequirement =
        | State of ShortCode.ShortCode
        | Measure of ShortCode.ShortCode

    type Likelihood<[<Measure>] 'u> =
        { RequiredCodes: LikelihoodRequirement list
          RequiredParameters: (ShortCode.ShortCode * Parameter.Pool.AnyParameter) list
          Evaluate: LikelihoodEval<'u> }

        with
            static member (+) (l1, l2) =
                { RequiredCodes = l1.RequiredCodes @ l2.RequiredCodes
                  RequiredParameters = l1.RequiredParameters @ l2.RequiredParameters
                  Evaluate =
                    fun getParam seriesMap ->
                        l1.Evaluate getParam seriesMap
                        + l2.Evaluate getParam seriesMap }

    /// A function that computes a measured system property given a
    /// current (time t) and previous (time t-1) system state.
    type Measurement<[<Measure>] 'u> =
        TypedTensor<Vector, ``parameter``> // current parameters
            -> CodedMap<TypedTensor<Vector, state>> // states time-series
            -> TypedTensor<Scalar, state> // last value of this measurement
            -> int // current time index
            -> TypedTensor<Scalar, 'u>

    /// Computes the hidden state’s initial value at t0
    /// from fitted parameters and baseline known values.
    type Initialiser<[<Measure>] 'u> =
        TypedTensor<Vector, ``parameter``> // current parameters
            -> CodedMap<TypedTensor<Scalar, ``environment``>> // baseline environment values at t0
            -> CodedMap<TypedTensor<Scalar, ``state``>> // baseline observed state values at t0
            -> TypedTensor<Scalar, 'u> // initial hidden state value

    type ModelSystem<[<Measure>] 'modelTimeUnit> =
        { Parameters: Parameter.Pool.ParameterPool
          EnvironmentKeys: ShortCode.ShortCode list
          Equations: ModelForm<'modelTimeUnit>
          Measures: CodedMap<Measurement<state>>
          Initialisers: CodedMap<Initialiser<state>>
          NegLogLikelihood: Likelihood<state> }

    type FitValue =
        { Fit: float<state>; Obs: float<state> }

    type FitSeries<'date, 'timeunit, 'timespan> = TimeSeries<FitValue, 'date, 'timeunit, 'timespan>

    /// An estimated model fit for a time-series model.
    type EstimationResult<'date, 'timeunit, 'timespan> =
        { ResultId: System.Guid
          Likelihood: float<``-logL``>
          Parameters: Parameter.Pool.ParameterPool
          Series: CodedMap<FitSeries<'date, 'timeunit, 'timespan>>
          Trace: (float<``-logL``> * float<``parameter``>[]) list
          InternalDynamics: CodedMap<float<state>[]> option }

/// The estimation engine represents the method used to
/// calculate equations and optimise a likelihood function.
/// The whole estimation engine is tensor-based internally,
/// but may take float-based equations as a legacy option.
module EstimationEngine =

    open System
    open Bristlecone.Logging
    open Bristlecone.Conditioning
    open ModelSystem

    /// A point in optimisation-space. Optim-space
    /// is tensor-based, so all points are tensor vectors
    /// representing the parameters.
    type Point = TypedTensor<Vector, ``optim-space``>

    /// The likelihood at a particular place in
    /// optimisation space.
    type Solution = float<``-logL``> * Point

    /// An objective function that can be optimised
    /// within an optimisation routine.
    type Objective = Point -> Tensors.TypedTensor<Tensors.Scalar, ``-logL``>

    /// Low‑level compiled likelihood
    /// Works directly with a parameter tensor (real space).
    type CompiledLikelihood<[<Measure>] 'u> =
        TypedTensor<Vector, ``parameter``> -> CodedMap<SeriesPair<'u>> -> TypedTensor<Scalar, ``-logL``>

    /// Reasons optimisation may stop.
    type OptimStopReason =
        | Continue
        | Stationary
        | Converged
        | Stuck
        | Degenerate
        | MaxIterations
        | NoImprovement
        | Custom of string

    /// Determines if the end has been reached based on a list
    /// of tupled Solutions with their iteration number.
    type EndCondition = Solution list -> int<iteration> -> OptimStopReason

    /// The domain is fine to be float-based, as it is only
    /// used to initialise the optimisation routine.
    /// Represents the bounds and any constraint.
    type Domain = (float<``optim-space``> * float<``optim-space``> * Parameter.Constraint)[]

    /// Model equations for estimation may be require time to be
    /// in indexed form (i.e. common across models and data).
    /// Parameter values are required in 'real' parameter units
    /// rather than (transformed) optimisation space.
    type ModelEquations =
        | TensorODEs of CodedMap<TensorODE>
        | FloatODEs of CodedMap<FloatODE>

    and TensorODE =
        TypedTensor<Vector, ``parameter``>
            -> CodedMap<TypedTensor<Scalar, ``environment``>>
            -> TypedTensor<Scalar, ``time index``>
            -> TypedTensor<Scalar, state>
            -> TypedTensor<Scalar, state / ``time index``>

    and FloatODE =
        CodedMap<float<parameter>>
            -> float<``time index``>
            -> float<state>
            -> ExternalEnvironment
            -> float<state / ``time index``>

    /// Represents an external logging function.
    type WriteOut = LogEvent -> unit

    /// A function that, given parameters, produces a parameterised RHS
    /// for the ODE system. This is the output of the static solver setup.
    type UnparameterisedRHS =
        TypedTensor<Vector, ``parameter``>
            -> TypedTensor<Scalar, ``time index``>
            -> TypedTensor<Vector, state>
            -> CodedMap<TypedTensor<Scalar, state / ``time index``>>

    /// A parameterised RHS — parameters already bound.
    /// This is what the integration routine actually steps.
    type ParameterisedRHS =
        TypedTensor<Scalar, ``time index``>
            -> TypedTensor<Vector, state>
            -> CodedMap<TypedTensor<Scalar, state / ``time index``>>


    module Solver =

        /// 'Bakes in' environment, timeline,
        /// model keys, etc., producing an UnparameterisedRHS
        /// that only needs parameters.
        type SolverSetup<'date, 'timeunit, 'timespan> =
            float<``time index``> // tInitial
                -> float<``time index``> // tEnd
                -> float<``time index``> // tStep
                -> CodedMap<TypedTensor<Scalar, environment>> // initial environment
                -> CodedMap<TimeIndex.TimeIndex<TypedTensor<Scalar, environment>, 'date, 'timeunit, 'timespan, 1>> // external env
                -> ModelEquations
                -> UnparameterisedRHS

        type ConfiguredSolver =
            TypedTensor<Vector, ``parameter``> // parameters
                -> CodedMap<TypedTensor<Vector, state>> * CodedMap<TypedTensor<Scalar, state>> // predictions * initial state

        /// A solver may configure environmental forcing variables
        /// to be interpolated if they are not available at an exact
        /// time t requested by integration.
        type InterpolationMode =
            | Exact
            | Lower
            | Linear


    module Integration =

        /// Represents a low-level numerical method used
        /// to integrate functions. Takes the intial time,
        /// final time, and time step,
        /// and returns a function that
        /// is compiled to only require the current time
        /// and current state.
        /// Must return the baseline state plus evolutions.
        type IntegrationRoutine =
            TypedTensor<Scalar, ``time index``> // tInitial
                -> TypedTensor<Scalar, ``time index``> // tEnd
                -> TypedTensor<Scalar, ``time index``> // tStep
                -> CodedMap<TypedTensor<Scalar, state>> // initialConditions
                -> ParameterisedRHS
                -> CodedMap<TypedTensor<Vector, state>>


    module Optimisation =

        type Optimise =
            Random
                -> WriteOut
                -> EndCondition
                -> Domain
                -> Point option // optional starting point
                -> Objective
                -> Solution list

        /// An `Optimiser` is an optimisation algorithm that may work either
        /// in 'transformed' parameter space (where parameter constraints are
        /// automatically handled) or in 'detatched' space (where the optimisation
        /// algorithm is responsible for respecting parameter constraints).
        type Optimiser =
            | InTransformedSpace of Optimise
            | InDetachedSpace of Optimise

    type TimeMode =
        | Discrete
        | Continuous of Integration.IntegrationRoutine

    type EstimationEngine<'date, 'timespan, [<Measure>] 'modelTimeUnit, [<Measure>] 'state> =
        { TimeHandling: TimeMode
          OptimiseWith: Optimisation.Optimiser
          Conditioning: Conditioning<'state>
          LogTo: WriteOut
          ToModelTime: DateMode.Conversion.ResolutionToModelUnits<'date, 'timespan, 'modelTimeUnit>
          InterpolationGlobal: Solver.InterpolationMode
          InterpolationPerVariable: CodedMap<Solver.InterpolationMode>
          Random: Random }
