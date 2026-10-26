namespace Bristlecone

open Bristlecone.Time
open Bristlecone.Numerics.Typed

/// Represents an ordinary differential equation model system and
/// its likelihood as an objective function that may be optimised.
module ModelSystem =

    [<Measure>]
    type ``environment``

    [<Measure>]
    type ``state``

    /// An equation that may require fixed or free parameters,
    /// the current time t, the current response value, and / or
    /// external environmental time series.
    type GenericModelEquation<'S,'V,[<Measure>] 'timeUnit, [<Measure>] 'returnUnit> =
        FsVector<'S,'V,``parameter``>
            -> CodedMap<FsScalar<'S,``environment``>>
            -> FsScalar<'S,'timeUnit>
            -> FsScalar<'S,state>
            -> FsScalar<'S,'returnUnit>

    type RateEquation<'S,'V,[<Measure>] 'timeUnit> = GenericModelEquation<'S,'V,'timeUnit, state / 'timeUnit>
    type StateEquation<'S,'V,[<Measure>] 'timeUnit> = GenericModelEquation<'S,'V,'timeUnit, state>

    /// The model system may make use of either stepped or integrated-time
    /// equations. Each are both require the same inputs, but differ in
    /// whether a solver is applied or not. Here, the DU's purpose is for
    /// additional type safety.
    type ModelForm<'S,'V,[<Measure>] 'timeUnit> =
        | DifferenceEqs of CodedMap<StateEquation<'S,'V,'timeUnit>>
        | DifferentialEqs of CodedMap<RateEquation<'S,'V,'timeUnit>>

    /// Predicted time-series for a single variable.
    /// 'u is the state unit of measure.
    type PredictedSeries<[<Measure>] 'u,'S,'V> = FsVector<'S,'V,'u>

    /// A function that returns a parameter's current value by its name.
    type ParameterValueAccessor<'S> =
        | ParameterValueAccessor of (string -> FsScalar<'S,``parameter``>)

        member this.Get name =
            let (ParameterValueAccessor v) = this in v name

    /// A single variable’s observed vs expected values
    /// after model-fitting.
    type SeriesPair<'S,'V,[<Measure>] 'u> =
        { Observed: FsVector<'S,'V,'u>
          Expected: FsVector<'S,'V,'u> }

    /// The negative log likelihood given the predicted and observed
    /// per-variable time-series and a function to retrieve parameters
    /// required by the likelihood function.
    type LikelihoodEval<'S,'V,[<Measure>] 'u> =
        ParameterValueAccessor<'S> -> CodedMap<SeriesPair<'S,'V,'u>> -> FsScalar<'S,``-logL``>

    type LikelihoodRequirement =
        | State of ShortCode.ShortCode
        | Measure of ShortCode.ShortCode

    type Likelihood<'S,'V,[<Measure>] 'u> =
        { RequiredCodes: LikelihoodRequirement list
          RequiredParameters: (ShortCode.ShortCode * Parameter.Pool.ParameterNoUnit) list
          Evaluate: LikelihoodEval<'S,'V,'u> }

        static member (+)(l1, l2) =
            { RequiredCodes = l1.RequiredCodes @ l2.RequiredCodes
              RequiredParameters = l1.RequiredParameters @ l2.RequiredParameters
              Evaluate = fun getParam seriesMap -> l1.Evaluate getParam seriesMap + l2.Evaluate getParam seriesMap }

    /// A function that computes a measured system property given a
    /// current (time t) and previous (time t-1) system state.
    type Measurement<'S,'V,[<Measure>] 'u> =
        FsVector<'S,'V, ``parameter``> // current parameters
            -> CodedMap<FsVector<'S,'V,state>> // states time-series
            -> FsScalar<'S,state> // last value of this measurement
            -> int // current time index
            -> FsScalar<'S,'u>

    /// Computes the hidden state’s initial value at t0
    /// from fitted parameters and baseline known values.
    type Initialiser<'S,'V,[<Measure>] 'u> =
        FsVector<'S,'V,``parameter``> // current parameters
            -> CodedMap<FsScalar<'S,``environment``>> // baseline environment values at t0
            -> CodedMap<FsScalar<'S,``state``>> // baseline observed state values at t0
            -> FsScalar<'S,'u> // initial hidden state value

    type ModelSystem<'S,'V,[<Measure>] 'modelTimeUnit> =
        { Parameters: Parameter.Pool.ParameterPool
          EnvironmentKeys: ShortCode.ShortCode list
          Equations: ModelForm<'S,'V,'modelTimeUnit>
          Measures: CodedMap<Measurement<'S,'V,state>>
          Initialisers: CodedMap<Initialiser<'S,'V,state>>
          NegLogLikelihood: Likelihood<'S,'V,state> }

    type FitValue =
        { Fit: float<state>; Obs: float<state> }

    type FitSeries<'date, 'timeunit, 'timespan> = TimeSeries<FitValue, 'date, 'timeunit, 'timespan>

    type Trace =
        { ComponentName: string
          StageName: string
          ReplicateNumber: int
          Results: (float<``-logL``> * float<``parameter``>[]) list }

    /// An estimated model fit for a time-series model.
    type EstimationResult<'date, 'timeunit, 'timespan> =
        { ResultId: System.Guid
          Likelihood: float<``-logL``>
          Parameters: Parameter.Pool.ParameterPool
          Series: CodedMap<FitSeries<'date, 'timeunit, 'timespan>>
          Trace: Trace list
          InternalDynamics: CodedMap<float<state>[]> option
          Metadata: List<string * string> }

/// The estimation engine represents the method used to
/// calculate equations and optimise a likelihood function.
/// The whole estimation engine is tensor-based internally,
/// but may take float-based equations as a legacy option.
module EstimationEngine =

    open System
    open Bristlecone.Logging
    open Bristlecone.Conditioning
    open ModelSystem

    /// A point in optimisation-space, which may be
    /// in a transformed or 'real' parameter space.
    type Point<'S,'V,[<Measure>] 'space> = FsVector<'S,'V,'space>

    /// An objective function that can be optimised
    /// within an optimisation routine.
    type Objective<'S,'V,[<Measure>] 'space> = Point<'S,'V,'space> -> FsScalar<'S,``-logL``>

    /// Low‑level compiled likelihood
    /// Works directly with a parameter tensor (real space).
    type CompiledLikelihood<'S,'V,[<Measure>] 'u> =
        FsVector<'S,'V, ``parameter``> -> CodedMap<SeriesPair<'S,'V,'u>> -> FsScalar<'S,``-logL``>

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

    /// The likelihood at a particular place in
    /// optimisation space.
    type Solution<[<Measure>] 'space> = float<``-logL``> * float<'space>[]

    /// Determines if the end has been reached based on a list
    /// of tupled Solutions with their iteration number.
    type EndCondition<[<Measure>] 'space> = Solution<'space> list -> int<iteration> -> OptimStopReason

    /// Model equations for estimation may be require time to be
    /// in indexed form (i.e. common across models and data).
    /// Parameter values are required in 'real' parameter units
    /// rather than (transformed) optimisation space.
    type ModelEquations<'S,'V> = CodedMap<RateEquation<'S,'V,``time index``>>

    /// Represents an external logging function.
    type WriteOut = LogEvent -> unit

    /// A function that, given parameters, produces a parameterised RHS
    /// for the ODE system. This is the output of the static solver setup.
    type UnparameterisedRHS<'S,'V> =
        FsVector<'S,'V,``parameter``>
            -> FsScalar<'S,``time index``>
            -> FsVector<'S,'V,state>
            -> CodedMap<FsScalar<'S,state / ``time index``>>

    /// A parameterised RHS — parameters already bound.
    /// This is what the integration routine actually steps.
    type ParameterisedRHS<'S,'V> =
        FsScalar<'S,``time index``>
            -> FsVector<'S,'V,state>
            -> CodedMap<FsScalar<'S,state / ``time index``>>


    module Solver =

        /// 'Bakes in' environment, timeline,
        /// model keys, etc., producing an UnparameterisedRHS
        /// that only needs parameters.
        type SolverSetup<'date, 'timeunit, 'timespan, 'S,'V> =
            float<``time index``> // tInitial
                -> float<``time index``> // tEnd
                -> float<``time index``> // tStep
                -> CodedMap<FsScalar<'S,environment>> // initial environment
                -> CodedMap<TimeIndex.TimeIndex<FsScalar<'S,environment>, 'date, 'timeunit, 'timespan, 1>> // external env
                -> ModelEquations<'S,'V>
                -> UnparameterisedRHS<'S,'V>

        type ConfiguredSolver<'S,'V> =
            FsVector<'S,'V,``parameter``> // parameters
                -> CodedMap<FsVector<'S,'V,state>> * CodedMap<FsScalar<'S,state>> // predictions * initial state

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
        type IntegrationRoutine<'S,'V,'M> =
            Numerics.NumericEngine<'S,'V,'M>
                -> FsScalar<'S,``time index``> // tInitial
                -> FsScalar<'S,``time index``> // tEnd
                -> FsScalar<'S,``time index``> // tStep
                -> CodedMap<FsScalar<'S,state>> // initialConditions
                -> ParameterisedRHS<'S,'V>
                -> CodedMap<FsVector<'S,'V,state>>


    module Optimisation =

        /// Represents the trace of an optimisation heuristic,
        /// which may have multiple 'components' (i.e. sub-algorithms)
        /// and one or many stages within each.
        type OptimisationTrace<[<Measure>] 'space> =
            { Component: string
              Stage: string
              Replicate: int
              Results: Solution<'space> list }

        /// Represents a wrapped objective that may be
        /// evalutated within an optimisation routine.
        /// Optimisers work exclusively in float-space.
        type EvaluateObjective<[<Measure>] 'u> = {
            Evaluate : float<'u>[] -> float<``-logL``>
            Gradient: float<'u>[] -> float<``-logL``>
            EvaluateVectorised: float<'u>[,] -> float<``-logL``>[]
        }

        /// The domain is fine to be float-based, as it is only
        /// used to initialise the optimisation routine.
        /// Represents the bounds and any constraint.
        type Domain<[<Measure>] 'u> = (float<'u> * float<'u> * Parameter.Constraint<'u>)[]

        type OptimPoint<[<Measure>] 'space> = float<'space>[]

        type Optimise<[<Measure>] 'space> =
            Random
                -> WriteOut
                -> EndCondition<'space>
                -> Domain<'space>
                -> OptimPoint<'space> option // optional starting point
                -> EvaluateObjective<'space>
                -> OptimisationTrace<'space> list

        /// An `Optimiser` is an optimisation algorithm that may work either
        /// in 'transformed' parameter space (where parameter constraints are
        /// automatically handled) or in 'detatched' space (where the optimisation
        /// algorithm is responsible for respecting parameter constraints).
        type Optimiser<[<Measure>] 'space,'S,'V,'M> = {
            Optimise: Optimise<'space>
            MakeTransformToSpace: Numerics.NumericEngine<'S,'V,'M> -> Parameter.Pool.ParameterPool -> Parameter.Pool.OptimiserConfig<'S,'V,'space>
        }

        type GenericOptimResult =
            | TransformedResult of OptimisationTrace<``optim-space-transformed``> list
            | UntransformedResult of OptimisationTrace<``optim-space``> list

    type TimeMode<'S,'V,'M> =
        | Discrete
        | Continuous of Integration.IntegrationRoutine<'S,'V,'M>

    type EstimationEngine<'S,'V,'M,'date, 'timespan, [<Measure>] 'modelTimeUnit, [<Measure>] 'state, [<Measure>] 'space> =
        { TimeHandling: TimeMode<'S,'V,'M>
          OptimiseWith: Optimisation.Optimiser<'space,'S,'V,'M>
          Conditioning: Conditioning<'state>
          Backend: Numerics.NumericEngine<'S,'V,'M>
          LogTo: WriteOut
          ToModelTime: DateMode.Conversion.ResolutionToModelUnits<'date, 'timespan, 'modelTimeUnit>
          InterpolationGlobal: Solver.InterpolationMode
          InterpolationPerVariable: CodedMap<Solver.InterpolationMode>
          Random: Random }
