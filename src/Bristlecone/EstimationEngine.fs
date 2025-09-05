namespace Bristlecone

open Bristlecone.Time
open Bristlecone.Tensors

/// Represents an ordinary differential equation model system and
/// its likelihood as an objective function that may be optimised.
module ModelSystem =

    [<Measure>] type ``environment``
    [<Measure>] type ``state``

    /// Model system that works with float-based
    /// data values and equations.
    module FloatBased =

        type Environment<[<Measure>] 'u> = CodedMap<float<'u>>

        type ModelEquationF<[<Measure>] 'u> =
            Parameter.Pool.ParameterPool -> float<``time index``> -> float<'u> -> Environment<'u> -> float<'u>

        type MeasurementF<[<Measure>] 'u> =
            float<'u> -> Environment<'u> -> Environment<'u> -> float<'u>

        type PredictedSeriesF = float[]

        type ParameterValueAccessorF = string -> float

        type LikelihoodF<[<Measure>] 'u> =
            ParameterValueAccessorF -> CodedMap<PredictedSeriesF> -> float<'u>

        type ModelSystemF<[<Measure>] 'u> =
            {   Parameters       : Parameter.Pool.ParameterPool
                Equations        : CodedMap<ModelEquationF<'u>>
                Measures         : CodedMap<MeasurementF<'u>>
                NegLogLikelihood : LikelihoodF<'u> }


    /// The external environment at any time t.
    type ExternalEnvironment = CodedMap<TypedTensor<Scalar,environment>>

    /// An equation that may require fixed or free parameters,
    /// the current time t, the current response value, and / or
    /// external environmental time series.
    /// - TODO Maybe allow the state be any unit (e.g. grams),
    ///   or maybe it can be at a higher (DSL) level.
    type GenericModelEquation<[<Measure>] 'timeUnit> =
        TypedTensor<Vector,``parameter``> ->
            CodedMap<TypedTensor<Scalar,``environment``>> ->
            TypedTensor<Scalar,'timeUnit> ->
            TypedTensor<Scalar,state> ->
            TypedTensor<Scalar,state>

    /// The model system may make use of either stepped or integrated-time
    /// equations. Each are both require the same inputs, but differ in
    /// whether a solver is applied or not. Here, the DU's purpose is for
    /// additional type safety.
    type ModelForm<[<Measure>] 'timeUnit> =
        | DifferenceEqs   of CodedMap<GenericModelEquation<'timeUnit>>
        | DifferentialEqs of CodedMap<GenericModelEquation<'timeUnit>>

    /// Predicted time-series for a single variable.
    /// 'u is the state unit of measure.
    type PredictedSeries<[<Measure>] 'u> = TypedTensor<Vector,'u>
    // type PredictedSeries<'data,'date,'timeunit,'timespan> =
    //     TimeSeries.TimeSeries<ModelFitToPoint<'data>, 'date, 'timeunit, 'timespan>

    /// A function that returns a parameter's current value by its name.
    type ParameterValueAccessor =
        | ParameterValueAccessor of (string -> TypedTensor<Scalar,``parameter``>)

        member this.Get name =
            let (ParameterValueAccessor v) = this in v name

    /// A single variable’s observed vs expected values
    /// after model-fitting.
    type SeriesPair<[<Measure>] 'u> =
        { Observed: TypedTensor<Vector,'u>
          Expected: TypedTensor<Vector,'u> }

    /// The negative log likelihood given the predicted and observed
    /// per-variable time-series and a function to retrieve parameters
    /// required by the likelihood function.
    type Likelihood<[<Measure>] 'u> =
        ParameterValueAccessor
            -> CodedMap<SeriesPair<'u>>
            -> TypedTensor<Scalar,``-logL``>

    /// A function that computes a measured system property given a
    /// current (time t) and previous (time t-1) system state.
    type Measurement<[<Measure>] 'u> =
        TypedTensor<Scalar,parameter>
            -> ExternalEnvironment
            -> ExternalEnvironment
            -> TypedTensor<Scalar,'u>

    /// TODO: Probably don't need 'dataUnit here, or revised;
    /// as the data unit will vary from eq to eq.
    type ModelSystem<[<Measure>] 'dataUnit, [<Measure>] 'timeUnit> =
        { Parameters       : Parameter.Pool.ParameterPool
          Equations        : ModelForm<'timeUnit>
          Measures         : CodedMap<Measurement<'dataUnit>>
          NegLogLikelihood : Likelihood<'dataUnit> }

    type FitValue = { Fit: float<state>; Obs: float }
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
    // type Solution = float<``-logL``> * float[]

    /// An objective function that can be optimised
    /// within an optimisation routine.
    type Objective = Point -> Tensors.TypedTensor<Tensors.Scalar,``-logL``>

    /// Low‑level compiled likelihood
    /// Works directly with a parameter tensor (real space).
    type CompiledLikelihood<[<Measure>] 'u> =
        TypedTensor<Vector,``parameter``>
            -> CodedMap<SeriesPair<'u>>
            -> TypedTensor<Scalar,``-logL``>

    /// Determines if the end has been reached based on a list
    /// of tupled Solutions with their iteration number.
    type EndCondition = Solution list -> int<iteration> -> bool
    
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

    and TensorODE = TypedTensor<Vector,``parameter``> -> CodedMap<TypedTensor<Scalar,``environment``>> -> TypedTensor<Scalar,``time index``> -> TypedTensor<Scalar,state> -> TypedTensor<Scalar,state>
    and FloatODE = CodedMap<float<parameter>> -> float<``time index``> -> float<state> -> ExternalEnvironment -> float<state>

    /// Represents an external logging function.
    type WriteOut = LogEvent -> unit

    /// A function that, given parameters, produces a parameterised RHS
    /// for the ODE system. This is the output of the static solver setup.
    type UnparameterisedRHS =
        TypedTensor<Vector,``parameter``>
            -> TypedTensor<Scalar,``time index``>
            -> TypedTensor<Vector,state>
            -> CodedMap<TypedTensor<Vector,state>>

    /// A parameterised RHS — parameters already bound.
    /// This is what the integration routine actually steps.
    type ParameterisedRHS =
        TypedTensor<Scalar,``time index``>
            -> TypedTensor<Vector,state>
            -> CodedMap<TypedTensor<Vector,state>>


    module Solver =

        /// 'Bakes in' environment, timeline,
        /// model keys, etc., producing an UnparameterisedRHS
        /// that only needs parameters.
        type SolverSetup<'date,'timeunit,'timespan> =
            float<``time index``> // tInitial
                -> float<``time index``> // tEnd
                -> float<``time index``> // tStep
                -> CodedMap<TypedTensor<Scalar,environment>> // initial environment
                -> CodedMap<TimeIndex.TimeIndex<TypedTensor<Scalar,environment>, 'date,'timeunit,'timespan>> // external env
                -> ModelEquations
                -> UnparameterisedRHS

        type ConfiguredSolver =
            TypedTensor<Vector,``parameter``> // parameters
                -> CodedMap<TypedTensor<Vector,state>>


    module Integration =

        /// Represents a low-level numerical method used
        /// to integrate functions. Takes the intial time,
        /// final time, and time step,
        /// the initial state, and returns a function that
        /// is compiled to only require the current time
        /// and current state.
        type IntegrationRoutine =
            TypedTensor<Scalar,``time index``> // tInitial
                -> TypedTensor<Scalar,``time index``> // tEnd
                -> TypedTensor<Scalar,``time index``> // tStep
                -> CodedMap<TypedTensor<Scalar,state>> // initialConditions
                -> ParameterisedRHS
                -> CodedMap<TypedTensor<Vector,state>>


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

    type TimeMode<'date, 'timeunit, 'timespan> =
        | Discrete
        | Continuous of Integration.IntegrationRoutine

    type EstimationEngine<'data, 'date, 'timeunit, 'timespan> =
        { TimeHandling: TimeMode<'date, 'timeunit, 'timespan>
          OptimiseWith: Optimisation.Optimiser
          Conditioning: Conditioning<'data>
          LogTo: WriteOut
          Random: Random }
