namespace Bristlecone

open Bristlecone.Time

/// Represents an ordinary differential equation model system and
/// its likelihood as an objective function that may be optimised.
module ModelSystem =

    type Environment<'data> = CodedMap<'data>

    /// An ordinary differential equation that may require fixed or free parameters,
    /// the current time t, the current response value, and / or external environmental time series.
    type ModelEquation<'data, 'timeIndex> =
        Parameter.Pool.ParameterPool -> 'timeIndex ->
            'data -> Environment<'data> -> 'data

    /// Paired time-series representing the true and modelled time-series.
    type PredictedSeries =
        { Expected: float[]; Observed: float[] }
    // type PredictedSeries<'data,'date,'timeunit,'timespan> =
    //     TimeSeries.TimeSeries<ModelFitToPoint<'data>, 'date, 'timeunit, 'timespan>

    /// A function that returns a parameter's current value by its name.
    type ParameterValueAccessor =
        | ParameterValueAccessor of (string -> float)

        member this.Get name =
            let (ParameterValueAccessor v) = this in v name

    /// A function that computes the likelihood of a set of parameters.
    type LikelihoodFn<'data> = ParameterValueAccessor -> CodedMap<PredictedSeries> -> 'data

    /// A function that computes a measured system property given a
    /// current (time t) and previous (time t-1) system state.
    type Measurement<'data> = 'data -> Environment<'data> -> Environment<'data> -> 'data

    type ModelSystem<'data, 'timeIndex> =
        { Parameters: Parameter.Pool.ParameterPool
          Equations: CodedMap<ModelEquation<'data, 'timeIndex>>
          Measures: CodedMap<Measurement<'data>>
          NegLogLikelihood: LikelihoodFn<'data> }

    type FitValue = { Fit: float; Obs: float }
    type FitSeries<'date, 'timeunit, 'timespan> = TimeSeries<FitValue, 'date, 'timeunit, 'timespan>

    /// An estimated model fit for a time-series model.
    type EstimationResult<'date, 'timeunit, 'timespan> =
        { ResultId: System.Guid
          Likelihood: float
          Parameters: Parameter.Pool.ParameterPool
          Series: CodedMap<FitSeries<'date, 'timeunit, 'timespan>>
          Trace: (float * float[]) list
          InternalDynamics: CodedMap<float[]> option }

module EstimationEngine =

    open System
    open Bristlecone.Logging
    open Bristlecone.Conditioning
    open DiffSharp
    open Bristlecone.Tensors

    [<Measure>] type ``parameter``
    [<Measure>] type ``environment``

    type Point<'a> = 'a[]
    type Solution<'a> = float * Point<'a>

    type Objective<'Input, 'Output> = 'Input -> 'Output
    type ObjectiveWrapper =
        | FloatObj of Objective<Point<float>, float>
        | TensorObj of Objective<Tensor, Tensor>

    /// Determines if the end has been reached based on a list
    /// of tupled Solutions with their iteration number.
    type EndCondition<'a> = (Solution<'a>) list -> int -> bool
    type Domain = (float * float * Parameter.Constraint)[]

    type State = float

//ModelExpression -> Tensors.ParameterPoolTensor -> Tensors.PointTensor -> Tensor -> Tensor -> Tensor

    type WriteOut = LogEvent -> unit

    // Probably need a type that is a CodedMap<'data> or a tensor<vector> depending
    // on the selected data type.

    // fun log tInitial tEnd tStep initialConditions externalEnvironment
        // (modelMap: EstimationEngine.ModelEquations) ->
    
    // Integrator should be revised so that there is an integration compiler
    // and a compiled integration function (p:Tensor -> s:Tensor -> Tensor).
    // Timeline and timesteps should be baked in already, as they will NEVER change
    // once compiled?

    /// Takes parameter vector, the current time index,
    /// environment data, and returns a vector of the data.
    /// Time is required to be indexed on a common timeline
    /// (i.e. in 'time index' units).
    type CompiledFunctionForIntegration =
        TypedTensor<Vector,``parameter``>
            -> TypedTensor<Scalar,``time index``>
            -> TypedTensor<Vector, 1>
            -> CodedMap<TypedTensor<Vector, 1>>

    type TensorODE = TypedTensor<Vector,``parameter``> -> CodedMap<TypedTensor<Scalar,``environment``>> -> TypedTensor<Scalar,``time index``> -> TypedTensor<Scalar,1> -> TypedTensor<Scalar,1>
    type FloatODE = float<``time index``> -> State -> ModelSystem.Environment<State> -> State

    // The models will always be written as 

    type ModelEquations =
        | TensorODEs of CodedMap<TensorODE>
        | FloatODEs of CodedMap<FloatODE>

    /// Disparate data types may enter the integration domain.
    /// However, only tensor data is permitted to leave the
    /// integration domain via the compiled function.
    type Integrate<'data, 'dataEnv, 'date, 'timeunit, 'timespan> =
        WriteOut
            -> float<``time index``>
            -> float<``time index``>
            -> float<``time index``>
            -> CodedMap<'data>
            -> CodedMap<TimeIndex.TimeIndex<'dataEnv, 'date, 'timeunit, 'timespan>>
            -> ModelEquations
            -> CompiledFunctionForIntegration // Was codedmap<float[]>

    /// Represents a method used to integrate functions.
    /// Takes the intial time, final time, and time step,
    /// the initial state, and returns a function that
    /// is compiled to only require the current time
    /// and current state.
    type IntegrationRoutine =
        TypedTensor<Scalar,``time index``>
            -> TypedTensor<Scalar,``time index``>
            -> TypedTensor<Scalar,``time index``>
            -> CodedMap<TypedTensor<Scalar,environment>>
            -> RightHandSide
            -> IntegrationFn

    // 'TypedTensor<Scalar,time index> -> 
        // TypedTensor<Scalar,time index> -> 
        // TypedTensor<Scalar,time index> -> 
        // Tensor -> 
        // (Tensor -> Tensor -> Tensor) 
        // -> Tensor'    


    and RightHandSide =
        TypedTensor<Scalar,``time index``>
            -> TypedTensor<Vector,environment>
            -> TypedTensor<Vector,1>

    and IntegrationFn =
        TypedTensor<Scalar,``time index``>
            -> TypedTensor<Vector, 1>
            -> array<TypedTensor<Vector, 1>>


    type Optimise<'data> =
        Random
            -> WriteOut
            -> EndCondition<'data>
            -> Domain
            -> Point<'data> option
            -> ('data[] -> 'data)
            -> Solution<'data> list

    /// An `Optimiser` is an optimisation algorithm that may work either
    /// in 'transformed' parameter space (where parameter constraints are
    /// automatically handled) or in 'detatched' space (where the optimisation
    /// algorithm is responsible for respecting parameter constraints).
    type Optimiser<'data> =
        | InTransformedSpace of Optimise<'data>
        | InDetachedSpace of Optimise<'data>

    type TimeMode<'data, 'date, 'timeunit, 'timespan> =
        | Discrete
        | Continuous of Integrate<'data, 'date, 'timeunit, 'timespan>

    type EstimationEngine<'data, 'date, 'timeunit, 'timespan> =
        { TimeHandling: TimeMode<'data, 'date, 'timeunit, 'timespan>
          OptimiseWith: Optimiser<'data>
          Conditioning: Conditioning<'data>
          LogTo: WriteOut
          Random: Random }
