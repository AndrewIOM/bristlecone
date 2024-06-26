namespace Bristlecone

open Bristlecone.Time

/// Represents an ordinary differential equation model system and
/// its likelihood as as objective function that may be optimised.
module ModelSystem =

    // Models
    type Response = float
    type Environment = CodedMap<float>
    type Time = float

    /// An ordinary differential equation that may require fixed or free parameters,
    /// the current time t, the current response value, and / or external environmental time series.
    type ModelEquation = Parameter.Pool -> Time -> Response -> Environment -> float

    /// Paired time-series representing the true and modelled time-series.
    type PredictedSeries =
        { Expected: float[]; Observed: float[] }

    /// A function that returns a parameter's current value by its name.
    type ParameterValueAccessor =
        | ParameterValueAccessor of (string -> float)

        member this.Get name =
            let (ParameterValueAccessor v) = this in v name

    /// A function that computes the likelihood of a set of parameters.
    type LikelihoodFn = ParameterValueAccessor -> CodedMap<PredictedSeries> -> float

    /// A function that computes a measured system property given a
    /// current (time t) and previous (time t-1) system state.
    type MeasureEquation = float -> Environment -> Environment -> float

    type ModelSystem =
        { Parameters: Parameter.Pool
          Equations: CodedMap<ModelEquation>
          Measures: CodedMap<MeasureEquation>
          NegLogLikelihood: LikelihoodFn }

    type FitValue = { Fit: float; Obs: float }
    type FitSeries = TimeSeries<FitValue>

    /// An estimated model fit for a time-series model.
    type EstimationResult =
        { ResultId: System.Guid
          Likelihood: float
          Parameters: Parameter.Pool
          Series: CodedMap<FitSeries>
          Trace: (float * float[]) list
          InternalDynamics: CodedMap<float[]> option }

module EstimationEngine =

    open System
    open Bristlecone.Logging
    open Bristlecone.Conditioning
    open ModelSystem

    /// Point is generic to allow choice of number precision
    type Point<'a> = 'a[]
    type Solution<'a> = float * Point<'a>
    type Objective<'a> = Point<'a> -> float
    /// Determines if the end has been reached based on a list
    /// of tupled Solutions with their iteration number.
    type EndCondition<'a> = (Solution<'a>) list -> int -> bool
    type Domain = (float * float * Parameter.Constraint)[]

    type Time = float
    type State = float
    type ODE = Time -> State -> Environment -> State

    type WriteOut = LogEvent -> unit

    type Integrate<'data, 'time> =
        WriteOut
            -> 'time
            -> 'time
            -> 'time
            -> CodedMap<'data>
            -> CodedMap<TimeIndex.TimeIndex<'data>>
            -> CodedMap<ODE>
            -> CodedMap<'data[]>

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

    type TimeMode<'data, 'time> =
        | Discrete
        | Continuous of Integrate<'data, 'time>

    type EstimationEngine<'data, 'time> =
        { TimeHandling: TimeMode<'data, 'time>
          OptimiseWith: Optimiser<'data>
          Conditioning: Conditioning<'data>
          LogTo: WriteOut
          Random: Random }
