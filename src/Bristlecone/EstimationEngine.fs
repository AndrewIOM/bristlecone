namespace Bristlecone

open Bristlecone.Time

/// Represents an ordinary differential equation model system and
/// its likelihood as as objective function that may be optimised.
module ModelSystem =

    type Environment<'data> = CodedMap<'data>

    // Time is defined as either:
    // - Integration / internal model timesteps
    // - External ('real') time (radiocarbon, date-time etc.)
    // Here, we will define time as a float
    type ModelTime = float<``time index``>

    /// An ordinary differential equation that may require fixed or free parameters,
    /// the current time t, the current response value, and / or external environmental time series.
    type ModelEquation<'data> = Parameter.Pool -> ModelTime -> 'data -> Environment<'data> -> 'data

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

    type ModelSystem<'data> =
        { Parameters: Parameter.Pool
          Equations: CodedMap<ModelEquation<'data>>
          Measures: CodedMap<Measurement<'data>>
          NegLogLikelihood: LikelihoodFn<'data> }

    type FitValue = { Fit: float; Obs: float }
    type FitSeries<'date, 'timeunit, 'timespan> = TimeSeries<FitValue, 'date, 'timeunit, 'timespan>

    /// An estimated model fit for a time-series model.
    type EstimationResult<'date, 'timeunit, 'timespan> =
        { ResultId: System.Guid
          Likelihood: float
          Parameters: Parameter.Pool
          Series: CodedMap<FitSeries<'date, 'timeunit, 'timespan>>
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

    type State = float

    type ODE = float<``time index``> -> State -> Environment -> State

    type WriteOut = LogEvent -> unit

    type Integrate<'data, 'date, 'timeunit, 'timespan> =
        WriteOut
            -> float<``time index``>
            -> float<``time index``>
            -> float<``time index``>
            -> CodedMap<'data>
            -> CodedMap<TimeIndex.TimeIndex<'data, 'date, 'timeunit, 'timespan>>
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

    type TimeMode<'data, 'date, 'timeunit, 'timespan> =
        | Discrete
        | Continuous of Integrate<'data, 'date, 'timeunit, 'timespan>

    type EstimationEngine<'data, 'date, 'timeunit, 'timespan> =
        { TimeHandling: TimeMode<'data, 'date, 'timeunit, 'timespan>
          OptimiseWith: Optimiser<'data>
          Conditioning: Conditioning<'data>
          LogTo: WriteOut
          Random: Random }
