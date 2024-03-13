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
    type PredictedSeries = {
        Expected: float[]
        Observed: float[]
    }

    /// A function that computes the likelihood of a set of parameters.
    type Likelihood = Parameter.Pool -> CodedMap<PredictedSeries> -> float

    /// A function that computes a measured system property given a
    /// current (time t) and previous (time t-1) system state.
    type MeasureEquation = float -> Environment -> Environment -> float

    /// TODO Constrain model system so that codes cannot be duplicated
    type ModelSystem = {
        Parameters: Parameter.Pool
        Equations:  CodedMap<ModelEquation>
        Measures:   CodedMap<MeasureEquation>
        Likelihood: Likelihood }

    type FitValue = { Fit: float; Obs: float }
    type FitSeries = TimeSeries<FitValue>
    type EstimationResult = {
        ResultId:   System.Guid
        Likelihood: float
        Parameters: Parameter.Pool
        Series:     CodedMap<FitSeries>
        Trace:      (float * float []) list
        InternalDynamics: CodedMap<float[]> option }

module EstimationEngine =

    open System
    open Bristlecone.Logging
    open Bristlecone.Conditioning
    open ModelSystem

    /// Point is generic to allow choice of number precision
    type Point<'a> = 'a []
    type Solution<'a> = float * Point<'a>
    type Objective<'a> = Point<'a> -> float
    /// Determines if the end has been reached based on a list
    /// of tupled Solutions with their iteration number.
    type EndCondition<'a> = (Solution<'a>) list -> int -> bool
    type Domain = (float*float*Parameter.Constraint) []

    type Time = float
    type State = float
    type ODE = Time -> State -> Environment -> State

    type WriteOut = LogEvent -> unit

    type Integrate<'data,'time> = WriteOut -> 'time -> 'time -> 'time -> CodedMap<'data> -> CodedMap<TimeIndex.TimeIndex<'data>> -> CodedMap<ODE> -> CodedMap<'data[]>
    type Optimise<'data> = Random -> WriteOut -> EndCondition<'data> -> Domain -> ('data[] -> 'data) -> Solution<'data> list

    type TimeMode<'data, 'time> =
        | Discrete
        | Continuous of Integrate<'data, 'time>

    type EstimationEngine<'data, 'time> = {
        TimeHandling: TimeMode<'data,'time>
        OptimiseWith: Optimise<'data>
        Conditioning: Conditioning<'data>
        Constrain: Parameter.ConstraintMode
        LogTo: WriteOut
        Random: Random
    }
