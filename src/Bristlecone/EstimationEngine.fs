namespace Bristlecone

module EnvironmentalVariables =

    type RegionalEnvironment = CodedMap<TimeSeries<float>>
    type LocalEnvironment = CodedMap<TimeSeries<float>>


module ModelSystem =

    // Models 
    type Response = float
    type Environment = CodedMap<float>
    type Time = float
    type ModelEquation = ParameterPool -> Time -> Response -> Environment -> float

    // Likelihood
    type PredictedSeries = {
        Expected: float[]
        Observed: float[] }
    type Likelihood = ParameterPool -> CodedMap<PredictedSeries> -> float

    type ModelSystem = {
        Parameters: ParameterPool
        Equations:  CodedMap<ModelEquation>
        Measures:   CodedMap<float -> Environment -> Environment -> float> //Environment(t-1) -> Environment(t) -> float
        Likelihood: Likelihood }

    type FitValue = { Fit: float; Obs: float }
    type FitSeries = TimeSeries<FitValue>
    type EstimationResult = {
        ResultId:   System.Guid
        Likelihood: float
        Parameters: ParameterPool
        Series:     CodedMap<FitSeries>
        Trace:      (float * float []) list }

/// Point is generic to allow choice of number precision
type Point<'a> = 'a []
type Solution<'a> = float * Point<'a>
type Objective<'a> = Point<'a> -> float
type EndCondition<'a> = Solution<'a> list -> bool
type Domain = (float*float*Parameter.Constraint) []

module EstimationEngine =

    open System
    open ModelSystem
    open Bristlecone.Logging
    open Bristlecone.Time.TimeIndex

    type Time = float
    type State = float
    type ODE = Time -> State -> Environment -> State

    type WriteOut = LogEvent -> unit

    type Integrate<'data,'time> = WriteOut -> 'time -> 'time -> 'time -> CodedMap<'data> -> CodedMap<TimeIndex<'data>> -> CodedMap<ODE> -> CodedMap<'data[]>
    type Optimise<'data> = Random -> WriteOut -> EndCondition<'data> -> Domain -> ('data[] -> 'data) -> Solution<'data> list

    type TimeMode<'data, 'time> =
        | Discrete
        | Continuous of Integrate<'data, 'time>

    type EstimationEngine<'data, 'time> = {
        TimeHandling: TimeMode<'data,'time>
        OptimiseWith: Optimise<'data>
        Conditioning: Conditioning<'data>
        Constrain: ConstraintMode
        LogTo: WriteOut
        Random: Random
    }
