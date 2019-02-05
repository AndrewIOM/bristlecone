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

    // Measures
    // Given previous times, and starting conditions, generate the next time step

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

    type EstimationResult = {
        Likelihood: float
        Parameters: ParameterPool
        Series:     CodedMap<PredictedSeries>
        Trace:      (float * float []) list }


module EstimationEngine =

    open ModelSystem
    open Bristlecone.Optimisation
    open Bristlecone.Logging

    type Time = float
    type State = float
    type ODE = Time -> State -> Environment -> State

    type WriteOut = LogEvent -> unit

    type Integrate<'data,'time> = WriteOut -> 'time -> 'time -> 'time -> CodedMap<'data> -> CodedMap<('time*'data)[]> -> CodedMap<ODE> -> CodedMap<'data[]>
    type Optimise<'data> = WriteOut -> EndCondition<'data> -> Domain -> ('data[] -> 'data) -> Solution<'data> list

    type TimeMode<'data, 'time> =
    | Discrete
    | Continuous of Integrate<'data, 'time>

    // type EndCondition =
    // | Iterations of int

    type EstimationEngine<'data, 'time> = {
        TimeHandling: TimeMode<'data,'time>
        OptimiseWith: Optimise<'data>
        Conditioning: Conditioning
        Constrain: ConstraintMode
        LogTo: WriteOut
    }