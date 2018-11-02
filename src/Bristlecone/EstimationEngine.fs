namespace Bristlecone

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
        Equations: CodedMap<ModelEquation>
        Likelihood: Likelihood }

    type EstimationResult = {
        Likelihood: float
        Parameters: ParameterPool
        Series: CodedMap<PredictedSeries>
        Trace: (float * float []) list }


module EstimationEngine =

    open ModelSystem
    open Bristlecone.Optimisation

    type Time = float
    type State = float
    type ODE = Time -> State -> Environment -> State

    type Integrate<'data,'time> = 'time -> 'time -> 'time -> CodedMap<'data> -> CodedMap<ODE> -> CodedMap<'data[]>
    type Optimise<'data> = int -> Domain -> ('data[] -> 'data) -> ('data * 'data []) list

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
        OnError : (string*exn) -> unit
    }