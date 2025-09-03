namespace Bristlecone

/// Configures a single function that represents a model and its likelihood
/// when fit to time-series data.
[<RequireQualifiedAccess>]
module Objective =

    open ModelSystem
    open Bristlecone.EstimationEngine
    open Bristlecone.Tensors

    let accessorFromRealVector
        (compiled: Parameter.Pool.CompiledTransforms)
        (thetaReal: TypedTensor<Vector,``parameter``>)
        : ModelSystem.ParameterValueAccessor =
        let idx = compiled.IndexByName
        ParameterValueAccessor (fun name ->
            match Map.tryFind name idx with
            | Some i -> asScalar<``parameter``> thetaReal.Value.[i]
            | None   -> invalidOp $"Parameter '{name}' not found")

    /// The system's `Measures` are computed from the product of the solver.
    let measure (measures: CodedMap<ModelSystem.Measurement<'u>>) solveDiscrete (expectedDynamic: CodedMap<TypedTensor<Vector,state>>) : CodedMap<TypedTensor<Vector,state>> =
        measures
        |> Map.map (fun key measure -> solveDiscrete key measure expectedDynamic)
        |> Map.fold (fun acc key value -> Map.add key value acc) expectedDynamic

    /// Pairs observed time series to predicted series for dynamic variables only.
    /// Environmental forcings and hidden variables are removed.
    let pairObservationsToExpected observed expected : CodedMap<SeriesPair<state>> =
        observed
        |> Map.filter (fun key _ -> expected |> Map.containsKey key)
        |> Map.map (fun key value ->
            let r =
                { Observed = value
                  Expected = expected |> Map.find key }
            if r.Observed.Value.shape = r.Expected.Value.shape then r
            else invalidOp (sprintf "The predicted series %s was a different length to the observed series" key.Value))

    let predict solveDifferential solveDiscrete measures p =
        p
        |> solveDifferential
        |> measure measures solveDiscrete

    /// Computes measurement variables and appends to expected data.
    /// Requires:
    /// - A continuous-time solver (for differential-based equations)
    /// - A discrete-time solver (for if measurement / computed variables are present)
    /// - Observed data (for calculating likelihood)
    let create
        (system: ModelSystem<state, 'timeIndex>)
        (solveDifferential: Solver.ConfiguredSolver)
        solveDiscrete
        parameterPool
        (observed: CodedMap<float[]>) : EstimationEngine.Objective =
            
            let compiled = Parameter.Pool.compileTransforms parameterPool
            let observedTensors =
                observed
                |> Map.map (fun _ arr ->
                    arr |> Array.map ((*) 1.<state>) |> Tensors.Typed.ofVector)
            
            fun point ->
                let thetaReal = compiled.Forward point
                let accessor  = accessorFromRealVector compiled thetaReal
                thetaReal
                |> predict solveDifferential solveDiscrete system.Measures
                |> pairObservationsToExpected observedTensors
                |> system.NegLogLikelihood accessor
