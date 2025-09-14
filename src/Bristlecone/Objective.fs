namespace Bristlecone

/// Configures a single function that represents a model and its likelihood
/// when fit to time-series data.
[<RequireQualifiedAccess>]
module Objective =

    open ModelSystem
    open Bristlecone.EstimationEngine
    open Bristlecone.Tensors

    let accessorFromRealVector
        (compiled: Parameter.Pool.CompiledTransforms<'space>)
        (thetaReal: TypedTensor<Vector,``parameter``>)
        : ModelSystem.ParameterValueAccessor =
        let idx = compiled.IndexByName
        ParameterValueAccessor (fun name ->
            match Map.tryFind name idx with
            | Some i -> asScalar<``parameter``> thetaReal.Value.[i]
            | None   -> invalidOp $"Parameter '{name}' not found")

    /// Compute the system's `Measures` from the dynamic variables produced by the solver.
    /// All operations happen in Tensor-space.
    let measure
        (measures: CodedMap<Measurement<state>>)
        (parameters: TypedTensor<Vector,``parameter``>)
        (expectedDynamic: CodedMap<TypedTensor<Vector,state>>)
        : CodedMap<TypedTensor<Vector,state>> =

        let length = expectedDynamic |> Seq.head |> fun kv -> kv.Value |> Typed.length

        let measuredSeries =
            measures
            |> Map.map (fun _ measFn ->
                let buf = ResizeArray()
                for i = 0 to length - 1 do
                    let value = measFn parameters expectedDynamic i
                    buf.Add value
                buf.ToArray() |> Typed.stack1D)

        // Merge into dynamic series
        Map.fold (fun acc key value -> Map.add key value acc) expectedDynamic measuredSeries

    /// Pairs observed time series to predicted series for dynamic variables only.
    /// Environmental forcings and hidden variables are removed.
    let pairObservationsToExpected observed expected : CodedMap<SeriesPair<state>> =
        observed
        |> Map.filter (fun key _ -> expected |> Map.containsKey key)
        |> Map.map (fun key value ->
            let r =
                { Observed = value
                  Expected = expected |> Map.find key }
            if Typed.length r.Observed = Typed.length r.Expected then r
            else invalidOp (sprintf "The predicted series %s was a different length to the observed series (%i vs %i)" key.Value (Typed.length r.Observed) (Typed.length r.Expected) ))

    let compiledFromConfig (config:Parameter.Pool.AnyOptimiserConfig) : Parameter.Pool.CompiledTransforms<``optim-space``> =
        match config with
        | Parameter.Pool.DetachedConfig cfg    -> cfg.Compiled
        | Parameter.Pool.TransformedConfig cfg -> unbox cfg.Compiled // TODO remove this unbox and coercion.

    let predict solver measures parameters =
        let dynamics = solver parameters
        let measured = measure measures parameters dynamics
        Map.fold (fun acc k v -> acc |> Map.add k v) dynamics measured

    /// Computes measurement variables and appends to expected data.
    /// Requires:
    /// - A continuous-time solver (for differential-based equations)
    /// - A discrete-time solver (for if measurement / computed variables are present)
    /// - Observed data (for calculating likelihood)
    let create
        (negLogLikFn: ModelSystem.Likelihood<state>)
        (measures: CodedMap<ModelSystem.Measurement<state>>)
        (solver: Solver.ConfiguredSolver)
        config
        (observed: CodedMap<float<'state>[]>) : EstimationEngine.Objective =

            let compiled = compiledFromConfig config
            let observedTensors =
                observed
                |> Map.map (fun _ arr ->
                    arr |> Array.map (Units.removeUnitFromFloat >> (*) 1.<state>) |> Tensors.Typed.ofVector)
            
            fun point ->
                let thetaReal = compiled.Forward point
                let accessor  = accessorFromRealVector compiled thetaReal
                thetaReal
                |> predict solver measures
                |> pairObservationsToExpected observedTensors
                |> negLogLikFn accessor

    let createPredictor (measures: CodedMap<ModelSystem.Measurement<state>>) (solver: Solver.ConfiguredSolver) config =
            let compiled = compiledFromConfig config            
            compiled.Forward >> predict solver measures
