namespace Bristlecone

/// Configures a single function that represents a model and its likelihood
/// when fit to time-series data.
[<RequireQualifiedAccess>]
module Objective =

    open ModelSystem
    open Bristlecone.EstimationEngine
    open Bristlecone.Numerics.Typed

    let accessorFromRealVector
        (compiled: Parameter.Pool.CompiledTransforms<'S,'V,'space>)
        (thetaReal: TypedVector<'S,'V,``parameter``>)
        : ModelSystem.ParameterValueAccessor<'S> =
        let idx = compiled.IndexByName

        ParameterValueAccessor(fun name ->
            match Map.tryFind name idx with
            | Some i -> Vector.itemAt i thetaReal
            | None -> invalidOp $"Parameter '{name}' not found")

    let prependInitialConditions initial expected =
        expected
        |> Map.map (fun k v ->
            let i = initial |> Map.find k
            Vector.prepend i v)

    /// Compute the system's `Measures` from the dynamic variables produced by the solver.
    /// All operations happen in Tensor-space. Initial conditions (t0) are added to the front
    /// of the predictions to enable previous value lookup where needed.
    let measure
        (measures: CodedMap<Measurement<'S,'V,state>>)
        (parameters: TypedVector<'S,'V,``parameter``>)
        (expectedDynamic: CodedMap<TypedVector<'S,'V,state>>)
        (initialConditions: CodedMap<TypedScalar<'S,state>>)
        : CodedMap<TypedVector<'S,'V,state>> =

        let expectedWithT0 = prependInitialConditions initialConditions expectedDynamic
        let length = expectedWithT0 |> Seq.head |> fun kv -> kv.Value |> Vector.length

        let measuredSeries =
            measures
            |> Map.map (fun measKey measFn ->
                let buf = ResizeArray()

                let initialThis =
                    initialConditions |> Map.tryFind measKey |> Option.defaultValue invalidTensor

                for i = 1 to length - 1 do
                    let thisVal = if i = 1 then initialThis else buf.[i - 2]
                    let value = measFn parameters expectedWithT0 thisVal i
                    buf.Add value

                buf.ToArray() |> Numerics.Stats.stack1D)

        // Merge into dynamic series
        Map.fold (fun acc key value -> Map.add key value acc) expectedDynamic measuredSeries

    /// Pairs observed time series to predicted series for dynamic variables only.
    /// Environmental forcings and hidden variables are removed.
    let pairObservationsToExpected observed expected : CodedMap<SeriesPair<'S,'V,state>> =
        observed
        |> Map.filter (fun key _ -> expected |> Map.containsKey key)
        |> Map.map (fun key value ->
            let r =
                { Observed = value
                  Expected = expected |> Map.find key }

            if Vector.length r.Observed = Vector.length r.Expected then
                r
            else
                invalidOp (
                    sprintf
                        "The predicted series %s was a different length to the observed series (%i vs %i)"
                        key.Value
                        (Vector.length r.Observed)
                        (Vector.length r.Expected)
                ))

    let compiledFromConfig
        (config: Parameter.Pool.AnyOptimiserConfig<'S,'V>)
        : Parameter.Pool.CompiledTransforms<'S,'V,``optim-space``> =
        match config with
        | Parameter.Pool.DetachedConfig cfg -> cfg.Compiled
        | Parameter.Pool.TransformedConfig cfg -> unbox cfg.Compiled // TODO remove this unbox and coercion.

    let predict solver measures parameters =
        let dynamics, initialConditions = solver parameters
        let measured = measure measures parameters dynamics initialConditions
        Map.fold (fun acc k v -> acc |> Map.add k v) dynamics measured

    /// Computes measurement variables and appends to expected data.
    /// Requires:
    /// - A continuous-time solver (for differential-based equations)
    /// - A discrete-time solver (for if measurement / computed variables are present)
    /// - Observed data (for calculating likelihood)
    let create
        (negLogLikFn: ModelSystem.Likelihood<'S,'V,state>)
        (measures: CodedMap<ModelSystem.Measurement<'S,'V,state>>)
        (solver: Solver.ConfiguredSolver<'S,'V>)
        config
        (observed: CodedMap<TypedVector<'S,'V,state>>)
        : EstimationEngine.Objective<'S,'V> =

        let compiled = compiledFromConfig config

        fun point ->
            let thetaReal = compiled.Forward point
            let accessor = accessorFromRealVector compiled thetaReal

            thetaReal
            |> predict solver measures
            |> pairObservationsToExpected observed
            |> negLogLikFn.Evaluate accessor

    let createPredictor (measures: CodedMap<ModelSystem.Measurement<'S,'V,state>>) (solver: Solver.ConfiguredSolver<'S,'V>) config =
        let compiled = compiledFromConfig config
        compiled.Forward >> predict solver measures
