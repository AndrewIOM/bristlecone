namespace Bristlecone

/// Configures a single function that represents a model and its likelihood
/// when fit to time-series data.
[<RequireQualifiedAccess>]
module Objective =

    open ModelSystem
    open Bristlecone.EstimationEngine

    let parameteriseModel parameterPool point (model: ModelEquation<'data>) =
        model (point |> Parameter.Pool.fromPointInTransformedSpace parameterPool)

    /// Pairs observed time series to predicted series for dynamic variables only.
    /// Environmental forcings and hidden variables are removed.
    let pairObservationsToExpected
        (observed: CodedMap<float[]>)
        (expected: CodedMap<float[]>)
        : CodedMap<PredictedSeries> =
        observed
        |> Map.filter (fun key _ -> expected |> Map.containsKey key)
        |> Map.map (fun key value ->
            let r =
                { Observed = value
                  Expected = expected |> Map.find key }

            if r.Observed.Length = r.Expected.Length then
                r
            else
                invalidOp (sprintf "The predicted series %s was a different length to the observed series" key.Value))

    /// The system's `Measures` are computed from the product of the solver.
    let measure (system: ModelSystem<'data>) solveDiscrete (expected: CodedMap<float[]>) : CodedMap<float[]> =
        system.Measures
        |> Map.map (fun key measure -> solveDiscrete key measure expected)
        |> Map.fold (fun acc key value -> Map.add key value acc) expected

    let predict (system: ModelSystem<'data>) integrate solveDiscrete (p: Point<float>) =
        system.Equations
        |> Map.map (fun _ v -> parameteriseModel system.Parameters p v)
        |> integrate
        |> measure system solveDiscrete

    /// Computes measurement variables and appends to expected data
    let create (system: ModelSystem<'data>) integrate solveDiscrete (observed: CodedMap<float[]>) =
        fun point ->
            point
            |> predict system integrate solveDiscrete
            |> pairObservationsToExpected observed
            |> system.NegLogLikelihood(
                let pool = point |> Parameter.Pool.fromPointInTransformedSpace system.Parameters

                ParameterValueAccessor
                <| fun name -> Parameter.Pool.tryGetRealValue name pool |> Option.get
            )
