namespace Bristlecone

/// Helper functions for the creation of `Solver` functions, which apply time-series models
/// to time-series data (when using Bristlecone time-series types).
module Solver =

    open Bristlecone.EstimationEngine
    open Bristlecone.Logging
    open Bristlecone.Time
    open Bristlecone.ModelSystem

    /// If environmental forcing data is supplied, the output series may be
    /// configured to be either external (i.e. on observation timeline) or
    /// internal (e.g. on environmental data timeline).
    type StepType =
        | Internal
        | External

    /// Step the solver using high resolution, and output at low resolution.
    /// External steps are of a fixed width.
    let fixedStep (model:CodedMap<TensorODE>) timeHandling tStart tEnd initialState forcings =
        match timeHandling with
        | Discrete -> invalidOp "Not configured"
        | Continuous i ->
            let compiledRhs = Integration.Base.makeCompiledFunctionForIntegration tStart tEnd 1.<``time index``> initialState forcings model
            let integrate = i tStart tEnd 1.<``time index``> initialState
            fun parameters ->
                integrate (compiledRhs parameters)
                |> Map.map (fun _ v -> v |> Tensors.Typed.tail)

    /// Step the solver using the high resolution, and output at low resolution.
    /// External steps can be variable in size.
    /// Each time jump is integrated individually.
    let variableExternalStep model timeHandling timeSteps (initialState: CodedMap<Tensors.TypedTensor<Tensors.Vector,state>>)
        : Solver.ConfiguredSolver =
        match timeHandling with
        | Discrete -> invalidOp "Not configured"
        | Continuous i ->
            fun parameters ->
                let results =
                    timeSteps
                    |> Seq.pairwise
                    |> Seq.scan
                        (fun state (tStart, tEnd) ->
                            let integrate = i tStart tEnd (tEnd - tStart) state
                            let rhs = Integration.Base.makeCompiledFunctionForIntegration tStart tEnd 1.<``time index``> state Map.empty model
                            integrate (rhs parameters) |> Map.map (fun _ v -> v.[0])) initialState

                eqs
                |> Map.toSeq
                |> Seq.map (fun (key, _) -> key, results |> Seq.map (fun r -> r.[key]) |> Seq.toArray)
                |> Map.ofSeq

    let fixedResolutionSolver
        models
        fRes
        stepType
        (dynamicSeries: TimeFrame.TimeFrame<'T, 'date, 'timeunit, 'timespan>)
        (environment: TimeFrame.TimeFrame<'T, 'date, 'timeunit, 'timespan> option)
        logTo
        timeHandling
        t0 : EstimationEngine.Solver.ConfiguredSolver
        =
        let timeline, forcings =
            match environment with
            | Some f ->
                match f |> TimeFrame.resolution with
                | Resolution.Variable -> failwith "Variable-time environmental forcing data is not supported."
                | Resolution.Fixed efRes ->
                    logTo
                    <| DebugEvent "Solving along the timeline of supplied environmental forcing data."

                    let timeline =
                        (dynamicSeries.Series |> Seq.head).Value
                        |> TimeIndex.create dynamicSeries.StartDate efRes
                        |> Seq.map fst

                    let envIndex =
                        f.Series
                        |> Map.map (fun k v ->
                            TimeIndex.TimeIndex(
                                dynamicSeries.StartDate,
                                efRes,
                                TimeIndex.IndexMode.Interpolate Statistics.Interpolate.lower,
                                v
                            )) //TimeIndex.IndexMode.Interpolate Statistics.Interpolate.bilinear, v))

                    (timeline, envIndex)
            | None ->
                logTo
                <| DebugEvent "No environmental forcing data was supplied. Solving using time points of observations."

                let timeline =
                    (dynamicSeries.Series |> Seq.head).Value
                    |> TimeIndex.create dynamicSeries.StartDate fRes
                    |> Seq.map fst

                (timeline, Map.empty)

        // Parameters for the integration routine
        // Internal step size is 1, given that the time-index is scaled to the steps of the high-resolution external data
        let startIndex = timeline |> Seq.head
        let endIndex = timeline |> Seq.last

        let externalSteps =
            timeline
            |> Seq.pairwise
            |> Seq.map (fun (a, b) -> b - a)
            |> Seq.distinct
            |> Seq.toList

        if externalSteps.Length <> 1 then
            failwithf "Encountered uneven timesteps: %A" externalSteps

        let solve =
            fixedStep models timeHandling (startIndex - externalSteps.Head) endIndex t0 forcings

        fun point ->
            match stepType with
            | Internal -> solve point
            | External ->
                // Filter the results so that only results that match low-res data in time are included
                solve point
                |> Map.map (fun _ v -> v |> Seq.everyNth (int externalSteps.Head) |> Seq.toArray) //TODO proper lookup


    /// Create a solver that applies time-series models to time-series data.
    /// Takes a `TimeFrame` of dynamic time-series.
    /// The solver should take a parameterised model equation set and compute
    /// the answer.
    let makeSolver
        logTo
        timeHandling
        modelEquations
        stepType
        (dynamicSeries: TimeFrame.TimeFrame<'T, 'date, 'timeunit, 'timespan>)
        (environment: TimeFrame.TimeFrame<'T, 'date, 'timeunit, 'timespan> option)
        t0
        : Solver.ConfiguredSolver =
        match dynamicSeries |> TimeFrame.resolution with
        | Resolution.Fixed fRes ->
            logTo
            <| DebugEvent(sprintf "Observations occur on a fixed temporal resolution: %A." fRes)

            fixedResolutionSolver modelEquations fRes stepType dynamicSeries environment logTo timeHandling t0
        | Resolution.Variable ->
            match environment with
            | Some f -> failwith "Variable time solver with environmental data is not yet supported."
            | None ->
                logTo
                <| DebugEvent "No environmental forcing data was supplied. Solving using time points of observations."

                logTo <| DebugEvent "Solving over time-series with uneven time steps."

                let medianTimespan =
                    dynamicSeries.Series
                    |> Seq.collect (fun ts -> ts.Value.TimeSteps)
                    |> Seq.sort
                    |> Seq.splitInto 2
                    |> Seq.skip 1
                    |> Seq.head
                    |> Seq.head

                logTo
                <| DebugEvent(
                    sprintf "Setting temporal resolution of solver as the median timestep (%A)." medianTimespan
                )

                let startDate = (dynamicSeries.Series |> Seq.head).Value.StartDate |> snd

                let timeIndex =
                    TimeIndex.TimeIndex(
                        startDate,
                        Resolution.FixedTemporalResolution.CustomEpoch medianTimespan,
                        TimeIndex.IndexMode.Exact, // TODO interpolate?
                        (dynamicSeries.Series |> Seq.head).Value
                    )

                variableExternalStep modelEquations timeHandling timeIndex.Index t0


    module Discrete =

        /// Finds the solution of a discretised model system, given time-series data.
        /// `startPoint` - a preconditioned point to represent time-zero.
        let solve startPoint : ShortCode.ShortCode -> ModelSystem.Measurement<environment> -> CodedMap<'a[]> -> 'a[] =
            fun (c: ShortCode.ShortCode) (m: ModelSystem.Measurement<environment>) (expected: CodedMap<'a[]>) ->
                expected
                |> Map.toList
                |> List.map (fun (c, d) -> d |> Array.map (fun x -> (c, x)) |> Seq.toList) // Add shortcode to each time point's value
                |> List.flip // Flip list so that it is primarily timepoint-indexed
                |> List.map Map.ofList // Make environment at each time into a CodedMap<float>
                // Scan through the environments, outputting the measure at each time
                |> List.scan
                    (fun (previousEnv, previousX) currentEnv -> (currentEnv, m previousX previousEnv currentEnv))
                    (startPoint, startPoint.Item c)
                |> List.tail // Remove conditioned point (time zero)
                |> List.map snd
                |> List.toArray

    /// Conditioning of time-series data, which allows for maximum use of observed time-series data.
    module Conditioning =

        open Bristlecone.Time

        /// Strategy for assigning a start time - `t0` - to a time series.
        let startPoint conditioning (series: CodedMap<TimeSeries<'T, 'date, 'timeunit, 'timespan>>) =
            match conditioning with
            | Conditioning.NoConditioning -> None
            | Conditioning.RepeatFirstDataPoint -> series |> Map.map (fun _ v -> v.Values |> Seq.head) |> Some
            | Conditioning.Custom precomputed -> precomputed |> Some
