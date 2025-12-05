namespace Bristlecone

/// Helper functions for the creation of `Solver` functions, which apply time-series models
/// to time-series data (when using Bristlecone time-series types).
module Solver =

    open Bristlecone.EstimationEngine
    open Bristlecone.Time
    open Bristlecone.ModelSystem
    open Bristlecone.Tensors

    /// If environmental forcing data is supplied, the output series may be
    /// configured to be either external (i.e. on observation timeline) or
    /// internal (e.g. on environmental data timeline).
    type StepType<'date> =
        | Internal
        | External of 'date list

    /// Some runners return a paired set of time-index * scalars whereas others
    /// return a tuple of a list of time-index and vectors for each map item.
    type RunnerOutput =
        | Paired of CodedMap<(float<``time index``> * TypedTensor<Scalar, state>)[]>
        | Unpaired of float<``time index``> list * CodedMap<TypedTensor<Vector, state>>

    /// Masks to only return the requested values, for example when
    /// comparing with observational data.
    module Masking =

        /// Apply masking/normalisation to runner output, producing aligned vectors.
        let makeMaskOutput
            (stepType: StepType<'date>)
            (dateMode: DateMode.DateMode<'date, 'yearType, 'timespan>)
            (startDate: 'date)
            (factor: float<'modelTimeUnit / ``time index``>)
            toModelUnits
            : RunnerOutput -> CodedMap<TypedTensor<Vector, state>> =

            match stepType with
            | Internal ->
                fun output ->
                    match output with
                    | Paired vars -> vars |> Map.map (fun _ series -> series |> Array.map snd |> Typed.stack1D)
                    | Unpaired(_times, vars) -> vars

            | External obsDates ->
                let obsTimes =
                    obsDates
                    |> List.map (fun d ->
                        let diff = dateMode.Difference startDate d
                        let tModel = toModelUnits diff.RealDifference
                        tModel / factor)
                    |> Set.ofList

                fun output ->
                    match output with
                    | Paired vars ->
                        vars
                        |> Map.map (fun _ series ->
                            series
                            |> Array.filter (fun (ti, _) -> Set.contains ti obsTimes)
                            |> Array.map snd
                            |> Typed.stack1D)

                    | Unpaired(times, vars) ->
                        let keepMask =
                            times |> List.map (fun ti -> Set.contains ti obsTimes) |> List.toArray

                        vars |> Map.map (fun _ v -> Typed.filterByMask keepMask v)


    // Time conversion tailored for TypedTensor and time-index
    module internal TimeWrapping =

        /// Wrap a model equation written in 'timeUnit so it accepts time index.
        let wrapTime<[<Measure>] 'timeUnit, [<Measure>] 'returnUnit>
            (factor: float<'timeUnit / ``time index``>)
            (eq: GenericModelEquation<'timeUnit, 'returnUnit>)
            : GenericModelEquation<``time index``, 'returnUnit> =
            fun pars env tIndex state ->
                // Convert tIndex (Scalar<time-index>) -> Scalar<'timeUnit> by multiplying with factor
                let tIndexF = Typed.toFloatScalar tIndex
                let tModel = Typed.ofScalar (tIndexF * factor)
                eq pars env tModel state

        /// Wrap a model equation written in 'timeUnit so it accepts time index.
        let wrapTimeDifference<[<Measure>] 'timeUnit, [<Measure>] 'state>
            (factor: float<'timeUnit / ``time index``>)
            (eq: StateEquation<'timeUnit>)
            : StateEquation<``time index``> =
            fun pars env tIndex state ->
                let tIndexF = Typed.toFloatScalar tIndex
                let tModel = Typed.ofScalar (tIndexF * factor)
                eq pars env tModel state

        // For differential equations: return unit is 'state / 'timeUnit
        let wrapTimeDifferential<[<Measure>] 'timeUnit, [<Measure>] 'state>
            (factor: float<'timeUnit / ``time index``>)
            (eq: RateEquation<'timeUnit>)
            : RateEquation<``time index``> =
            fun pars env tIndex state ->
                let tIndexF = Typed.toFloatScalar tIndex
                let tModel = Typed.ofScalar (tIndexF * factor)
                eq pars env tModel state |> Typed.retype

        // Lift wrapping over a whole model form
        let wrapModelForm<[<Measure>] 'timeUnit>
            (factor: float<'timeUnit / ``time index``>)
            (mf: ModelForm<'timeUnit>)
            : ModelForm<``time index``> =
            match mf with
            | DifferenceEqs eqs -> eqs |> Map.map (fun _ e -> wrapTimeDifference factor e) |> DifferenceEqs
            | DifferentialEqs eqs -> eqs |> Map.map (fun _ e -> wrapTimeDifferential factor e) |> DifferentialEqs


    /// Runners return the baseline state plus every evolved state.
    module SolverRunners =

        module DiscreteTime =

            let internal stepOnce
                (eqs: CodedMap<StateEquation<``time index``>>)
                (pars: TypedTensor<Vector, ``parameter``>)
                (env: CodedMap<TypedTensor<Scalar, environment>>)
                (t: TypedTensor<Scalar, ``time index``>)
                (state: CodedMap<TypedTensor<Scalar, state>>)
                : CodedMap<TypedTensor<Scalar, state>> =
                eqs |> Map.map (fun key eq -> eq pars env t state.[key])

            let iterateDifference
                (eqs: CodedMap<StateEquation<``time index``>>)
                (timeline: float<``time index``>[])
                (envStream: CodedMap<TypedTensor<Scalar, environment>>[])
                (baselineValue: CodedMap<TypedTensor<Scalar, state>>)
                pars
                : CodedMap<(float<``time index``> * TypedTensor<Scalar, state>)[]> =
                let _, outputs =
                    ((baselineValue, eqs |> Map.map (fun _ _ -> [])), timeline |> Array.mapi (fun i ti -> i, ti))
                    ||> Array.fold (fun (state, acc) (i, tiVal) ->
                        if i = 0 then
                            // Do not advance or record at baseline
                            (state, acc)
                        else
                            // Advance once from previous state to this time, then record
                            let t = Typed.ofScalar tiVal
                            let env = envStream.[i]
                            let nextState = stepOnce eqs pars env t state
                            let acc' = acc |> Map.map (fun k vs -> (tiVal, nextState.[k]) :: vs)
                            (nextState, acc'))

                outputs |> Map.map (fun _ vs -> vs |> List.rev |> Array.ofList)

            /// The fixed runner must include the baseline date in its timeline,
            /// but only returns values for t1..tN.
            let fixedRunner
                (eqs: CodedMap<StateEquation<``time index``>>)
                (timeline: float<``time index``>[])
                (envIndex: CodedMap<TimeIndex.TimeIndex<float<environment>, _, _, _>>)
                (baselineValueFn: TypedTensor<Vector, ``parameter``> -> CodedMap<TypedTensor<Scalar, state>>)
                point
                =
                let envStream =
                    timeline
                    |> Array.map (fun ti ->
                        envIndex
                        |> Map.map (fun _ idxTI ->
                            let v = idxTI.Item ti
                            Typed.ofScalar v))

                iterateDifference eqs timeline envStream (baselineValueFn point) point |> Paired

            let variableRunner
                (eqs: CodedMap<StateEquation<``time index``>>)
                (timeline: float<``time index``>[]) // irregular observation times
                (envIndex: CodedMap<TimeIndex.TimeIndex<float<environment>, _, _, _>>)
                (t0: TypedTensor<Vector, ``parameter``> -> CodedMap<TypedTensor<Scalar, state>>)
                point
                =

                // Build environment stream aligned to each observation time
                let envStream =
                    timeline
                    |> Array.map (fun ti ->
                        envIndex
                        |> Map.map (fun _ idxTI ->
                            let v = idxTI.Item ti
                            Typed.ofScalar v))

                // Run the difference equations interval‑by‑interval
                let outputs = iterateDifference eqs timeline envStream (t0 point) point

                // Return as Unpaired (times + values)
                Unpaired(
                    timeline.[1..] |> Array.toList,
                    outputs |> Map.map (fun _ arr -> arr |> Array.map snd |> Typed.stack1D)
                )


        module DifferentialTime =

            /// Integrate an ODE model system given fixed timesteps.
            /// Timeline includes baseline, outputs exclude baseline.
            /// The initialState is that at the baseline.
            /// Expects integration routines that include the baseline.
            let fixedRunner
                eqs
                (integrator: Integration.IntegrationRoutine)
                (times: float<``time index``> array)
                (forcings: CodedMap<TimeIndex.TimeIndex<float<environment>, _, _, _>>)
                (initialStateFn: TypedTensor<Vector, ``parameter``> -> CodedMap<TypedTensor<Scalar, state>>)
                =

                let tStart, tEnd = times.[0], times.[times.Length - 1]

                let compiledRhs =
                    Integration.Base.makeCompiledFunctionForIntegration
                        tStart
                        tEnd
                        1.<``time index``>
                        forcings
                        initialStateFn
                        eqs

                let integrate =
                    integrator (Typed.ofScalar tStart) (Typed.ofScalar tEnd) (Typed.ofScalar 1.<``time index``>)

                fun parameters ->
                    let states = integrate (initialStateFn parameters) (compiledRhs parameters)
                    let statesTailed = states |> Map.map (fun _ v -> v |> Typed.tail)
                    Unpaired(times.[1..] |> Array.toList, statesTailed)

            /// Timeline includes baseline.
            /// Outputs exclude baseline time/state.
            let variableRunner
                eqs
                (integrator: Integration.IntegrationRoutine)
                (times: float<``time index``> array)
                (forcings: CodedMap<TimeIndex.TimeIndex<float<environment>, _, _, _>>)
                (initialStateFn: TypedTensor<Vector, ``parameter``> -> CodedMap<TypedTensor<Scalar, state>>)
                =
                fun parameters ->

                    let initialState = initialStateFn parameters

                    let rec loop acc currentState i =
                        if i >= times.Length then
                            List.rev acc
                        else
                            let tStart, tEnd = times.[i - 1], times.[i]
                            let step = tEnd - tStart

                            let compiledRhs =
                                Integration.Base.makeCompiledFunctionForIntegration
                                    tStart
                                    tEnd
                                    step
                                    forcings
                                    (fun _ -> currentState)
                                    eqs

                            let integrate =
                                integrator (Typed.ofScalar tStart) (Typed.ofScalar tEnd) (Typed.ofScalar step)

                            let newTrajectory = integrate currentState (compiledRhs parameters)

                            let finalState =
                                newTrajectory
                                |> Map.map (fun _ v -> v |> Tensors.Typed.itemAt (Typed.length v - 1))

                            loop (finalState :: acc) finalState (i + 1)

                    let states = loop [] initialState 1 |> List.toArray
                    let emptyAcc = initialState |> Map.map (fun _ _ -> [])

                    let vars =
                        states
                        |> Array.fold (fun acc s -> acc |> Map.map (fun k vs -> s.[k] :: vs)) emptyAcc
                        |> Map.map (fun _ scalars -> scalars |> List.rev |> Array.ofList |> Typed.stack1D)

                    Unpaired(times.[1..] |> Array.toList, vars)


    module SolverCompiler =

        /// Determine the appropriate resolution for the integration routine
        /// to run along, if using a fixed resolution.
        let internal decideIntegrationResolution dynamicSeries environment =
            match TimeFrame.resolution dynamicSeries with
            | Resolution.Fixed fRes ->
                let iRes =
                    match environment |> Option.map TimeFrame.resolution with
                    | Some(Resolution.Fixed efRes) ->
                        let dateMode = (dynamicSeries.Series |> Seq.head).Value.DateMode
                        Resolution.finestResolution dateMode.ResolutionToSpan dateMode.TotalDays fRes efRes
                    | _ -> fRes

                iRes
            | Resolution.Variable ->
                let medianTimespan =
                    dynamicSeries.Series
                    |> Seq.collect (fun ts -> ts.Value.TimeSteps)
                    |> Seq.sort
                    |> Seq.splitInto 2
                    |> Seq.skip 1
                    |> Seq.head
                    |> Seq.head

                let custom = Resolution.FixedTemporalResolution.CustomEpoch medianTimespan
                custom

        let private computeFactor toModelUnits (dateMode: DateMode.DateMode<'a, 'b, 'c>) integrationRes =
            let span = dateMode.ResolutionToSpan integrationRes
            toModelUnits span / 1.0<``time index``>

        let private interpFunction =
            function
            | Solver.Exact -> TimeIndex.IndexMode.Exact
            | Solver.Lower -> TimeIndex.IndexMode.Interpolate Statistics.Interpolate.lower
            | Solver.Linear -> TimeIndex.IndexMode.Interpolate Statistics.Interpolate.bilinear

        let private buildEnvIndex
            getInterpModeFor
            startDate
            integrationRes
            (environment: option<TimeFrame.TimeFrame<float<environment>, 'date, 'timeunit, 'timespan>>)
            : CodedMap<TimeIndex.TimeIndex<float<environment>, 'date, 'timeunit, 'timespan>> =
            environment
            |> Option.map (fun f ->
                f.Series
                |> Map.map (fun sc v ->
                    let mode = sc |> getInterpModeFor |> interpFunction
                    TimeIndex.TimeIndex(startDate, integrationRes, mode, v)))
            |> Option.defaultValue Map.empty

        /// Mandate that environmental data falls exactly on the solver's timesteps.
        /// Only required when no interpolation is specified.
        let enforceExactAlignment
            (envIndex: CodedMap<TimeIndex.TimeIndex<float<environment>, 'date, 'timeunit, 'timespan>>)
            solverTimeline
            =
            envIndex
            |> Map.iter (fun v t ->
                if not (solverTimeline |> Array.contains t) then
                    invalidOp (sprintf "Environment variable has Exact mode but no value at solver time %A" t))

        let private buildKeepMask headSeries startDate observationRes timeline =
            let observedIndices =
                headSeries
                |> TimeIndex.create startDate observationRes
                |> Seq.map fst
                |> Set.ofSeq

            timeline |> Array.map (fun idx -> Set.contains idx observedIndices)

        let private stateVariableKeys (models: ModelSystem.ModelForm<'modelTimeUnit>) =
            match models with
            | ModelForm.DifferenceEqs eqs -> eqs |> Map.keys
            | ModelForm.DifferentialEqs eqs -> eqs |> Map.keys

        /// Compile a configured solver, automatically selecting the correct runner.
        /// t0 (conditioned or otherwise) is obtained automatically from the dynamic series.
        let compile
            logTo
            (dataTimeToModelTime: 'timespan -> float<'modelTimeUnit>)
            (modelEquations: ModelForm<'modelTimeUnit>)
            engineTimeMode
            (stepType: StepType<'date>)
            (observedStates: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>)
            (observedMeasuresT0: CodedMap<TypedTensor<Scalar, state>>)
            (hiddenStatesT0: CodedMap<Tensors.TypedTensor<Tensors.Scalar, state>>)
            (hiddenStatesT0Initialisers: CodedMap<ModelSystem.Initialiser<state>>)
            (environment: TimeFrame.TimeFrame<float<environment>, 'date, 'timeunit, 'timespan> option)
            (interpolationModeFor: ShortCode.ShortCode -> Solver.InterpolationMode)
            : Solver.ConfiguredSolver =

            // 1. Initial setup; identify t0 and start date.
            let headSeries = (observedStates.Series |> Seq.head).Value
            let dateMode = headSeries.DateMode
            let startDate = observedStates.StartDate

            let observedStatesT0 =
                observedStates
                |> TimeFrame.t0
                |> Map.map (fun k v -> Typed.ofScalar v)
                |> Map.fold (fun acc k v -> Map.add k v acc) hiddenStatesT0

            let baselineObservables =
                observedMeasuresT0
                |> Map.fold (fun acc k v -> Map.add k v acc) hiddenStatesT0
                |> Map.fold (fun acc k v -> Map.add k v acc) observedStatesT0

            let states = stateVariableKeys modelEquations

            let t0 parameters =
                states
                |> Seq.fold
                    (fun acc k ->
                        let initVal =
                            match Map.tryFind k hiddenStatesT0Initialisers with
                            | Some f -> f parameters Map.empty baselineObservables
                            | None ->
                                match Map.tryFind k observedStatesT0 with
                                | Some v -> v
                                | None -> hiddenStatesT0.[k]

                        Map.add k initVal acc)
                    Map.empty

            if not hiddenStatesT0.IsEmpty || not hiddenStatesT0Initialisers.IsEmpty then
                logTo
                <| Logging.GeneralEvent(
                    sprintf
                        "Solver: hidden states are present. Static t0 values are %A; initialisers are %A"
                        baselineObservables.Keys
                        hiddenStatesT0Initialisers.Keys
                )

            // 2. Decide resolutions
            let integrationRes = decideIntegrationResolution observedStates environment

            logTo
            <| Logging.GeneralEvent(sprintf "Solver: integration resolution is %A" integrationRes)

            // 3. Compute factor and wrap equations
            let factor = computeFactor dataTimeToModelTime dateMode integrationRes
            let modelInTI = TimeWrapping.wrapModelForm factor modelEquations

            // 4. Build timeline and env index
            let dataResolution = TimeFrame.resolution observedStates

            let envIndex =
                buildEnvIndex interpolationModeFor startDate integrationRes environment

            // 4. Precompute a keep-mask for External mode
            let maskOutput =
                Masking.makeMaskOutput stepType dateMode startDate factor dataTimeToModelTime

            logTo <| Logging.GeneralEvent(sprintf "Solver: starting at date %A" startDate)

            environment
            |> Option.iter (fun s -> logTo <| Logging.GeneralEvent(sprintf "Solver: env data start = %A" s.StartDate))

            envIndex
            |> Map.iter (fun k v ->
                logTo
                <| Logging.GeneralEvent(sprintf "%A Baseline %A, values %A" k v.Baseline v.Values))

            // 5. Pick runner automatically
            let runner =
                match dataResolution with
                | Resolution.Fixed _ ->

                    let fixedTimeline =
                        headSeries
                        |> TimeIndex.create startDate integrationRes
                        |> Seq.map fst
                        |> Seq.toArray

                    logTo <| Logging.GeneralEvent(sprintf "Fixed timeline is %A" fixedTimeline)

                    match modelInTI, engineTimeMode with
                    | DifferentialEqs eqs, Continuous i ->
                        SolverRunners.DifferentialTime.fixedRunner eqs i fixedTimeline envIndex t0

                    | DifferenceEqs eqs, Discrete ->
                        SolverRunners.DiscreteTime.fixedRunner eqs fixedTimeline envIndex t0

                    | _ -> invalidOp "Mismatch between time-mode and differential/difference equation form."

                | Resolution.Variable ->

                    let obsTimes =
                        observedStates
                        |> TimeFrame.dates
                        |> Seq.map (fun d ->
                            let diff = dateMode.Difference startDate d
                            dataTimeToModelTime diff.RealDifference / factor)
                        |> Seq.toArray

                    logTo <| Logging.GeneralEvent(sprintf "Variable timeline is %A" obsTimes)

                    match modelInTI, engineTimeMode with
                    | DifferentialEqs eqs, Continuous i ->
                        SolverRunners.DifferentialTime.variableRunner eqs i obsTimes envIndex t0

                    | DifferenceEqs eqs, Discrete -> SolverRunners.DiscreteTime.variableRunner eqs obsTimes envIndex t0

                    | _ -> invalidOp "Mismatch between time-mode and differential/difference equation form."

            // 6. Return configured solver
            fun point -> point |> runner |> maskOutput, t0 point


    /// Solver conditioning enables adding synthetic initial time-points
    /// from which to solve from.
    module Conditioning =

        type Resolved<'date, 'timeunit, 'timespan> =
            { StatesHiddenForSolver: CodedMap<TypedTensor<Scalar, state>>
              StatesObservedForSolver: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>
              MeasuresForSolver: CodedMap<TypedTensor<Scalar, state>>
              ExogenousForSolver: option<TimeFrame.TimeFrame<float<environment>, 'date, 'timeunit, 'timespan>>
              ObservedForPairing: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>
              Log: string option }

        let internal t0FromFirstObs (tf: TimeFrame.TimeFrame<float<state>, _, _, _>) =
            tf.Series
            |> Map.map (fun _ ts -> ts |> TimeSeries.head |> fst |> Typed.ofScalar)

        let internal toEquationStatesOnly (equationKeys: seq<ShortCode.ShortCode>) data =
            data |> TimeFrame.filter equationKeys

        let internal ensureEnvCoverage
            (solverStart: 'date)
            (envTF: TimeFrame.TimeFrame<'T, 'date, 'timeunit, 'timespan>)
            : TimeFrame.TimeFrame<'T, 'date, 'timeunit, 'timespan> =
            let firstEnvDate = envTF.StartDate

            if firstEnvDate > solverStart then
                invalidOp (sprintf "Environment data starts at %A but solver needs %A" firstEnvDate solverStart)
            else
                envTF

        /// Convert a conditioned dynamic TimeFrame into observation arrays
        let toObservationData
            (tf: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>)
            : CodedMap<float<state>[]> =
            tf.Series
            |> Map.map (fun _ ts ->
                ts.Values
                |> Seq.map (fun v -> v |> Units.removeUnitFromFloat |> (*) 1.<state>)
                |> Seq.toArray)

        let private resolveWithConditionedT0
            (t0: CodedMap<TypedTensor<Scalar, state>>)
            (observedTF: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>)
            (envTF: option<TimeFrame.TimeFrame<float<environment>, 'date, 'timeunit, 'timespan>>)
            equationKeys
            measureKeys
            logMessage
            =

            // Error if t0 does not contain all required values.
            let t0Missing =
                Set.difference (Map.keys observedTF.Series |> Set.ofSeq) (Map.keys t0 |> Set.ofSeq)

            if not t0Missing.IsEmpty then
                failwithf "t0 value(s) were missing for: %A" t0Missing

            // Work out one step backwards
            let firstSeries = observedTF.Series |> Seq.head |> (fun kv -> kv.Value)
            let dm = firstSeries.DateMode

            let stepSpan =
                match TimeFrame.resolution observedTF with
                | Resolution.Fixed res -> dm.ResolutionToSpan res
                | Resolution.Variable -> invalidOp "Conditioning requires fixed dynamic resolution."

            let solverStartDate = dm.SubtractTime observedTF.StartDate stepSpan

            let dynamicForSolver =
                observedTF
                |> TimeFrame.prepend solverStartDate (t0 |> Map.map (fun _ v -> Typed.toFloatScalar v))

            let trimmedEnv = envTF |> Option.map (ensureEnvCoverage solverStartDate)

            let measuresT0, nonMeasures =
                t0 |> Map.partition (fun k _ -> measureKeys |> Seq.contains k)

            let hiddenT0 =
                nonMeasures |> Map.filter (fun k _ ->
                    equationKeys |> Seq.contains k &&
                    not (observedTF.Keys |> Seq.contains k))

            { StatesObservedForSolver = toEquationStatesOnly equationKeys dynamicForSolver
              StatesHiddenForSolver = hiddenT0
              MeasuresForSolver = measuresT0
              ObservedForPairing = observedTF
              ExogenousForSolver = trimmedEnv
              Log = Some logMessage }

        let resolve
            (conditioning: Conditioning.Conditioning<'stateUnit>)
            (observedTF: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>)
            (exogenousTF: option<TimeFrame.TimeFrame<float<environment>, 'date, 'timeunit, 'timespan>>)
            equationKeys
            measureKeys
            : Resolved<'date, 'timeunit, 'timespan> =

            match conditioning with
            | Conditioning.NoConditioning ->
                // Solver baseline = obs[0]; predictions (External) align to obs[1..]
                let solverStartDate = observedTF.StartDate
                let env = exogenousTF |> Option.map (ensureEnvCoverage solverStartDate)
                let t0 = t0FromFirstObs observedTF
                let trimmedDyn = TimeFrame.dropFirstObservation observedTF

                { StatesObservedForSolver = toEquationStatesOnly equationKeys observedTF
                  StatesHiddenForSolver = Map.empty // No conditioning for hidden states. They must be set using initialisers if present.
                  MeasuresForSolver = t0 |> Map.filter (fun k _ -> measureKeys |> Seq.contains k)
                  ObservedForPairing = trimmedDyn
                  ExogenousForSolver = env
                  Log = Some "No conditioning: predictions start at t1; baseline = first observation." }

            | Conditioning.Custom t0Map ->
                let t0 =
                    t0Map
                    |> Map.map (fun _ v -> v |> Units.removeUnitFromFloat |> (*) 1.<state> |> Typed.ofScalar)

                resolveWithConditionedT0
                    t0
                    observedTF
                    exogenousTF
                    equationKeys
                    measureKeys
                    "Custom conditioning: synthetic t0 one step before first observation; pairs include obs[0]."

            | Conditioning.RepeatFirstDataPoint ->
                let t0 = t0FromFirstObs observedTF

                resolveWithConditionedT0
                    t0
                    observedTF
                    exogenousTF
                    equationKeys
                    measureKeys
                    "Repeat-first conditioning: duplicated first obs one step earlier; pairs include obs[0]."
