namespace Bristlecone

/// Helper functions for the creation of `Solver` functions, which apply time-series models
/// to time-series data (when using Bristlecone time-series types).
module Solver =

    open Bristlecone.EstimationEngine
    open Bristlecone.Time
    open Bristlecone.ModelSystem
    open Bristlecone.Numerics
    open Bristlecone.Numerics.Typed

    /// If environmental forcing data is supplied, the output series may be
    /// configured to be either external (i.e. on observation timeline) or
    /// internal (e.g. on environmental data timeline).
    type StepType<'date> =
        | Internal
        | External of 'date list

    /// Some runners return a paired set of time-index * scalars whereas others
    /// return a tuple of a list of time-index and vectors for each map item.
    type RunnerOutput<'S,'V,[<Measure>] 'modelTimeUnit> =
        | Paired of CodedMap<(float<'modelTimeUnit ``time index``> * TypedScalar<'S,state>)[]>
        | Unpaired of float<'modelTimeUnit ``time index``> list * CodedMap<TypedVector<'S,'V,state>>

    /// Masks to only return the requested values, for example when
    /// comparing with observational data.
    module Masking =

        /// Apply masking/normalisation to runner output, producing aligned vectors.
        let makeMaskOutput
            (numEngine: NumericEngine<'S,'V,'M>)
            (stepType: StepType<'date>)
            (dateMode: DateMode.DateMode<'date, 'yearType, 'timespan>)
            (startDate: 'date)
            (dataTimeToIndexTime: DateMode.Conversion.ResolutionToModelUnits<'date, 'timespan, ``time index``>)
            : RunnerOutput<'S,'V,1> -> CodedMap<TypedVector<'S,'V,state>> =

            match stepType with
            | Internal ->
                fun output ->
                    match output with
                    | Paired vars -> vars |> Map.map (fun _ series -> series |> Array.map snd |> Stats.stack1D numEngine.vectorBackend numEngine.vectorAccess)
                    | Unpaired(_times, vars) -> vars

            | External obsDates ->
                let obsTimes =
                    obsDates
                    |> List.map (fun d ->
                        let diff = dateMode.Difference startDate d
                        dataTimeToIndexTime (DateMode.Conversion.FromDifference diff))
                    |> Set.ofList

                fun output ->
                    match output with
                    | Paired vars ->
                        vars
                        |> Map.map (fun _ series ->
                            series
                            |> Array.filter (fun (ti, _) -> Set.contains ti obsTimes)
                            |> Array.map snd
                            |> Stats.stack1D numEngine.vectorBackend numEngine.vectorAccess)

                    | Unpaired(times, vars) ->
                        let keepMask =
                            times |> List.map (fun ti -> Set.contains ti obsTimes) |> List.toArray

                        vars |> Map.map (fun _ v -> Vector.filterByMask keepMask v)


    // Time conversion tailored for TypedTensor and time-index
    module internal TimeWrapping =

        /// Wrap a model equation written in 'timeUnit so it accepts time index.
        let wrapTime<'S,'V,[<Measure>] 'timeUnit, [<Measure>] 'returnUnit>
            (factor: float<'timeUnit / ``time index``>)
            (eq: GenericModelEquation<'S,'V,'timeUnit, 'returnUnit>)
            : GenericModelEquation<'S,'V,``time index``, 'returnUnit> =
            fun pars env tIndex state ->
                // Convert tIndex (Scalar<time-index>) -> Scalar<'timeUnit> by multiplying with factor
                let tIndexF: float<``time index``> = Scalar.toFloat tIndex
                let tModel = Typed.ofScalar tIndex.Backend (tIndexF * factor)
                eq pars env tModel state

        /// Wrap a model equation written in 'timeUnit so it accepts time index.
        let wrapTimeDifference<'S,'V,[<Measure>] 'timeUnit>
            (factor: float<'timeUnit / ``time index``>)
            (eq: StateEquation<'S,'V,'timeUnit>)
            : StateEquation<'S,'V,``time index``> =
            fun pars env tIndex state ->
                let tIndexF = Scalar.toFloat tIndex
                let tModel = Typed.ofScalar tIndex.Backend (tIndexF * factor)
                eq pars env tModel state

        // For differential equations: return unit is 'state / 'timeUnit
        let wrapTimeDifferential<'S,'V,[<Measure>] 'timeUnit>
            (factor: float<'timeUnit / ``time index``>)
            (eq: RateEquation<'S,'V,'timeUnit>)
            : RateEquation<'S,'V,``time index``> =
            fun pars env tIndex state ->
                let tIndexF = Scalar.toFloat tIndex
                let tModel = Typed.ofScalar tIndex.Backend (tIndexF * factor)
                eq pars env tModel state |> Scalar.retype

        // Lift wrapping over a whole model form
        let wrapModelForm<'S,'V,[<Measure>] 'timeUnit>
            (factor: float<'timeUnit / ``time index``>)
            (mf: ModelForm<'S,'V,'timeUnit>)
            : ModelForm<'S,'V,``time index``> =
            match mf with
            | DifferenceEqs eqs -> eqs |> Map.map (fun _ e -> wrapTimeDifference factor e) |> DifferenceEqs
            | DifferentialEqs eqs -> eqs |> Map.map (fun _ e -> wrapTimeDifferential factor e) |> DifferentialEqs


    /// Runners return the baseline state plus every evolved state.
    module SolverRunners =

        module DiscreteTime =

            let internal stepOnce
                (eqs: CodedMap<StateEquation<'S,'V,``time index``>>)
                (pars: TypedVector<'S,'V,``parameter``>)
                (env: CodedMap<TypedScalar<'S,environment>>)
                (t: TypedScalar<'S,``time index``>)
                (state: CodedMap<TypedScalar<'S,state>>)
                : CodedMap<TypedScalar<'S,state>> =
                eqs |> Map.map (fun key eq -> eq pars env t state.[key])

            let iterateDifference
                (numericEngine: NumericEngine<'S,'V,'M>)
                (eqs: CodedMap<StateEquation<'S,'V,``time index``>>)
                (timeline: float<``time index``>[])
                (envStream: CodedMap<TypedScalar<'S,environment>>[])
                (baselineValue: CodedMap<TypedScalar<'S,state>>)
                pars
                : CodedMap<(float<``time index``> * TypedScalar<'S,state>)[]> =
                let _, outputs =
                    ((baselineValue, eqs |> Map.map (fun _ _ -> [])), timeline |> Array.mapi (fun i ti -> i, ti))
                    ||> Array.fold (fun (state, acc) (i, tiVal) ->
                        if i = 0 then
                            // Do not advance or record at baseline
                            (state, acc)
                        else
                            // Advance once from previous state to this time, then record
                            let t = Typed.ofScalar numericEngine.scalarBackend tiVal

                            let env =
                                Map.fold
                                    (fun acc k v -> Map.add k v acc)
                                    envStream.[i]
                                    (state |> Map.map (fun _ v -> Scalar.retype v))

                            let nextState = stepOnce eqs pars env t state
                            let acc' = acc |> Map.map (fun k vs -> (tiVal, nextState.[k]) :: vs)
                            (nextState, acc'))

                outputs |> Map.map (fun _ vs -> vs |> List.rev |> Array.ofList)

            /// The fixed runner must include the baseline date in its timeline,
            /// but only returns values for t1..tN.
            let fixedRunner
                (numericEngine: NumericEngine<'S,'V,'M>)
                (eqs: CodedMap<StateEquation<'S,'V,``time index``>>)
                (timeline: float<``time index``>[])
                (envIndex: CodedMap<TimeIndex.TimeIndex<float<environment>, _, _, _, 1>>)
                (baselineValueFn: TypedVector<'S,'V,``parameter``> -> CodedMap<TypedScalar<'S,state>>)
                point
                =
                let envStream =
                    timeline
                    |> Array.map (fun ti ->
                        envIndex
                        |> Map.map (fun _ idxTI ->
                            let v = idxTI.Item ti
                            Typed.ofScalar numericEngine.scalarBackend v))

                iterateDifference numericEngine eqs timeline envStream (baselineValueFn point) point |> Paired

            let variableRunner
                (numEngine: NumericEngine<'S,'V,'M>)
                (eqs: CodedMap<StateEquation<'S,'V,``time index``>>)
                (timeline: float<``time index``>[]) // irregular observation times
                (envIndex: CodedMap<TimeIndex.TimeIndex<float<environment>, _, _, _, 1>>)
                (t0: TypedVector<'S,'V,``parameter``> -> CodedMap<TypedScalar<'S,state>>)
                point
                =

                // Build environment stream aligned to each observation time
                let envStream =
                    timeline
                    |> Array.map (fun ti ->
                        envIndex
                        |> Map.map (fun _ idxTI ->
                            let v = idxTI.Item ti
                            Typed.ofScalar numEngine.scalarBackend v))

                // Run the difference equations interval‑by‑interval
                let outputs = iterateDifference numEngine eqs timeline envStream (t0 point) point

                // Return as Unpaired (times + values)
                Unpaired(
                    timeline.[1..] |> Array.toList,
                    outputs |> Map.map (fun _ arr -> arr |> Array.map snd |> Stats.stack1D numEngine.vectorBackend numEngine.vectorAccess)
                )


        module DifferentialTime =

            /// Integrate an ODE model system given fixed timesteps.
            /// Timeline includes baseline, outputs exclude baseline.
            /// The initialState is that at the baseline.
            /// Expects integration routines that include the baseline.
            let fixedRunner
                (numericEngine: NumericEngine<'S,'V,'M>)
                eqs
                (integrator: Integration.IntegrationRoutine<'S,'V,'M>)
                (times: float<``time index``> array)
                (forcings: CodedMap<TimeIndex.TimeIndex<float<environment>, _, _, _, 1>>)
                (initialStateFn: TypedVector<'S,'V,``parameter``> -> CodedMap<TypedScalar<'S,state>>)
                =

                let tStart, tEnd = times.[0], times.[times.Length - 1]
                let tStep = 1.<``time index``>
                let timelineIntegrated = [| tStart..tStep..tEnd |]

                let compiledRhs =
                    Integration.Base.makeCompiledFunctionForIntegration numericEngine tStart tEnd tStep forcings initialStateFn eqs

                let toPureIndex (x: float<'modelTime ``time index``>) =
                    x / LanguagePrimitives.FloatWithMeasure<'modelTime> 1.

                let fromPureIndex (x: float<``time index``>) =
                    x * LanguagePrimitives.FloatWithMeasure<'modelTime> 1.

                let integrate =
                    integrator
                        numericEngine
                        (Typed.ofScalar numericEngine.scalarBackend (toPureIndex tStart))
                        (Typed.ofScalar numericEngine.scalarBackend (toPureIndex tEnd))
                        (Typed.ofScalar numericEngine.scalarBackend tStep)

                fun parameters ->
                    let states = integrate (initialStateFn parameters) (compiledRhs parameters)
                    let statesTailed = states |> Map.map (fun _ v -> v |> Vector.tail)
                    Unpaired(timelineIntegrated.[1..] |> Array.toList |> List.map fromPureIndex, statesTailed)

            /// Timeline includes baseline.
            /// Outputs exclude baseline time/state.
            let variableRunner
                (numEngine: NumericEngine<'S,'V,'M>)
                eqs
                (integrator: Integration.IntegrationRoutine<'S,'V,'M>)
                (times: float<``time index``> array)
                (forcings: CodedMap<TimeIndex.TimeIndex<float<environment>, _, _, _, 1>>)
                (initialStateFn: TypedVector<'S,'V,``parameter``> -> CodedMap<TypedScalar<'S,state>>)
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
                                    numEngine
                                    tStart
                                    tEnd
                                    step
                                    forcings
                                    (fun _ -> currentState)
                                    eqs

                            let integrate =
                                integrator numEngine (Typed.ofScalar numEngine.scalarBackend tStart) (Typed.ofScalar numEngine.scalarBackend tEnd) (Typed.ofScalar numEngine.scalarBackend step)

                            let newTrajectory = integrate currentState (compiledRhs parameters)

                            let finalState =
                                newTrajectory
                                |> Map.map (fun _ v -> v |> Vector.itemAt (Vector.length v - 1))

                            loop (finalState :: acc) finalState (i + 1)

                    let states = loop [] initialState 1 |> List.toArray
                    let emptyAcc = initialState |> Map.map (fun _ _ -> [])

                    let vars =
                        states
                        |> Array.fold (fun acc s -> acc |> Map.map (fun k vs -> s.[k] :: vs)) emptyAcc
                        |> Map.map (fun _ scalars -> scalars |> List.rev |> Array.ofList |> Stats.stack1D numEngine.vectorBackend numEngine.vectorAccess)

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
                        Resolution.finestResolution dateMode.TotalDays fRes efRes
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

        /// A fixed factor to represent the conversion of one temporal
        /// resolution to another. Introduces discrepancies when a date mode
        /// with non-uniform time-spans is used (e.g. gregorian leap years).
        let private computeCanonicalFactor toModelUnits integrationRes =
            toModelUnits (DateMode.Conversion.FromResolution integrationRes)
            / 1.0<``time index``>

        let private interpFunction =
            function
            | Solver.Exact -> TimeIndex.IndexMode.Exact
            | Solver.Lower -> TimeIndex.IndexMode.Interpolate Statistics.Interpolate.lower
            | Solver.Linear -> TimeIndex.IndexMode.Interpolate Statistics.Interpolate.bilinear

        let private buildEnvIndex
            getInterpModeFor
            startDate
            (dataTimeToIndexTime: DateMode.Conversion.ConvertFrom<'date, 'timespan> -> float<1>)
            (environment: option<TimeFrame.TimeFrame<float<environment>, 'date, 'timeunit, 'timespan>>)
            : CodedMap<TimeIndex.TimeIndex<float<environment>, 'date, 'timeunit, 'timespan, 1>> =
            environment
            |> Option.map (fun f ->
                f.Series
                |> Map.map (fun sc v ->
                    let mode = sc |> getInterpModeFor |> interpFunction
                    TimeIndex.TimeIndex(startDate, dataTimeToIndexTime, mode, v)))
            |> Option.defaultValue Map.empty

        /// Mandate that environmental data falls exactly on the solver's timesteps.
        /// Only required when no interpolation is specified.
        let enforceExactAlignment
            (envIndex: CodedMap<TimeIndex.TimeIndex<float<environment>, 'date, 'timeunit, 'timespan, 'modelTimeUnit>>)
            solverTimeline
            =
            envIndex
            |> Map.iter (fun v t ->
                if not (solverTimeline |> Array.contains t) then
                    invalidOp (sprintf "Environment variable has Exact mode but no value at solver time %A" t))

        let private stateVariableKeys (models: ModelSystem.ModelForm<'S,'V,'modelTimeUnit>) =
            match models with
            | ModelForm.DifferenceEqs eqs -> eqs |> Map.keys
            | ModelForm.DifferentialEqs eqs -> eqs |> Map.keys

        /// Compile a configured solver, automatically selecting the correct runner.
        /// t0 (conditioned or otherwise) is obtained automatically from the dynamic series.
        let compile
            (numericEngine: NumericEngine<'S,'V,'M>)
            logTo
            (dataTimeToModelTime: DateMode.Conversion.ResolutionToModelUnits<'date, 'timespan, 'modelTimeUnit>)
            (modelEquations: ModelForm<'S,'V,'modelTimeUnit>)
            engineTimeMode
            (stepType: StepType<'date>)
            (observedStates: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>)
            (observedMeasuresT0: CodedMap<TypedScalar<'S,state>>)
            (hiddenStatesT0: CodedMap<TypedScalar<'S,state>>)
            (hiddenStatesT0Initialisers: CodedMap<ModelSystem.Initialiser<'S,'V,state>>)
            (environment: TimeFrame.TimeFrame<float<environment>, 'date, 'timeunit, 'timespan> option)
            (interpolationModeFor: ShortCode.ShortCode -> Solver.InterpolationMode)
            : Solver.ConfiguredSolver<'S,'V> =

            // 1. Initial setup; identify t0 and start date.
            let headSeries = (observedStates.Series |> Seq.head).Value
            let dateMode = headSeries.DateMode
            let startDate = observedStates.StartDate

            let observedStatesT0 =
                observedStates
                |> TimeFrame.t0
                |> Map.map (fun k v -> Typed.ofScalar numericEngine.scalarBackend v)
                |> Map.fold (fun acc k v -> Map.add k v acc) hiddenStatesT0

            let baselineObservables =
                observedMeasuresT0
                |> Map.fold (fun acc k v -> Map.add k v acc) hiddenStatesT0
                |> Map.fold (fun acc k v -> Map.add k v acc) observedStatesT0

            let states = stateVariableKeys modelEquations

            let t0States parameters =
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

            // 2. Select the finest data resolution as the integration resolution.
            let integrationRes = decideIntegrationResolution observedStates environment

            logTo
            <| Logging.GeneralEvent(sprintf "Solver: integration resolution is %A" integrationRes)

            // 3. Compute fixed factor and wrap equations so that time is
            // retrieved in the resolution of the model.
            let factor = computeCanonicalFactor dataTimeToModelTime integrationRes
            let modelInTI = TimeWrapping.wrapModelForm factor modelEquations

            // 4. Build timeline and env index
            let modelTimeIndexToIndex (x: float<'modelTimeUnit ``time index``>) =
                x / LanguagePrimitives.FloatWithMeasure<'modelTimeUnit> 1.

            let modelTimeToIndex (x: float<'modelTimeUnit>) : float<``time index``> = Units.retype x
            let eraseModelUnit (x: float<'modelTimeUnit>) : float<1> = Units.retype x

            let envIndex: CodedMap<TimeIndex.TimeIndex<float<environment>, 'date, 'timeunit, 'timespan, 1>> =
                buildEnvIndex interpolationModeFor startDate (dataTimeToModelTime >> eraseModelUnit) environment

            // 4. Precompute a keep-mask for External mode
            let maskOutput =
                Masking.makeMaskOutput numericEngine stepType dateMode startDate (dataTimeToModelTime >> modelTimeToIndex)

            logTo <| Logging.GeneralEvent(sprintf "Solver: starting at date %A" startDate)

            environment
            |> Option.iter (fun s -> logTo <| Logging.GeneralEvent(sprintf "Solver: env data start = %A" s.StartDate))

            envIndex
            |> Map.iter (fun k v ->
                logTo
                <| Logging.GeneralEvent(sprintf "%A Baseline %A, values %A" k v.Baseline v.Values))

            // 5. Pick runner automatically
            let obsResolution = TimeFrame.resolution observedStates

            let runner =
                match obsResolution with
                | Resolution.Fixed _ ->

                    let fixedTimeline =
                        headSeries
                        |> TimeIndex.create startDate dataTimeToModelTime
                        |> Seq.map (fst >> modelTimeIndexToIndex)
                        |> Seq.toArray

                    logTo
                    <| Logging.GeneralEvent(sprintf "Fixed timeline in model's time resolution is %A" fixedTimeline)

                    match modelInTI, engineTimeMode with
                    | DifferentialEqs eqs, Continuous i ->
                        SolverRunners.DifferentialTime.fixedRunner numericEngine eqs i fixedTimeline envIndex t0States

                    | DifferenceEqs eqs, Discrete ->
                        SolverRunners.DiscreteTime.fixedRunner numericEngine eqs fixedTimeline envIndex t0States

                    | _ -> invalidOp "Mismatch between time-mode and differential/difference equation form."

                | Resolution.Variable ->

                    let obsTimes =
                        observedStates
                        |> TimeFrame.dates
                        |> Seq.map (fun d ->
                            let diff = dateMode.Difference startDate d

                            dataTimeToModelTime (DateMode.Conversion.FromDifference diff)
                            |> modelTimeToIndex)
                        |> Seq.toArray

                    logTo <| Logging.GeneralEvent(sprintf "Variable timeline is %A" obsTimes)

                    match modelInTI, engineTimeMode with
                    | DifferentialEqs eqs, Continuous i ->
                        SolverRunners.DifferentialTime.variableRunner numericEngine eqs i obsTimes envIndex t0States

                    | DifferenceEqs eqs, Discrete ->
                        SolverRunners.DiscreteTime.variableRunner numericEngine eqs obsTimes envIndex t0States

                    | _ -> invalidOp "Mismatch between time-mode and differential/difference equation form."
            
            // 6. Return configured solver
            fun point -> point |> runner |> maskOutput, t0States point |> Map.append observedMeasuresT0


    /// Solver conditioning enables adding synthetic initial time-points
    /// from which to solve from.
    module Conditioning =

        type Resolved<'S, 'date, 'timeunit, 'timespan> =
            { StatesHiddenForSolver: CodedMap<TypedScalar<'S,state>>
              StatesObservedForSolver: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>
              MeasuresForSolver: CodedMap<TypedScalar<'S,state>>
              ExogenousForSolver: option<TimeFrame.TimeFrame<float<environment>, 'date, 'timeunit, 'timespan>>
              ObservedForPairing: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>
              Log: string option }

        let internal t0FromFirstObs mkScalar (tf: TimeFrame.TimeFrame<float<state>, _, _, _>) =
            tf.Series
            |> Map.map (fun _ ts -> ts |> TimeSeries.head |> fst |> mkScalar)

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
            (t0: CodedMap<TypedScalar<'S,state>>)
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

            let solverStartDate =
                match TimeFrame.resolution observedTF with
                | Resolution.Fixed res ->
                    match res with
                    | Resolution.Years y -> dm.AddYears observedTF.StartDate -y.Value
                    | Resolution.Months m -> dm.AddMonths observedTF.StartDate -m.Value
                    | Resolution.Days d -> dm.AddDays observedTF.StartDate -d.Value
                    | Resolution.CustomEpoch c -> dm.SubtractTime observedTF.StartDate c
                | Resolution.Variable -> invalidOp "Conditioning requires fixed dynamic resolution."

            let dynamicForSolver =
                observedTF
                |> TimeFrame.prepend solverStartDate (t0 |> Map.map (fun _ v -> Scalar.toFloat v))

            let trimmedEnv = envTF |> Option.map (ensureEnvCoverage solverStartDate)

            let measuresT0, nonMeasures =
                t0 |> Map.partition (fun k _ -> measureKeys |> Seq.contains k)

            let hiddenT0 =
                nonMeasures
                |> Map.filter (fun k _ -> equationKeys |> Seq.contains k && not (observedTF.Keys |> Seq.contains k))

            { StatesObservedForSolver = dynamicForSolver //toEquationStatesOnly equationKeys dynamicForSolver
              StatesHiddenForSolver = hiddenT0
              MeasuresForSolver = measuresT0
              ObservedForPairing = observedTF
              ExogenousForSolver = trimmedEnv
              Log = Some logMessage }

        let resolve
            mkScalar
            (conditioning: Conditioning.Conditioning<'stateUnit>)
            (observedTF: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>)
            (exogenousTF: option<TimeFrame.TimeFrame<float<environment>, 'date, 'timeunit, 'timespan>>)
            equationKeys
            measureKeys
            : Resolved<'S, 'date, 'timeunit, 'timespan> =

            match conditioning with
            | Conditioning.NoConditioning ->
                // Solver baseline = obs[0]; predictions (External) align to obs[1..]
                let solverStartDate = observedTF.StartDate
                let env = exogenousTF |> Option.map (ensureEnvCoverage solverStartDate)
                let t0 = t0FromFirstObs mkScalar observedTF
                let trimmedDyn = TimeFrame.dropFirstObservation observedTF

                { StatesObservedForSolver = observedTF // toEquationStatesOnly equationKeys observedTF
                  StatesHiddenForSolver = Map.empty // No conditioning for hidden states. They must be set using initialisers if present.
                  MeasuresForSolver = t0 |> Map.filter (fun k _ -> measureKeys |> Seq.contains k)
                  ObservedForPairing = trimmedDyn
                  ExogenousForSolver = env
                  Log = Some "No conditioning: predictions start at t1; baseline = first observation." }

            | Conditioning.Custom t0Map ->
                let t0 =
                    t0Map
                    |> Map.map (fun _ v -> v |> Units.removeUnitFromFloat |> (*) 1.<state> |> mkScalar)

                resolveWithConditionedT0
                    t0
                    observedTF
                    exogenousTF
                    equationKeys
                    measureKeys
                    "Custom conditioning: synthetic t0 one step before first observation; pairs include obs[0]."

            | Conditioning.RepeatFirstDataPoint ->
                let t0 = t0FromFirstObs mkScalar observedTF

                resolveWithConditionedT0
                    t0
                    observedTF
                    exogenousTF
                    equationKeys
                    measureKeys
                    "Repeat-first conditioning: duplicated first obs one step earlier; pairs include obs[0]."
