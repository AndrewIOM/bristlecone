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
        | Paired of CodedMap<(float<``time index``> * TypedTensor<Scalar,state>)[]>
        | Unpaired of float<``time index``> list * CodedMap<TypedTensor<Vector,state>>

    /// Masks to only return the requested values, for example when
    /// comparing with observational data.
    module Masking =

        /// Apply masking/normalisation to runner output, producing aligned vectors.
        let makeMaskOutput
            (stepType: StepType<'date>)
            (dateMode: DateMode.DateMode<'date,'yearType,'timespan>)
            (startDate: 'date)
            (factor: float<'modelTimeUnit/``time index``>)
            toModelUnits
            : RunnerOutput -> CodedMap<TypedTensor<Vector,state>> =

            match stepType with
            | Internal ->
                fun output ->
                    match output with
                    | Paired vars ->
                        vars |> Map.map (fun _ series ->
                            series |> Array.map snd |> Typed.stack1D)
                    | Unpaired (_times, vars) ->
                        vars

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
                        vars |> Map.map (fun _ series ->
                            series
                            |> Array.filter (fun (ti, _) -> Set.contains ti obsTimes)
                            |> Array.map snd
                            |> Typed.stack1D)

                    | Unpaired (times, vars) ->
                        let keepMask = times |> List.map (fun ti -> Set.contains ti obsTimes) |> List.toArray
                        vars |> Map.map (fun _ v -> Typed.filterByMask keepMask v)


    // Time conversion tailored for TypedTensor and time-index
    module internal TimeWrapping =

        /// Wrap a model equation written in 'timeUnit so it accepts time index.
        let wrapTime<[<Measure>] 'timeUnit, [<Measure>] 'returnUnit>
            (factor: float<'timeUnit / ``time index``>)
            (eq: GenericModelEquation<'timeUnit, 'returnUnit>)
            : GenericModelEquation<``time index``,'returnUnit> =
            fun pars env tIndex state ->
                // Convert tIndex (Scalar<time-index>) -> Scalar<'timeUnit> by multiplying with factor
                let tIndexF = Typed.toFloatScalar tIndex
                let tModel  = Typed.ofScalar (tIndexF * factor)
                eq pars env tModel state

        /// Wrap a model equation written in 'timeUnit so it accepts time index.
        let wrapTimeDifference<[<Measure>] 'timeUnit, [<Measure>] 'state>
            (factor: float<'timeUnit / ``time index``>)
            (eq: StateEquation<'timeUnit>) : StateEquation<``time index``> =
            fun pars env tIndex state ->
                let tIndexF = Typed.toFloatScalar tIndex
                let tModel  = Typed.ofScalar (tIndexF * factor)
                eq pars env tModel state

        // For differential equations: return unit is 'state / 'timeUnit
        let wrapTimeDifferential<[<Measure>] 'timeUnit, [<Measure>] 'state>
            (factor: float<'timeUnit / ``time index``>)
            (eq: RateEquation<'timeUnit>) : RateEquation<``time index``> =
            fun pars env tIndex state ->
                let tIndexF = Typed.toFloatScalar tIndex
                let tModel  = Typed.ofScalar (tIndexF * factor)
                eq pars env tModel state |> Typed.retype

        // Lift wrapping over a whole model form
        let wrapModelForm<[<Measure>] 'timeUnit>
            (factor: float<'timeUnit / ``time index``>)
            (mf: ModelForm<'timeUnit>)
            : ModelForm<``time index``> =
            match mf with
            | DifferenceEqs eqs   -> eqs |> Map.map (fun _ e -> wrapTimeDifference factor e) |> DifferenceEqs
            | DifferentialEqs eqs -> eqs |> Map.map (fun _ e -> wrapTimeDifferential factor e) |> DifferentialEqs


    /// Runners return the baseline state plus every evolved state.
    module SolverRunners =

        module DiscreteTime =

            let internal stepOnce
                (eqs: CodedMap<StateEquation<``time index``>>)
                (pars: TypedTensor<Vector,``parameter``>)
                (env: CodedMap<TypedTensor<Scalar,environment>>)
                (t: TypedTensor<Scalar,``time index``>)
                (state: CodedMap<TypedTensor<Scalar,state>>)
                : CodedMap<TypedTensor<Scalar,state>> =
                eqs |> Map.map (fun key eq -> eq pars env t state.[key])

            let iterateDifference
                (eqs: CodedMap<StateEquation<``time index``>>)
                (timeline: float<``time index``>[])
                (envStream: CodedMap<TypedTensor<Scalar,environment>>[])
                (baselineValue: CodedMap<TypedTensor<Scalar,state>>)
                pars
                : CodedMap<(float<``time index``> * TypedTensor<Scalar,state>)[]> =
                let _, outputs =
                    ((baselineValue, eqs |> Map.map (fun _ _ -> [])), timeline |> Array.mapi (fun i ti -> i, ti))
                    ||> Array.fold (fun (state, acc) (i, tiVal) ->
                        if i = 0 then
                            // Do not advance or record at baseline
                            (state, acc)
                        else
                            // Advance once from previous state to this time, then record
                            let t   = Typed.ofScalar tiVal
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
                (envIndex: CodedMap<TimeIndex.TimeIndex<float<environment>,_,_,_>>)
                baselineValue
                point =
                let envStream =
                    timeline
                    |> Array.map (fun ti ->
                        envIndex |> Map.map (fun _ idxTI ->
                            let v = idxTI.Item ti
                            Typed.ofScalar v))
                iterateDifference eqs timeline envStream baselineValue point |> Paired

            let variableRunner
                (eqs: CodedMap<StateEquation<``time index``>>)
                (timeline: float<``time index``>[])   // irregular observation times
                (envIndex: CodedMap<TimeIndex.TimeIndex<float<environment>,_,_,_>>)
                t0
                point =

                // Build environment stream aligned to each observation time
                let envStream =
                    timeline
                    |> Array.map (fun ti ->
                        envIndex |> Map.map (fun _ idxTI ->
                            let v = idxTI.Item ti
                            Typed.ofScalar v))

                // Run the difference equations interval‑by‑interval
                let outputs = iterateDifference eqs timeline envStream t0 point

                // Return as Unpaired (times + values)
                Unpaired (timeline.[1..] |> Array.toList,
                        outputs |> Map.map (fun _ arr -> arr |> Array.map snd |> Typed.stack1D))


        module DifferentialTime =

            /// Integrate an ODE model system given fixed timesteps.
            /// Timeline includes baseline, outputs exclude baseline.
            /// The initialState is that at the baseline.
            /// Expects integration routines that include the baseline.
            let fixedRunner eqs (integrator:Integration.IntegrationRoutine) (times: float<``time index``> array) (forcings:CodedMap<TimeIndex.TimeIndex<float<environment>,_,_,_>>) (initialState: CodedMap<TypedTensor<Scalar,state>>) =
                // TODO Resolve environment vs state conflict in integration maker, to remove below line
                let t0' = initialState |> Map.map (fun _ v -> v.Value |> Tensors.tryAsScalar<environment> |> Option.get)
                let tStart, tEnd = times.[0], times.[times.Length-1]
                let compiledRhs = Integration.Base.makeCompiledFunctionForIntegration tStart tEnd 1.<``time index``> t0' forcings eqs
                let integrate = integrator (Typed.ofScalar tStart) (Typed.ofScalar tEnd) (Typed.ofScalar 1.<``time index``>) initialState
                fun parameters ->
                    let states = integrate (compiledRhs parameters)
                    let statesTailed = states |> Map.map(fun _ v -> v |> Typed.tail)
                    Unpaired (times.[1..] |> Array.toList, statesTailed)

            /// Timeline includes baseline.
            /// Outputs exclude baseline time/state.
            let variableRunner eqs (integrator:Integration.IntegrationRoutine) (times: float<``time index``> array) (forcings:CodedMap<TimeIndex.TimeIndex<float<environment>,_,_,_>>) (initialState: CodedMap<TypedTensor<Scalar,state>>) =
                // TODO Currently can't pre-compile RHS as relies on state vector.
                // In the compileRHS, it takes all of the 'state' and 'environment forcings' current values and combines them into a single vector, which fudges to state. When this is fixed, we can fix this code.
                // Splitting forcings from state would help for this and fixe above unit mismatch.
                fun parameters ->
                    let rec loop acc state i =
                        if i >= times.Length then List.rev acc
                        else
                            let tStart, tEnd = times.[i-1], times.[i]
                            let step = tEnd - tStart
                            // TODO Resolve environment vs state conflict in integration maker, to remove below line.
                            let state' = state |> Map.map (fun _ v -> v |> Typed.retype)
                            let compiledRhs = Integration.Base.makeCompiledFunctionForIntegration tStart tEnd step state' forcings eqs
                            let integrate = integrator (Typed.ofScalar tStart) (Typed.ofScalar tEnd) (Typed.ofScalar step) state
                            let newState = integrate (compiledRhs parameters)
                            let finalState = newState |> Map.map(fun _ v -> v |> Tensors.Typed.itemAt (Typed.length v - 1))
                            loop (finalState::acc) finalState (i+1)
                    let states = loop [] initialState 1 |> List.toArray
                    let emptyAcc = initialState |> Map.map (fun _ _ -> [])
                    let vars =
                        states
                        |> Array.fold (fun acc s -> acc |> Map.map (fun k vs -> s.[k] :: vs)) emptyAcc
                        |> Map.map (fun _ scalars -> scalars |> List.rev |> Array.ofList |> Typed.stack1D)
                    Unpaired (times.[1..] |> Array.toList, vars)


    module SolverCompiler =
        
        /// Determine the appropriate resolution for the integration routine
        /// to run along, if using a fixed resolution.
        let internal decideIntegrationResolution dynamicSeries environment =
            match TimeFrame.resolution dynamicSeries with
            | Resolution.Fixed fRes ->
                let iRes =
                    match environment |> Option.map TimeFrame.resolution with
                    | Some (Resolution.Fixed efRes) ->
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
        
        let private computeFactor toModelUnits (dateMode:DateMode.DateMode<'a,'b,'c>) integrationRes =
            let span = dateMode.ResolutionToSpan integrationRes
            toModelUnits span / 1.0<``time index``>

        let private interpFunction = function
            | Solver.Exact -> TimeIndex.IndexMode.Exact
            | Solver.Lower -> TimeIndex.IndexMode.Interpolate Statistics.Interpolate.lower
            | Solver.Linear -> TimeIndex.IndexMode.Interpolate Statistics.Interpolate.bilinear

        let private buildEnvIndex getInterpModeFor startDate integrationRes (environment:option<TimeFrame.TimeFrame<float<environment>,'date,'timeunit,'timespan>>) : CodedMap<TimeIndex.TimeIndex<float<environment>,'date,'timeunit,'timespan>> =
            environment
            |> Option.map (fun f ->
                f.Series
                |> Map.map (fun sc v ->
                    let mode = sc |> getInterpModeFor |> interpFunction
                    TimeIndex.TimeIndex(
                        startDate,
                        integrationRes,
                        mode,
                        v)))
            |> Option.defaultValue Map.empty

        /// Mandate that environmental data falls exactly on the solver's timesteps.
        /// Only required when no interpolation is specified.
        let enforceExactAlignment (envIndex:CodedMap<TimeIndex.TimeIndex<float<environment>,'date,'timeunit,'timespan>>) solverTimeline =
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

        /// Compile a configured solver, automatically selecting the correct runner.
        /// t0 (conditioned or otherwise) is obtained automatically from the dynamic series.
        let compile
            logTo
            (dataTimeToModelTime: 'timespan -> float<'modelTimeUnit>)
            (modelEquations: ModelForm<'modelTimeUnit>)
            engineTimeMode
            (stepType: StepType<'date>)
            (dynamicSeries: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>)
            (environment: TimeFrame.TimeFrame<float<environment>, 'date, 'timeunit, 'timespan> option)
            (interpolationModeFor: ShortCode.ShortCode -> Solver.InterpolationMode)
            : Solver.ConfiguredSolver =

            // 1. Initial setup; identify t0 and start date.
            let headSeries = (dynamicSeries.Series |> Seq.head).Value
            let dateMode = headSeries.DateMode
            let startDate = dynamicSeries.StartDate
            let t0 = dynamicSeries |> TimeFrame.t0 |> Map.map(fun k v -> Typed.ofScalar v)

            // 2. Decide resolutions
            let integrationRes = decideIntegrationResolution dynamicSeries environment

            // 3. Compute factor and wrap equations
            let factor = computeFactor dataTimeToModelTime dateMode integrationRes
            let modelInTI = TimeWrapping.wrapModelForm factor modelEquations

            // 4. Build timeline and env index
            let dataResolution = TimeFrame.resolution dynamicSeries
            let envIndex = buildEnvIndex interpolationModeFor startDate integrationRes environment

            // 4. Precompute a keep-mask for External mode
            let maskOutput = Masking.makeMaskOutput stepType dateMode startDate factor dataTimeToModelTime

            // 5. Pick runner automatically
            let runner =
                match dataResolution with
                | Resolution.Fixed _ ->

                    let fixedTimeline =
                        headSeries
                        |> TimeIndex.create startDate integrationRes
                        |> Seq.map fst
                        |> Seq.toArray

                    match modelInTI, engineTimeMode with
                    | DifferentialEqs eqs, Continuous i ->
                        SolverRunners.DifferentialTime.fixedRunner eqs i fixedTimeline envIndex t0

                    | DifferenceEqs eqs, Discrete ->
                        SolverRunners.DiscreteTime.fixedRunner eqs fixedTimeline envIndex t0

                    | _ -> invalidOp "Mismatch between time-mode and differential/difference equation form."

                | Resolution.Variable ->

                    let obsTimes =
                        dynamicSeries
                        |> TimeFrame.dates
                        |> Seq.map (fun d ->
                            let diff = dateMode.Difference startDate d
                            dataTimeToModelTime diff.RealDifference / factor)
                        |> Seq.toArray

                    match modelInTI, engineTimeMode with
                    | DifferentialEqs eqs, Continuous i ->
                        SolverRunners.DifferentialTime.variableRunner eqs i obsTimes envIndex t0

                    | DifferenceEqs eqs, Discrete ->
                        SolverRunners.DiscreteTime.variableRunner eqs obsTimes envIndex t0

                    | _ -> invalidOp "Mismatch between time-mode and differential/difference equation form."

            // 6. Return configured solver
            fun point ->
                point
                |> runner
                |> maskOutput


    /// Solver conditioning enables adding synthetic initial time-points
    /// from which to solve from.
    module Conditioning =

        type Resolved<'date,'timeunit,'timespan> = {
            T0: CodedMap<TypedTensor<Scalar,state>>
            DynamicForSolver: TimeFrame.TimeFrame<float<state>,'date,'timeunit,'timespan>
            DynamicForPairing: TimeFrame.TimeFrame<float<state>,'date,'timeunit,'timespan>
            Environment: option<TimeFrame.TimeFrame<float<environment>,'date,'timeunit,'timespan>>
            Log: string option
        }

        let internal t0FromFirstObs (tf: TimeFrame.TimeFrame<float<state>,_,_,_>) =
            tf.Series |> Map.map (fun _ ts -> ts |> TimeSeries.head |> fst |> Typed.ofScalar)

        let internal ensureEnvCoverage
            (solverStart: 'date)
            (envTF: TimeFrame.TimeFrame<'T,'date,'timeunit,'timespan>)
            : TimeFrame.TimeFrame<'T,'date,'timeunit,'timespan> =
            let firstEnvDate = envTF.StartDate
            if firstEnvDate > solverStart then
                invalidOp (sprintf "Environment data starts at %A but solver needs %A" firstEnvDate solverStart)
            else envTF
            
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
            (t0: CodedMap<TypedTensor<Scalar,state>>)
            (dynamicTF: TimeFrame.TimeFrame<float<state>,'date,'timeunit,'timespan>)
            (envTF: option<TimeFrame.TimeFrame<float<environment>,'date,'timeunit,'timespan>>)
            logMessage =
            
            // Work out one step backwards
            let firstSeries = dynamicTF.Series |> Seq.head |> fun kv -> kv.Value
            let dm = firstSeries.DateMode
            let stepSpan =
                match TimeFrame.resolution dynamicTF with
                | Resolution.Fixed res -> dm.ResolutionToSpan res
                | Resolution.Variable ->
                    invalidOp "Conditioning requires fixed dynamic resolution."

            let solverStartDate = dm.SubtractTime dynamicTF.StartDate stepSpan

            let dynamicForSolver =
                dynamicTF
                |> TimeFrame.prepend solverStartDate (t0 |> Map.map (fun _ v -> Typed.toFloatScalar v))

            let trimmedEnv = envTF |> Option.map (ensureEnvCoverage solverStartDate)

            { T0 = t0
              DynamicForSolver = dynamicForSolver
              DynamicForPairing = dynamicTF
              Environment = trimmedEnv
              Log = Some logMessage }

        let resolve
            (conditioning: Conditioning.Conditioning<'stateUnit>)
            (dynamicTF: TimeFrame.TimeFrame<float<state>,'date,'timeunit,'timespan>)
            (envTF: option<TimeFrame.TimeFrame<float<environment>,'date,'timeunit,'timespan>>)
            : Resolved<'date,'timeunit,'timespan> =

            match conditioning with
            | Conditioning.NoConditioning ->
                // Solver baseline = obs[0]; predictions (External) align to obs[1..]
                let solverStartDate = dynamicTF.StartDate
                let env = envTF |> Option.map (ensureEnvCoverage solverStartDate)
                let t0 = t0FromFirstObs dynamicTF
                let trimmedDyn = TimeFrame.dropFirstObservation dynamicTF
                { T0 = t0; DynamicForSolver = dynamicTF; DynamicForPairing = trimmedDyn; Environment = env; Log = Some "No conditioning: predictions start at t1; baseline = first observation." }

            | Conditioning.Custom t0Map ->
                let t0 = t0Map |> Map.map (fun _ v -> v |> Units.removeUnitFromFloat |> (*) 1.<state> |> Typed.ofScalar)
                resolveWithConditionedT0 t0 dynamicTF envTF "Custom conditioning: synthetic t0 one step before first observation; pairs include obs[0]."

            | Conditioning.RepeatFirstDataPoint ->
                let t0 = t0FromFirstObs dynamicTF
                resolveWithConditionedT0 t0 dynamicTF envTF "Repeat-first conditioning: duplicated first obs one step earlier; pairs include obs[0]."
