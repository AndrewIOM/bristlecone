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
    type StepType =
        | Internal
        | External

    // Time conversion tailored for your TypedTensor and time-index
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
                eqs
                timeline
                (envStream: CodedMap<TypedTensor<Scalar,environment>>[])
                t0
                pars
                : CodedMap<TypedTensor<Vector,state>> =

                let _, buffers =
                    ((t0, Map.map (fun _ _ -> ResizeArray()) eqs), Array.indexed timeline)
                    ||> Array.fold (fun (state, acc) (i, tiVal) ->
                        let t = Typed.ofScalar tiVal
                        let env = envStream.[i]
                        let nextState = stepOnce eqs pars env t state
                        nextState |> Map.iter (fun k v -> acc.[k].Add v)
                        (nextState, acc))

                buffers
                |> Map.map (fun _ buf ->
                    buf
                    |> Seq.toArray
                    |> Tensors.Typed.stack1D)

            let discreteRunner
                (eqs: CodedMap<StateEquation<``time index``>>)
                (timeline: float<``time index``>[])
                (envIndex: CodedMap<TimeIndex.TimeIndex<float<environment>,_,_,_>>)
                t0
                point =
                let envStream =
                    timeline
                    |> Array.map (fun ti ->
                        envIndex |> Map.map (fun _ idxTI ->
                            let v = idxTI.Item ti
                            Typed.ofScalar v))
                iterateDifference eqs timeline envStream t0 point


        module DifferentialTime =

            /// Step the solver using high resolution, and output at low resolution.
            /// External steps are of a fixed width.
            let fixedStep (model:CodedMap<TensorODE>) (i:Integration.IntegrationRoutine) tStart tEnd (initialState: CodedMap<TypedTensor<Scalar,state>>) forcings =
                // TODO Resolve environment vs state conflict in integration maker, to remove below line
                let t0' = initialState |> Map.map (fun _ v -> v.Value |> Tensors.tryAsScalar<environment> |> Option.get)
                let compiledRhs = Integration.Base.makeCompiledFunctionForIntegration tStart tEnd 1.<``time index``> t0' forcings model
                let tStart, tEnd, tStep = Typed.ofScalar tStart, Typed.ofScalar tEnd, Typed.ofScalar 1.<``time index``>
                let integrate = i tStart tEnd tStep initialState
                fun parameters ->
                    integrate (compiledRhs parameters)
                    |> Map.map (fun _ v -> v |> Tensors.Typed.tail)

            let differentialRunner
                (eqs: CodedMap<ModelSystem.RateEquation<``time index``>>)
                integrateRoutine
                (timeline: float<``time index``>[])
                (envIndex: CodedMap<TimeIndex.TimeIndex<float<environment>,_,_,_>>)
                t0
                point =
                let startIndex = timeline.[0]
                let endIndex   = timeline.[timeline.Length - 1]
                let solve = fixedStep eqs integrateRoutine (startIndex - 1.<``time index``>) endIndex t0 envIndex
                solve point



    module SolverCompiler =
        
        /// Determine the appropriate resolution for the integration routine
        /// to run along, if using a fixed resolution.
        let internal decideIntegrationResolution dynamicSeries environment =
            match dynamicSeries |> TimeFrame.resolution with
            | Resolution.Fixed fRes ->
                let iRes =
                    match environment with
                    | Some envTF ->
                        match envTF |> TimeFrame.resolution with
                        | Resolution.Fixed efRes -> efRes
                        | Resolution.Variable -> failwith "Variable-time environmental forcing data is not supported."
                    | None -> fRes
                iRes, fRes
            | Resolution.Variable ->
                match environment with
                | Some _ -> failwith "Variable time solver with environmental data is not yet supported."
                | None ->
                    let medianTimespan =
                        dynamicSeries.Series
                        |> Seq.collect (fun ts -> ts.Value.TimeSteps)
                        |> Seq.sort
                        |> Seq.splitInto 2
                        |> Seq.skip 1
                        |> Seq.head
                        |> Seq.head
                    let custom = Resolution.FixedTemporalResolution.CustomEpoch medianTimespan
                    custom, custom

        let private computeFactor toModelUnits (dateMode:DateMode.DateMode<'a,'b,'c>) integrationRes =
            let span = dateMode.ResolutionToSpan integrationRes
            toModelUnits span / 1.0<``time index``>

        let private buildEnvIndex startDate integrationRes (environment:option<TimeFrame.TimeFrame<float<environment>,'date,'timeunit,'timespan>>) : CodedMap<TimeIndex.TimeIndex<float<environment>,'date,'timeunit,'timespan>> =
            environment
            |> Option.map (fun f ->
                f.Series
                |> Map.map (fun _ v ->
                    TimeIndex.TimeIndex(
                        startDate,
                        integrationRes,
                        TimeIndex.IndexMode.Interpolate Statistics.Interpolate.lower,
                        v)))
            |> Option.defaultValue Map.empty

        let private buildKeepMask headSeries startDate observationRes timeline =
            let observedIndices =
                headSeries
                |> TimeIndex.create startDate observationRes
                |> Seq.map fst
                |> Set.ofSeq
            timeline |> Array.map (fun idx -> Set.contains idx observedIndices)

        /// Compile a configured solver, automatically selecting the correct runner
        let compile
            logTo
            (toModelUnits: 'timespan -> float<'modelTimeUnit>)
            (modelEquations: ModelForm<'modelTimeUnit>)
            engineTimeMode
            stepType
            (dynamicSeries: TimeFrame.TimeFrame<float<state>, 'date, 'timeunit, 'timespan>)
            (environment: TimeFrame.TimeFrame<float<environment>, 'date, 'timeunit, 'timespan> option)
            (t0: CodedMap<TypedTensor<Scalar,state>>)
            : Solver.ConfiguredSolver =

            let headSeries = (dynamicSeries.Series |> Seq.head).Value
            let dateMode   = headSeries.DateMode
            let startDate  = dynamicSeries.StartDate

            // 1. Decide resolutions
            let integrationRes, observationRes =
                decideIntegrationResolution dynamicSeries environment

            // 2. Compute factor and wrap equations
            let factor = computeFactor toModelUnits dateMode integrationRes
            let modelInTI = TimeWrapping.wrapModelForm factor modelEquations

            // 3. Build timeline and env index
            let envIndex = buildEnvIndex startDate integrationRes environment
            let timeline =
                headSeries
                |> TimeIndex.create startDate integrationRes
                |> Seq.map fst
                |> Seq.toArray

            // 4. Precompute keepMask for External mode
            let keepMask = buildKeepMask headSeries startDate observationRes timeline

            // 5. Pick runner automatically
            let runner =
                match modelInTI, engineTimeMode with
                | DifferentialEqs eqs, Continuous i -> SolverRunners.DifferentialTime.differentialRunner eqs i
                | DifferenceEqs eqs, Discrete -> SolverRunners.DiscreteTime.discreteRunner eqs
                | _ -> invalidOp "Mismatch between time-mode and differential/difference equation form."

            // 6. Return configured solver
            fun point ->
                let series = runner timeline envIndex t0 point
                match stepType with
                | Internal -> series
                | External -> series |> Map.map (fun _ v -> Tensors.Typed.filterByMask keepMask v)



    /// Step the solver using the high resolution, and output at low resolution.
    /// External steps can be variable in size.
    /// Each time jump is integrated individually.
    let variableExternalStep model timeHandling timeSteps (initialState: CodedMap<Tensors.TypedTensor<Tensors.Scalar,state>>)
        : Solver.ConfiguredSolver =
        match timeHandling with
        | Discrete -> invalidOp "Not configured"
        | Continuous i ->
            failwith "implementation requires mending (AD)"
            // fun parameters ->
            //     let results =
            //         timeSteps
            //         |> Seq.pairwise
            //         |> Seq.scan
            //             (fun state (tStart, tEnd) ->
            //                 let integrate = i tStart tEnd (tEnd - tStart) state
            //                 let rhs = Integration.Base.makeCompiledFunctionForIntegration tStart tEnd 1.<``time index``> state Map.empty model
            //                 integrate (rhs parameters) |> Map.map (fun _ v -> v.[0])) initialState

            //     eqs
            //     |> Map.toSeq
            //     |> Seq.map (fun (key, _) -> key, results |> Seq.map (fun r -> r.[key]) |> Seq.toArray)
            //     |> Map.ofSeq


    /// Conditioning of time-series data, which allows for maximum use of observed time-series data.
    module Conditioning =

        open Bristlecone.Time

        /// Strategy for assigning a start time - `t0` - to a time series.
        let startPoint conditioning (series: CodedMap<TimeSeries<float<'u>, 'date, 'timeunit, 'timespan>>) =
            match conditioning with
            | Conditioning.NoConditioning -> None
            | Conditioning.RepeatFirstDataPoint -> series |> Map.map (fun _ v -> v.Values |> Seq.head) |> Some
            | Conditioning.Custom precomputed -> precomputed |> Some
