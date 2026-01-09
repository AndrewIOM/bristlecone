module ``Solver tests``

open System
open Expecto
open FsCheck
open Bristlecone
open Bristlecone.ModelSystem
open Bristlecone.EstimationEngine
open Bristlecone.Tensors
open Bristlecone.Time

module Runners =

    open Solver.SolverRunners

    let dummyParameters = [| 1.<parameter>; 1.<parameter> |] |> Typed.ofVector

    [<Tests>]
    let discreteTimeTests =
        testList "Discrete time" [

            testPropertyWithConfig Config.config
                "stepOnce applies each equation to the matching state" <| fun c (t0:NormalFloat) (v:NormalFloat) ->
                let f _ _ _ (state:TypedTensor<Scalar,state>) = state + Typed.ofScalar (v.Get * 1.<state>)
                let eqs = [ c, f ] |> Map.ofList
                let p = Typed.ofVector [| -999. * 1.<parameter> |]
                let t0' = [ c, Typed.ofScalar <| t0.Get * 1.<state> ] |> Map.ofList
                let t = 1.<``time index``> |> Typed.ofScalar
                let run = DiscreteTime.stepOnce eqs p Map.empty t t0'
                Expect.equal
                    (run.[c] |> Typed.toFloatScalar |> Units.removeUnitFromFloat)
                    (t0.Get + v.Get)
                    "Single step did not add value once"

            testPropertyWithConfig Config.config
                "iterateDifference accumulates outputs over the timeline" <| fun c (start:NormalFloat) (inc:NormalFloat) ->
                let f _ _ _ (state:TypedTensor<Scalar,state>) =
                    state + Typed.ofScalar (inc.Get * 1.<state>)
                let eqs = [ c, f ] |> Map.ofList
                let steps = 5
                // timeline includes baseline at 0.0
                let timeline = Array.init (steps+1) (fun i -> float i * 1.<``time index``>)
                let envStream = Array.init (steps+1) (fun _ -> Map.empty)
                let t0 = [ c, Typed.ofScalar (start.Get * 1.<state>) ] |> Map.ofList
                let p = Typed.ofVector [| 0.0 * 1.<parameter> |]
                let result = DiscreteTime.iterateDifference eqs timeline envStream t0 p
                let arr = result.[c] |> Array.map (snd >> Typed.toFloatScalar >> Units.removeUnitFromFloat)
                // expected excludes baseline, so length = steps
                let expected = Array.init steps (fun i -> start.Get + inc.Get * float (i+1))
                Expect.equal arr.Length steps "Output length must equal timeline length minus one (baseline excluded)"
                Config.sequenceEqualTol arr expected "Accumulated values do not match expected sequence"
                Expect.floatClose Accuracy.veryHigh arr.[0] (start.Get + inc.Get) "First output must be post-step from t0"

            testPropertyWithConfig Config.config
                "discreteRunner passes env values through to equations" <| fun c envKey (envVal:NormalFloat) ->
                let eqs : CodedMap<StateEquation<Time.``time index``>> =
                    [ c, fun _ (env: CodedMap<TypedTensor<Scalar,environment>>) _ _ -> env.[envKey] |> Typed.retype ]
                    |> Map.ofList
                let startDate = DateTime(1980,01,01)
                let times = [| for i in 0 .. 10 -> startDate.AddYears i |] // includes baseline year 0
                let timeline = [| for i in 0 .. 10 -> float i * 1.<``time index``> |]
                let fakeTimeSeries =
                    times |> Array.map(fun t -> envVal.Get * 1.<environment>, t) |> TimeSeries.fromNeoObservations
                let dataT0 _ = [ c, Typed.ofScalar 0.0<state> ] |> Map.ofList
                let point = Typed.ofVector [| 0.0 * 1.<parameter> |]
                let index = TimeIndex.TimeIndex(startDate, (fun c -> DateMode.Conversion.CalendarDates.toYears c |> Units.retype), TimeIndex.IndexMode.Exact, fakeTimeSeries)
                let indexMap = Map.ofList [ envKey, index ]
                let result = DiscreteTime.fixedRunner eqs timeline indexMap dataT0 point
                let outputTimes, outputData =
                    match result with
                    | Solver.RunnerOutput.Paired ts ->
                        ts.[c] |> Array.map fst,
                        ts.[c] |> Array.map snd |> Typed.stack1D |> Typed.toFloatArray |> Array.map Units.removeUnitFromFloat
                    | _ -> failwith "Expected paired output"
                // outputs exclude baseline, so length = timeline.Length - 1
                let expected = Array.init (timeline.Length-1) (fun _ -> envVal.Get)
                Expect.sequenceEqual outputData expected "Env value should flow through unchanged"
                Expect.sequenceEqual outputTimes timeline.[1..] "Output times must equal timeline excluding baseline"

            // Checks that a discrete model x_{t+1} = a*x_t + b matches the closed form.
            // The runner does not include t0 in the output, only t1 onwards.
            testPropertyWithConfig Config.config "Discrete affine matches closed form" <|
                fun shortCode (a: NormalFloat) (b: NormalFloat) (x0: NormalFloat) (PositiveInt steps) ->
                let steps = max 1 steps
                let a' = abs a.Get * 1.<state/state>
                let b' = abs b.Get * 1.<state>
                let x0' = x0.Get * 1.<state>
                let eqs = Map.ofList [ shortCode, fun _ _ _ state ->
                    Typed.ofScalar (a' * Typed.toFloatScalar state + b') ]
                // timeline includes baseline at 0.0
                let timeline = [| for i in 0 .. steps -> float i * 1.<``time index``> |]
                let envIndex = Map.empty
                let t0 _ = Map.ofList [ shortCode, Typed.ofScalar x0' ]
                let result = DiscreteTime.fixedRunner eqs timeline envIndex t0 dummyParameters
                let predTimes, predData =
                    match result with
                    | Solver.RunnerOutput.Paired ts ->
                        ts.[shortCode] |> Array.map fst,
                        ts.[shortCode] |> Array.map snd |> Typed.stack1D |> Typed.toFloatArray |> Array.map Units.removeUnitFromFloat
                    | _ -> failwith "Expected paired output"
                // Closed form at t=1..steps
                let closed (t: int) : float<state> =
                    if abs (float a' - 1.0) < 1e-12 then
                        x0' + b' * float t
                    else
                        x0' * (float a' ** float t) +
                        (b' * (1. - float a' ** float t) / (1. - float a'))
                Expect.equal predTimes timeline.[1..] "Output times must equal timeline excluding baseline"
                predData
                |> Array.iteri (fun i pred ->
                    let t = i+1
                    Expect.floatClose Accuracy.veryHigh pred (Units.removeUnitFromFloat <| closed t) ""
                )        ]

    let integrationMethod = Integration.RungeKutta.rk4

    [<Tests>]
    let differentialTests =
        testList "Differential time runner" [

            // Check increments are all ~ c * Δt
            testPropertyWithConfig Config.config "ODE constant-rate produces linear stock increments"
            <| fun shortCode (c: NormalFloat) (PositiveInt steps) ->
                    let c = if c.Get < 0. then c.Get * -1.<state> else c.Get * 1.<state>
                    let c' = c / 1.<``time index``>
                    let t0 _ = Map.ofList [ shortCode, Typed.ofScalar 0.0<state> ]
                    let eqs = Map.ofList [ shortCode, fun _ _ _ _ -> Typed.ofScalar c' ]
                    let timeline = [| for i in 0 .. steps -> float i * 1.<``time index``> |]
                    let series = DifferentialTime.fixedRunner eqs integrationMethod timeline Map.empty t0 dummyParameters
                    let predTimes, predData =
                        match series with
                        | Solver.RunnerOutput.Unpaired (times,data) ->
                            times,
                            data.[shortCode] |> Typed.toFloatArray
                        | _ -> failwith "Expected unpaired output"                    
                    let deltas = predData |> Array.pairwise |> Array.map (fun (a,b) -> b - a)
                    Expect.all deltas (fun d -> abs (d - c) < 1e-8<state>) "All increments match c"

        ]

module MakeSolver =

    [<Tests>]
    let compiledVsRunnerEquivalence =
        testList "Compile pipeline equivalence" [

            // Difference equations: compiled solver equals discrete runner
            testPropertyWithConfig Config.config "compile selects discrete runner and equals manual result" <|
            fun shortCode (a: NormalFloat) (b: NormalFloat) (x0: NormalFloat) ->
                // ModelForm: x_{t+1} = a x_t + b
                let a' = abs a.Get * 1.<state/state>
                let b' = b.Get * 1.<state>

                // Build a single-series dynamic frame (states only)
                let startDate = DateTime(2000,1,1)
                let obs = [| for i in 0..9 -> x0.Get * 1.<state>, startDate.AddDays(float i) |] |> TimeSeries.fromNeoObservations
                let dynTF = TimeFrame.tryCreate (Map.ofList [ shortCode, obs ]) |> Option.get

                // No environment
                let envOpt = None

                // Initial state
                let t0 _ = Map.ofList [ shortCode, Typed.ofScalar (x0.Get * 1.<state>) ]

                let eqs t =
                    Map.ofList [ shortCode,
                        fun _ t _ (state: TypedTensor<Scalar,state>) ->
                            Typed.ofScalar (a' * Typed.toFloatScalar state + b') ]
                let modelForm = ModelForm.DifferenceEqs (eqs ignore)

                // Engine configuration
                let engineTimeMode = EstimationEngine.Discrete
                let stepType = Solver.StepType.Internal
                let toModelUnits = DateMode.Conversion.CalendarDates.toDays

                // Compile configured solver
                let solver = Solver.SolverCompiler.compile ignore toModelUnits modelForm engineTimeMode stepType dynTF Map.empty Map.empty Map.empty envOpt (fun _ -> Solver.Exact)

                // Manual discrete run along the internal timeline
                let timeline =
                    dynTF.Series
                    |> Seq.head |> fun kv -> kv.Value
                    |> TimeIndex.create startDate toModelUnits
                    |> Seq.map fst |> Seq.toArray |> Array.map Units.retype
                let envIndex = Map.empty
                let manual = Solver.SolverRunners.DiscreteTime.fixedRunner (eqs ignore) timeline envIndex t0 Runners.dummyParameters
                let manTimes, man =
                    match manual with
                    | Solver.RunnerOutput.Paired ts ->
                        ts.[shortCode] |> Array.map fst,
                        ts.[shortCode] |> Array.map snd |> Typed.stack1D |> Typed.toFloatArray
                    | _ -> failwith "Expected paired output"

                // Compare series
                let sCompiled = solver Runners.dummyParameters |> fst
                let comp = sCompiled.[shortCode] |> Tensors.Typed.toFloatArray
                Expect.sequenceEqual comp man "Compiled solver should match manual discrete runner"
            

            // Differential equations: compiled solver equals differential runner
            testPropertyWithConfig Config.config "compile selects differential runner and equals manual result" <|
            fun shortCode (c: NormalFloat) ->
                // ModelForm: dx/dt = c (constant rate)
                let c' = abs c.Get * 1.<state/``time index``>

                let startDate = DateTime(2000,1,1)
                let obs = [| for i in 0..9 -> float i * 1.<state>, startDate.AddDays(float i) |] |> TimeSeries.fromNeoObservations
                let dynTF = TimeFrame.tryCreate (Map.ofList [ shortCode, obs ])

                let envOpt = None
                let t0 _ = Map.ofList [ shortCode, Typed.ofScalar 0.0<state> ]

                let eqs = Map.ofList [ shortCode, fun _ _ _ _ -> Typed.ofScalar c' ]
                let modelForm = ModelForm.DifferentialEqs eqs

                // Choose integration routine
                let i = Integration.RungeKutta.rk4
                let engineTimeMode = EstimationEngine.Continuous i
                let stepType = Solver.StepType.Internal

                let solver = Solver.SolverCompiler.compile ignore (fun c -> DateMode.Conversion.CalendarDates.toDays c |> Units.retype) modelForm engineTimeMode stepType dynTF.Value Map.empty Map.empty Map.empty envOpt (fun _ -> Solver.Exact)

                // Manual differential run along timeline
                let timeline =
                    dynTF.Value.Series
                    |> Seq.head |> fun kv -> kv.Value
                    |> TimeIndex.create startDate DateMode.Conversion.CalendarDates.toDays
                    |> Seq.map fst |> Seq.map Units.retype |> Seq.toArray 
                let envIndex = Map.empty
                let manual = Solver.SolverRunners.DifferentialTime.fixedRunner eqs i timeline envIndex t0 Runners.dummyParameters
                let manTimes, man =
                    match manual with
                    | Solver.RunnerOutput.Unpaired (times,data) ->
                        times,
                        data.[shortCode] |> Typed.toFloatArray
                    | _ -> failwith "Expected unpaired output"

                let sCompiled = solver Runners.dummyParameters |> fst
                let comp = sCompiled.[shortCode] |> Tensors.Typed.toFloatArray
                Expect.sequenceEqual comp man "Compiled solver should match manual differential runner"
        ]

    [<Tests>]
    let indexingConsistency =
        testList "Indexing consistency" [

            testCase "Environment index aligns with conditioned t0"
            <| fun _ ->

                let startDate = 1936<year>
                let t0Date = 1959<year>

                // Build environment series from 1936–1965
                let env =
                    [ startDate .. 1<year> .. t0Date + 5<year> ]
                    |> List.map (fun y -> float y, DatingMethods.Annual y)
                    |> TimeSeries.fromObservations DateMode.annualDateMode

                let idx = TimeIndex.TimeIndex(DatingMethods.Annual t0Date, DateMode.Conversion.Annual.toYears, TimeIndex.IndexMode.Exact, env)

                Expect.equal (idx.Index |> Seq.min) -23.0<``time index`` year> "1936 should be -23"
                Expect.isTrue (idx.Index |> Seq.contains 0.0<``time index`` year>) "1959 should be index 0"
                Expect.equal idx.[0.0<``time index`` year>] 1959.0 "Value at t0 should be 1959.0"
                Expect.equal idx.[1.0<``time index`` year>] 1960.0 "Value at +1 step should be 1960.0"


            testPropertyWithConfig Config.config "Discrete runner starts at x1 (t0 not included)" <| fun k ->
                let a' = 0.0 * 1.<state/state> |> Typed.ofScalar
                let b' = 0.0<state> |> Typed.ofScalar
                let eqs = Map.ofList [ k, fun _ _ _ s -> a' * s + b' ]
                let t0 _ = Map.ofList [ k, Typed.ofScalar 1.0<state> ]
                let timeline = [| 0.<``time index``>; 1.<``time index``> |]
                let series = Solver.SolverRunners.DiscreteTime.fixedRunner eqs timeline Map.empty t0 Runners.dummyParameters
                let predTimes, predData =
                    match series with
                    | Solver.RunnerOutput.Paired ts ->
                        ts.[k] |> Array.map fst,
                        ts.[k] |> Array.map snd |> Typed.stack1D |> Typed.toFloatArray
                    | _ -> failwith "Expected paired output"
                Expect.equal predData.Length 1 "Discrete series should have same length as timeline"
                Expect.equal predData.[0] 0.0<state> "First element should be x1, not t0"
                Expect.equal predTimes.[0] 1.<``time index``> "First time returned should be t1"

            // Runner returns t1 and t2 states. The baseline time index is not returned.
            testPropertyWithConfig Config.config "Differential runner returns stocks (initial not duplicated)" <| fun k ->
                let c' = 1.0<state/``time index``>
                let eqs = Map.ofList [ k, fun _ _ _ _ -> Typed.ofScalar c' ]
                let baselineValue _ = Map.ofList [ k, Typed.ofScalar 0.0<state> ]
                let timeline = [| 0.<``time index``>; 1.<``time index``>; 2.<``time index``> |]
                let series = Solver.SolverRunners.DifferentialTime.fixedRunner eqs Integration.RungeKutta.rk4 timeline Map.empty baselineValue Runners.dummyParameters
                let predTimes, predData =
                    match series with
                    | Solver.RunnerOutput.Unpaired (times,data) ->
                        times,
                        data.[k] |> Typed.toFloatArray
                    | _ -> failwith "Expected unpaired output"
                // With c'=1, increments by 1 per index; no duplicate of t0
                Expect.sequenceEqual predData [| 1.0<state>; 2.0<state> |] "Differential runner should return stocks after integration (tail)"
        ]


module Conditioning =

    [<Tests>]
    let conditioningTests =
        testList "Conditioning" [

            // Insert test here? Conditioning A can't work with only two data points.

            testPropertyWithConfig Config.config "Conditioning A (none): predictions returned for t2 .. tn" <|
                fun shortCode (PositiveInt steps) (x0: NormalFloat) ->
                    // Build dynamic series with constant increment
                    let steps = max 3 steps
                    let startDate = DateTime(2000,1,1)
                    let obs =
                        [| for i in 0 .. steps-1 ->
                            (x0.Get + float i) * 1.<state>, startDate.AddDays(float i) |]
                        |> TimeSeries.fromNeoObservations
                    let dynTF = TimeFrame.tryCreate (Map.ofList [ shortCode, obs ]) |> Option.get

                    let resolved = Solver.Conditioning.resolve Conditioning.NoConditioning dynTF None [ shortCode ] []

                    let eqs = Map.ofList [ shortCode, fun _ _ _ s -> s + Typed.ofScalar 1.0<state> ]
                    let modelForm = ModelForm.DifferenceEqs eqs

                    // Compile with trimmed dynamic frame (drop first obs)
                    let solver = Solver.SolverCompiler.compile ignore DateMode.Conversion.CalendarDates.toDays
                                    modelForm Discrete (Solver.StepType.External (resolved.ObservedForPairing |> TimeFrame.dates))
                                    resolved.StatesObservedForSolver Map.empty Map.empty Map.empty resolved.ExogenousForSolver (fun _ -> Solver.Exact)

                    let predicted = solver Runners.dummyParameters |> fst
                    let xs = predicted.[shortCode] |> Tensors.Typed.toFloatArray
                    // Expected: first returned value is x0+1, then x0+2, ...
                    let expected = [| for t in 1 .. steps-1 -> (x0.Get + float t) * 1.<state> |]
                    Expect.hasLength obs.Values (expected.Length + 1) "Predictions should be one shorter than input observations"
                    Config.sequenceEqualTol xs expected "Predicted values should match closed form for t2 onwards (no t1)"

            testPropertyWithConfig Config.config "Conditioning B (custom t0): predictions start at t0" <|
                fun shortCode (PositiveInt steps) (x0: NormalFloat) ->
                    let steps = max 2 steps
                    let startDate = DateTime(2000,1,1)
                    let obs =
                        [| for i in 0 .. steps-1 ->
                            (x0.Get + float i) * 1.<state>, startDate.AddDays(float i) |]
                        |> TimeSeries.fromNeoObservations
                    let dynTF = TimeFrame.tryCreate (Map.ofList [ shortCode, obs ]) |> Option.get

                    let customT0 = Map.ofList [ shortCode, (x0.Get - 5.0) * 1.<state> ]
                    let expected = [| for i in 1 .. steps -> (x0.Get - 5.0 + float i) * 1.<state> |]
                    let resolved = Solver.Conditioning.resolve (Conditioning.Custom customT0) dynTF None [ shortCode ] []
                    let eqs = Map.ofList [ shortCode, fun _ _ _ s -> s + Typed.ofScalar 1.0<state> ]
                    let modelForm = ModelForm.DifferenceEqs eqs

                    Expect.equal resolved.StatesObservedForSolver.StartDate (startDate.AddDays(-1.)) "Start date on conditioned data should be one day earlier"
                    Expect.equal (TimeFrame.t0 resolved.StatesObservedForSolver) customT0 "T0 was not set correctly in conditioning stage."

                    // Synthetic t0 different from first obs
                    let solver = Solver.SolverCompiler.compile ignore DateMode.Conversion.CalendarDates.toDays
                                    modelForm EstimationEngine.Discrete Solver.StepType.Internal //(Solver.StepType.External (resolved.DynamicForPairing |> TimeFrame.dates))
                                    resolved.StatesObservedForSolver resolved.MeasuresForSolver Map.empty Map.empty resolved.ExogenousForSolver (fun _ -> Solver.Exact)

                    let predicted = solver Runners.dummyParameters |> fst
                    let xs = predicted.[shortCode] |> Tensors.Typed.toFloatArray

                    // Expected: start from customT0 but with one added at each time (i.e. t1 = customT0 + 1)
                    Expect.hasLength obs.Values expected.Length "Predictions should be equal in length to original observations"
                    Config.sequenceEqualTol xs expected "Predicted values should follow from custom t0"

            testPropertyWithConfig Config.config "Conditioning C (repeat-first): repeats first without duplicate" <|
                fun shortCode (PositiveInt steps) (x0: NormalFloat) ->
                    let steps = max 2 steps
                    let startDate = DateTime(2000,1,1)
                    let obs =
                        [| for t in 1 .. steps ->
                            (x0.Get + float t) * 1.<state>, startDate.AddDays(float (t-1)) |]
                        |> TimeSeries.fromNeoObservations
                    let dynTF = TimeFrame.tryCreate (Map.ofList [ shortCode, obs ]) |> Option.get

                    let resolved = Solver.Conditioning.resolve Conditioning.RepeatFirstDataPoint dynTF None [ shortCode ] []
                    let eqs = Map.ofList [ shortCode, fun _ _ _ s -> s + Typed.ofScalar 1.0<state> ]
                    let modelForm = ModelForm.DifferenceEqs eqs

                    let solver = Solver.SolverCompiler.compile ignore DateMode.Conversion.CalendarDates.toDays
                                    modelForm EstimationEngine.Discrete (Solver.StepType.External (resolved.ObservedForPairing |> TimeFrame.dates))
                                    resolved.StatesObservedForSolver resolved.MeasuresForSolver Map.empty Map.empty resolved.ExogenousForSolver (fun _ -> Solver.Exact)

                    let predicted = solver Runners.dummyParameters |> fst
                    let xs = predicted.[shortCode] |> Tensors.Typed.toFloatArray

                    // Expected: The model is adding 1 each time-index, so it is obs[0] + 1 as first value.
                    let expected = [| for i in 1 .. steps -> fst obs.Head + float i * 1.<state> |]

                    Expect.hasLength xs steps  "Predictions should be equal in length to original observations"
                    Config.sequenceEqualTol  xs expected "Predicted values should match closed form without duplicate at t0"

        ]