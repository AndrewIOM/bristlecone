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
        testList "DiscreteTime runner" [

            testPropertyWithConfig Config.config
                "stepOnce applies each equation to the matching state" <| fun c (t0:NormalFloat) (v:NormalFloat) ->
                let f _ _ _ (state:TypedTensor<Scalar,state>) = state + Typed.ofScalar (v.Get * 1.<state>)
                let eqs = [ c, f ] |> Map.ofList
                let p = Typed.ofVector [| nan * 1.<parameter> |]
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
                    let timeline = Array.init steps (fun i -> float i * 1.<``time index``>)
                    let envStream = Array.init steps (fun _ -> Map.empty)
                    let t0 = [ c, Typed.ofScalar (start.Get * 1.<state>) ] |> Map.ofList
                    let p = Typed.ofVector [| nan * 1.<parameter> |]
                    let result = DiscreteTime.iterateDifference eqs timeline envStream t0 p
                    let arr = result.[c] |> Typed.toFloatArray
                    let expected = Array.init steps (fun i -> start.Get + inc.Get * float (i+1))
                    Config.sequenceEqualTol arr expected "Accumulated values do not match expected sequence"

            testPropertyWithConfig Config.config
                "discreteRunner passes env values through to equations" <| fun c envKey (envVal:NormalFloat) ->
                    let eqs : CodedMap<StateEquation<Time.``time index``>> =
                        [ c, fun _ (env: CodedMap<TypedTensor<Scalar,environment>>) _ _ -> env.[envKey] |> Typed.retype ] |> Map.ofList
                    let times = Array.init 10 (fun i -> DateTime(1980,01,01).AddYears i )
                    let timeline = Array.init 10 (fun i -> float i * 1.<``time index``> )

                    let startDate = DateTime(1980,01,01)
                    let fakeTimeSeries = times |> Array.map(fun t -> envVal.Get * 1.<environment>, t) |> TimeSeries.fromNeoObservations
                    let dataT0 = [ c, Typed.ofScalar 0.0<state> ] |> Map.ofList
                    let point = Typed.ofVector [| nan * 1.<parameter> |]
                    
                    let index = TimeIndex.TimeIndex(startDate, (Resolution.Years (PositiveInt.create 1<year> |> Option.get)), TimeIndex.IndexMode.Exact, fakeTimeSeries)
                    let indexMap = Map.ofList [ envKey, index ]

                    let result = DiscreteTime.discreteRunner eqs timeline indexMap dataT0 point
                    let arr = result.[c] |> Typed.toFloatArray |> Array.map Units.removeUnitFromFloat
                    let expected = Array.init 10 (fun _ -> envVal.Get)
                    Expect.sequenceEqual arr expected "Env value should flow through unchanged"

            // Checks that a discrete model x_{t+1} = a*x_t + b matches the closed form.
            // The runner does not include t0 in the output, only t1 onwards.
            testPropertyWithConfig Config.config "Discrete affine matches closed form" <|
                fun shortCode (a: NormalFloat) (b: NormalFloat) (x0: NormalFloat) (PositiveInt steps) ->
                    let a' = abs a.Get * 1.<state/state>
                    let b' = abs b.Get * 1.<state>
                    let x0' = x0.Get * 1.<state>
                    let eqs = Map.ofList [ shortCode, fun _ _ _ state ->
                        Typed.ofScalar (a' * Typed.toFloatScalar state + b') ]
                    let timeline = [| for i in 0 .. steps -> float i * 1.<``time index``> |]
                    let envIndex = Map.empty
                    let t0 = Map.ofList [ shortCode, Typed.ofScalar x0' ]
                    let series = DiscreteTime.discreteRunner eqs timeline envIndex t0 dummyParameters
                    let predicted: float<state>[] = series.[shortCode] |> Tensors.Typed.toFloatArray
                    let closedForm (t: int) : float<state> =
                        if abs (float a' - 1.0) < 1e-12 then
                            x0' + b' * float t
                        else
                            x0' * (float a' ** float t) +
                            (b' * (1. - float a' ** float t) / (1. - float a'))
                    let expected = predicted |> Array.mapi (fun i x -> i, x)
                    expected
                    |> Seq.iter(fun (i,pred) ->
                        Expect.floatClose Accuracy.veryHigh (Units.removeUnitFromFloat pred) (Units.removeUnitFromFloat <| closedForm (i+1)) ""
                    )
        ]

    let integrationMethod = Integration.RungeKutta.rk4

    [<Tests>]
    let differentialTests =
        testList "Differential time runner" [

            // Check increments are all ~ c * Δt
            testPropertyWithConfig Config.config "ODE constant-rate produces linear stock increments"
            <| fun shortCode (c: NormalFloat) (PositiveInt steps) ->
                    let c = if c.Get < 0. then c.Get * -1.<state> else c.Get * 1.<state>
                    let c' = c / 1.<``time index``>
                    let t0 = Map.ofList [ shortCode, Typed.ofScalar 0.0<state> ]
                    let eqs = Map.ofList [ shortCode, fun _ _ _ _ -> Typed.ofScalar c' ]
                    let timeline = [| for i in 0 .. steps -> float i * 1.<``time index``> |]
                    let series = DifferentialTime.differentialRunner eqs integrationMethod timeline Map.empty t0 dummyParameters
                    let xs = series.[shortCode] |> Tensors.Typed.toFloatArray
                    let deltas = xs |> Array.pairwise |> Array.map (fun (a,b) -> b - a)
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
                let obs = [| for i in 0..9 -> float i * 1.<state>, startDate.AddDays(float i) |] |> TimeSeries.fromNeoObservations
                let dynTF = TimeFrame.tryCreate (Map.ofList [ shortCode, obs ]) |> Option.get

                // No environment
                let envOpt = None

                // Initial state
                let t0 = Map.ofList [ shortCode, Typed.ofScalar (x0.Get * 1.<state>) ]

                let eqs =
                    Map.ofList [ shortCode,
                        fun _ _ _ (state: TypedTensor<Scalar,state>) ->
                            Typed.ofScalar (a' * Typed.toFloatScalar state + b') ]
                let modelForm = ModelForm.DifferenceEqs eqs

                // Engine configuration
                let engineTimeMode = EstimationEngine.Discrete
                let stepType = Solver.StepType.Internal

                // Compile configured solver
                let toModelUnits (ts: TimeSpan) = ts.TotalDays * 1.<``time index``>
                let solver = Solver.SolverCompiler.compile ignore toModelUnits modelForm engineTimeMode stepType dynTF envOpt t0

                // Manual discrete run along the internal timeline
                let timeline =
                    dynTF.Series
                    |> Seq.head |> fun kv -> kv.Value
                    |> TimeIndex.create startDate (Resolution.Days (PositiveInt.create 1<day> |> Option.get))
                    |> Seq.map fst |> Seq.toArray
                let envIndex = Map.empty
                let manual = Solver.SolverRunners.DiscreteTime.discreteRunner eqs timeline envIndex t0 Runners.dummyParameters

                // Compare series
                let sCompiled = solver Runners.dummyParameters
                let sManual   = manual
                let comp = sCompiled.[shortCode] |> Tensors.Typed.toFloatArray
                let man  = sManual.[shortCode]   |> Tensors.Typed.toFloatArray
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
                let t0 = Map.ofList [ shortCode, Typed.ofScalar 0.0<state> ]

                let eqs = Map.ofList [ shortCode, fun _ _ _ _ -> Typed.ofScalar c' ]
                let modelForm = ModelForm.DifferentialEqs eqs

                // Choose integration routine
                let i = Integration.RungeKutta.rk4
                let engineTimeMode = EstimationEngine.Continuous i
                let stepType = Solver.StepType.Internal

                let toModelUnits (ts: TimeSpan) = ts.TotalDays * 1.<``time index``>
                let solver = Solver.SolverCompiler.compile ignore toModelUnits modelForm engineTimeMode stepType dynTF.Value envOpt t0

                // Manual differential run along timeline
                let timeline =
                    dynTF.Value.Series
                    |> Seq.head |> fun kv -> kv.Value
                    |> TimeIndex.create startDate (Resolution.Days (PositiveInt.create 1<day> |> Option.get))
                    |> Seq.map fst |> Seq.toArray
                let envIndex = Map.empty
                let manual = Solver.SolverRunners.DifferentialTime.differentialRunner eqs i timeline envIndex t0 Runners.dummyParameters

                let sCompiled = solver Runners.dummyParameters
                let comp = sCompiled.[shortCode] |> Tensors.Typed.toFloatArray
                let man  = manual.[shortCode]   |> Tensors.Typed.toFloatArray
                Expect.sequenceEqual comp man "Compiled solver should match manual differential runner"
        ]

    [<Tests>]
    let indexingConsistency =
        testList "Indexing consistency" [

            testPropertyWithConfig Config.config "Discrete runner starts at x1 (t0 not included)" <| fun k ->
                let a' = 0.0 * 1.<state/state> |> Typed.ofScalar
                let b' = 0.0<state> |> Typed.ofScalar
                let eqs = Map.ofList [ k, fun _ _ _ s -> a' * s + b' ]
                let t0 = Map.ofList [ k, Typed.ofScalar 1.0<state> ]
                let timeline = [| 0.<``time index``>; 1.<``time index``> |]
                let series = Solver.SolverRunners.DiscreteTime.discreteRunner eqs timeline Map.empty t0 Runners.dummyParameters
                let xs = series.[k] |> Tensors.Typed.toFloatArray
                Expect.equal xs.Length 2 "Discrete series should have same length as timeline"
                Expect.equal xs.[0] 0.0<state> "First element should be x1, not t0"

            // Runner returns t0, t1, and t2 states. It does not remove any conditioning points here;
            // that happens in the built solver.
            testPropertyWithConfig Config.config "Differential runner returns stocks (initial not duplicated)" <| fun k ->
                let c' = 1.0<state/``time index``>
                let eqs = Map.ofList [ k, fun _ _ _ _ -> Typed.ofScalar c' ]
                let t0 = Map.ofList [ k, Typed.ofScalar 0.0<state> ]
                let timeline = [| 0.<``time index``>; 1.<``time index``>; 2.<``time index``> |]
                let series = Solver.SolverRunners.DifferentialTime.differentialRunner eqs Integration.RungeKutta.rk4 timeline Map.empty t0 Runners.dummyParameters
                let xs = series.[k] |> Tensors.Typed.toFloatArray
                // With c'=1, increments by 1 per index; no duplicate of t0
                Expect.sequenceEqual xs [| 1.0<state>; 2.0<state>; 3.0<state> |] "Differential runner should return stocks after integration (tail)"
        ]


module Conditioning =

    [<Tests>]
    let conditioningTests =
        testList "Conditioning" [

            // Insert test here? Conditioning A can't work with only two data points.

            testPropertyWithConfig Config.config "Conditioning A: predictions start at t1" <|
                fun shortCode (PositiveInt steps) (x0: NormalFloat) ->
                    let steps = max 3 steps // need at least three observations
                    // Build dynamic series with constant increment
                    let startDate = DateTime(2000,1,1)
                    let obs =
                        [| for i in 0 .. steps-1 ->
                            (x0.Get + float i) * 1.<state>, startDate.AddDays(float i) |]
                        |> TimeSeries.fromNeoObservations
                    let dynTF = TimeFrame.tryCreate (Map.ofList [ shortCode, obs ]) |> Option.get

                    let envOpt = None
                    let t0State = Map.ofList [ shortCode, Typed.ofScalar (x0.Get * 1.<state>) ]
                    let eqs = Map.ofList [ shortCode, fun _ _ _ s -> s + Typed.ofScalar 1.0<state> ]
                    let modelForm = ModelForm.DifferenceEqs eqs

                    // Compile with trimmed dynamic frame (drop first obs)
                    let trimmedDyn = TimeFrame.dropFirstObservation dynTF
                    let solver = Solver.SolverCompiler.compile ignore (fun (ts:TimeSpan) -> ts.TotalDays * 1.<``time index``>)
                                    modelForm EstimationEngine.Discrete Solver.StepType.External
                                    trimmedDyn envOpt t0State

                    let predicted = solver Runners.dummyParameters
                    let obsTimes = obs |> TimeSeries.toObservations |> Seq.map snd |> Seq.skip 1 |> Seq.toArray
                    let predTimes = trimmedDyn.Series.[shortCode] |> TimeSeries.toObservations |> Seq.map snd |> Seq.toArray
                    Expect.sequenceEqual predTimes obsTimes "Predicted timestamps should start at t1"

            testPropertyWithConfig Config.config "Conditioning B: custom t0, predictions start at t0" <|
                fun shortCode (PositiveInt steps) (x0: NormalFloat) ->
                    let steps = max 2 steps
                    let startDate = DateTime(2000,1,1)
                    let obs =
                        [| for i in 0 .. steps-1 ->
                            (x0.Get + float i) * 1.<state>, startDate.AddDays(float i) |]
                        |> TimeSeries.fromNeoObservations
                    let dynTF = TimeFrame.tryCreate (Map.ofList [ shortCode, obs ]) |> Option.get

                    let envOpt = None
                    // Synthetic t0 different from first obs
                    let t0State = Map.ofList [ shortCode, Typed.ofScalar ((x0.Get - 5.0) * 1.<state>) ]
                    let eqs = Map.ofList [ shortCode, fun _ _ _ s -> s + Typed.ofScalar 1.0<state> ]
                    let modelForm = ModelForm.DifferenceEqs eqs

                    let solver = Solver.SolverCompiler.compile ignore (fun (ts: TimeSpan) -> ts.TotalDays * 1.<``time index``>)
                                    modelForm EstimationEngine.Discrete Solver.StepType.External
                                    dynTF envOpt t0State

                    let predicted = solver Runners.dummyParameters
                    let obsTimes = obs |> TimeSeries.toObservations |> Seq.map snd |> Seq.toArray
                    let predTimes = dynTF.Series.[shortCode] |> TimeSeries.toObservations |> Seq.map snd |> Seq.toArray
                    Expect.sequenceEqual predTimes obsTimes "Predicted timestamps should start at t0"

            testPropertyWithConfig Config.config "Conditioning C: repeat-first without duplicate" <|
                fun shortCode (PositiveInt steps) (x0: NormalFloat) ->
                    let steps = max 2 steps
                    let startDate = DateTime(2000,1,1)
                    let obs =
                        [| for i in 0 .. steps-1 ->
                            (x0.Get + float i) * 1.<state>, startDate.AddDays(float i) |]
                        |> TimeSeries.fromNeoObservations
                    let dynTF = TimeFrame.tryCreate (Map.ofList [ shortCode, obs ]) |> Option.get

                    let envOpt = None
                    let t0State = Map.ofList [ shortCode, Typed.ofScalar (x0.Get * 1.<state>) ]
                    let eqs = Map.ofList [ shortCode, fun _ _ _ s -> s + Typed.ofScalar 1.0<state> ]
                    let modelForm = ModelForm.DifferenceEqs eqs

                    let solver = Solver.SolverCompiler.compile ignore (fun (ts: TimeSpan) -> ts.TotalDays * 1.<``time index``>)
                                    modelForm EstimationEngine.Discrete Solver.StepType.External
                                    dynTF envOpt t0State

                    let predicted = solver Runners.dummyParameters
                    let predTimes = dynTF.Series.[shortCode] |> TimeSeries.toObservations |> Seq.map snd |> Seq.toArray
                    // Ensure no duplicate timestamps in predicted series
                    let distinctPredTimes = predTimes |> Array.distinct
                    Expect.equal predTimes distinctPredTimes "Predicted series should not contain duplicate timestamps"


        ]