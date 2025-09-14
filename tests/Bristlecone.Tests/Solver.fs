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

            // testPropertyWithConfig Config.config
            //     "discreteRunner passes env values through to equations" <| fun c envKey (envVal:NormalFloat) ->
            //         let eq : ModelSystem.GenericModelEquation<``time index``> =
            //             fun _ env _ _ -> env.[envKey] |> Typed.retype
            //         let eqs = [ c, eq ] |> Map.ofList
            //         let times = Array.init 10 (fun i -> DateTime(1980,01,01).AddYears i )
            //         let timeline = Array.init 10 (fun i -> float i * 1.<``time index``> )

            //         let fakeTimeSeries = times |> Array.map(fun t -> 1.<environment>, t) |> TimeSeries.fromNeoObservations
            //         let envIndex = TimeIndex.create (DateTime(1980,01,01)) y fakeTimeSeries
            //         let t0 = [ c, Typed.ofScalar 0.0<state> ] |> Map.ofList
            //         let point = Typed.ofVector [| nan * 1.<parameter> |]
            //         let result = DiscreteTime.discreteRunner eqs timeline envIndex t0 point
            //         let arr = result.[c] |> Typed.toFloatArray |> Array.map Units.removeUnitFromFloat
            //         Expect.sequenceEqual arr [| envVal.Get; envVal.Get |] "Env value should flow through unchanged"

            testCase "Empty eqs map produces empty output" <| fun _ ->
                failwith "Not implemented"

            testCase "Single-step timeline produces single output element" <| fun _ ->
                failwith "Not implemented"
        ]


    // [<Tests>]
    // let differentialTimeTests =
    //     testList "DifferentialTime runner" [

    //         testCase "fixedStep calls integration routine and tails the result" <| fun _ ->
    //             failwith "Not implemented"

    //         testCase "differentialRunner passes correct start and end times to fixedStep" <| fun _ ->
    //             failwith "Not implemented"

    //         testCase "Integration routine output shape matches expected keys and length" <| fun _ ->
    //             failwith "Not implemented"
    //     ]


    // [<Measure>] type testModelUnit

    // // 1. A no‑op logger
    // let logTo _ = ()

    // // 2. A trivial unit‑conversion
    // let toModelUnits (_: int) = 1.0<testModelUnit>

    // // 3. A dummy model form that matches the engineTimeMode you want to exercise
    // let modelEquations = DifferentialEqs Map.empty  // or DifferenceEqs dummyEqs

    // // 4. Engine time mode to match the model form
    // let engineTimeMode = Continuous i // or Discrete

    // // 5. Step type
    // let stepType = Solver.StepType.External

    // // 6. A minimal dynamicSeries with a single headSeries
    // let headSeries =
    //     { new obj() with
    //         member _.DateMode = Unchecked.defaultof<_> }
    //     |> fun hs -> { Value = hs }
    // let dynamicSeries =
    //     { StartDate = Unchecked.defaultof<_>
    //     Series = seq { yield headSeries } }

    // // 7. No environment
    // let environment = None

    // // 8. A minimal t0 map
    // let t0 =
    //     [ ShortCode.create "x" |> Option.get,
    //     Typed.ofScalar 0.0<state> ]
    //     |> Map.ofList

    // // 9. Stub out all the helper functions the compiler calls
    // let decideIntegrationResolution _ _ = Unchecked.defaultof<_>, Unchecked.defaultof<_>
    // let computeFactor _ _ _ = 1.0
    // let wrapModelForm _ m = m
    // let buildEnvIndex _ _ _ = Unchecked.defaultof<_>
    // let buildKeepMask _ _ _ _ = [||]
    // module SolverRunners =
    //     module DifferentialTime =
    //         let differentialRunner _ _ _ _ _ = Map.empty
    //     module DiscreteTime =
    //         let discreteRunner _ _ _ _ _ = Map.empty


    [<Tests>]
    let compilationTests =
        testList "Compiling a solver" [

            testCase "Integration routine output shape matches expected keys and length" <| fun _ ->
                failwith "Not implemented"
        ]
