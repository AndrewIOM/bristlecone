module BristleconeTests

open Bristlecone
open Expecto
open FsCheck
open Bristlecone.EstimationEngine
open Bristlecone.Time

// Checks floats are equal, but accounting for nan <> nan
let expectSameFloat a b message =
    Expect.isTrue (LanguagePrimitives.GenericEqualityER a b) message

let expectSameFloatList a b message =
    Seq.zip a b |> Seq.iter (fun (a, b) -> expectSameFloat a b message)



module TestModels =

    open Bristlecone.Language

    let constant bound1 bound2 =
        Model.empty
        |> Model.addEquation "x" (Parameter "a")
        |> Model.estimateParameter "a" noConstraints (min bound1 bound2) (max bound1 bound2)
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ "x" ])
        |> Model.compile

    let twoEquationConstant cons bound1 bound2 =
        Model.empty
        |> Model.addEquation "x" (Parameter "a")
        |> Model.addEquation "y" (Parameter "a")
        |> Model.estimateParameter "a" cons (min bound1 bound2) (max bound1 bound2)
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ "x"; "y" ])
        |> Model.compile

    let logistic kMax rMax environmentMultiplier =
        Model.empty
        |> Model.addEquation "x" (environmentMultiplier * Parameter "r" * This * (Constant 1. - (This / Parameter "K")))
        |> Model.estimateParameter "K" notNegative 30.0 kMax
        |> Model.estimateParameter "r" notNegative 0.75 rMax
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ "x" ])
        |> Model.compile


let defaultEngine =
    { TimeHandling = Continuous <| Integration.MathNet.integrate
      OptimiseWith = Optimisation.None.none
      LogTo = ignore
      Random = MathNet.Numerics.Random.MersenneTwister(1000, true)
      Conditioning = Conditioning.RepeatFirstDataPoint Conditioning.ConditionTimeline.ObservedData }

let defaultEndCon = Optimisation.EndConditions.afterIteration 1000

module ``Objective creation`` =

    [<Tests>]
    let initialBounds =
        testList
            "Objective"
            [

              // testProperty "Time-series are paired to correct years" <| fun x ->
              //     false

              // testProperty "Throws when time-series are not the same length" <| fun x ->
              //     false

              testPropertyWithConfig Config.config "Likelihood functions use 'real' parameter values"
              <| fun shouldTransform (data: float list) (b1: NormalFloat) (b2: NormalFloat) ->

                  // Returns the parameter value
                  let fakeLikelihood: Bristlecone.ModelSystem.LikelihoodFn =
                      fun paramAccessor data -> paramAccessor.Get "a"

                  if b1.Get = b2.Get || b1.Get = 0. || b2.Get = 0. then
                      ()
                  else
                      let b1 = if b1.Get < 0. then b1.Get * -1. else b1.Get
                      let b2 = if b2.Get < 0. then b2.Get * -1. else b2.Get

                      let mode =
                          if shouldTransform then
                              Language.notNegative
                          else
                              Language.noConstraints

                      let model =
                          Language.Model.empty
                          |> Language.Model.addEquation "x" (Language.Parameter "a")
                          |> Language.Model.estimateParameter "a" mode (min b1 b2) (max b1 b2)
                          |> Language.Model.useLikelihoodFunction fakeLikelihood
                          |> Language.Model.compile

                      let testObjective =
                          Objective.create
                              model
                              (fun _ -> Map.empty)
                              (fun _ _ _ -> [| 2.0 |])
                              ([ (ShortCode.create "x").Value, data |> List.toArray ] |> Map.ofList)

                      Expect.floatClose
                          Accuracy.high
                          (testObjective
                              [| (if shouldTransform then
                                      Parameter.transformOut mode b1
                                  else
                                      b1) |])
                          b1
                          "The likelihood function did not retrieve the 'real' parameter value"

              ]

module ``Fit`` =

    [<Tests>]
    let conditioningTest =
        testList
            "Conditioning"
            [

              testPropertyWithConfig Config.config "Repeating first data point sets t0 as t1"
              <| fun time resolution (data: float list) ->
                    let data =
                        [ (Language.code "x").Value,
                        Time.TimeSeries.fromSeq time (Time.FixedTemporalResolution.Years resolution) data ]
                        |> Map.ofList

                    let result = Bristlecone.Fit.t0 data (Conditioning.RepeatFirstDataPoint Conditioning.ConditionTimeline.ObservedData) ignore

                    expectSameFloatList
                        (fst result)
                        (data |> Map.map (fun k v -> v.Values |> Seq.head))
                        "t0 did not equal t1"

              // testProperty "t0 is set as a custom point when specified" <| fun () ->
              //     false
              ]

    [<Tests>]
    let fitTests =
        testList
            "Bristlecone.fit"
            [

              testList
                "Scenarios" [

                    testPropertyWithConfig Config.config "Estimation result applying high-res environmental data"
                    <| fun date (data: TimeSeries.Observation<float> list) (dailyResolution:PositiveInt.PositiveInt) ->

                        // Otherwise computation gets too much for a simple test:
                        let data = data |> List.map fst |> List.truncate 50

                        let actualData = data |> TimeSeries.fromSeq date (Resolution.Days dailyResolution)
                        let c = ShortCode.create "x" |> Option.get
                        let allData = 
                            [ (ShortCode.create "env-val").Value, 
                                TimeSeries.fromSeq (date.AddDays(-dailyResolution.Value)) (FixedTemporalResolution.Days ((PositiveInt.create 1).Value)) 
                                    ([ 1. .. float (dailyResolution.Value + (data.Length * dailyResolution.Value)) ] |> List.map(fun d -> d * 0.05)) ]
                            |> Map.ofList
                            |> Map.add c actualData

                        let result =
                            Expect.wantOk
                                (Bristlecone.tryFit defaultEngine (Optimisation.EndConditions.afterIteration 1) allData (TestModels.logistic 40.0 2.0 (Language.Environment "env-val")))
                                "Fitting did not happen successfully."

                        Expect.equal result.Series.[c].Length actualData.Length
                            "The output time-series does not have the same length as the input"

                        Expect.isSome result.InternalDynamics
                            "There should be internal dynamics, as high-resolution environmental data was input"

                        let daysInTimeline = (actualData.EndDate - (snd actualData.StartDate)).Days
                        Expect.equal (result.InternalDynamics.Value.[c].Length - 1) daysInTimeline // TODO WHY -1??
                            "The output (internal) time-series does not have the correct number of days"

                        Expect.sequenceEqual (result.Series.[c].Values |> Seq.map(fun m -> m.Obs)) actualData.Values
                            "The output time-series was not the input time-series"

                ]

              testList
                  "Establish common timelines"
                  [

                    testPropertyWithConfig Config.config "Core fitting functions are reproducible"
                    <| fun b1 b2 seedNumber (obs: float list) startDate months ->
                        if
                            System.Double.IsNaN b1
                            || b1 = infinity
                            || b1 = -infinity
                            || System.Double.IsNaN b2
                            || b2 = infinity
                            || b2 = -infinity
                        then
                            ()
                        else
                            let data: CodedMap<Time.TimeSeries.TimeSeries<float>> =
                                [ (Language.code "x").Value,
                                  Time.TimeSeries.fromSeq startDate (Time.FixedTemporalResolution.Months months) obs ]
                                |> Map.ofList

                            let result =
                                Expect.wantOk
                                    (Bristlecone.tryFit defaultEngine defaultEndCon data (TestModels.constant b1 b2))
                                    "Fitting did not happen successfully."

                            let result2 =
                                Expect.wantOk
                                    (Bristlecone.tryFit
                                        { defaultEngine with
                                            Random = MathNet.Numerics.Random.MersenneTwister(seedNumber, true) }
                                        defaultEndCon
                                        data
                                        (TestModels.constant b1 b2))
                                    "Fitting did not happen successfully."

                            expectSameFloat result.Likelihood result2.Likelihood "Different likelihoods"

                            expectSameFloat
                                result.InternalDynamics
                                result2.InternalDynamics
                                "Different internal dynamics"

                            expectSameFloat result.Parameters result2.Parameters "Different parameters"

                            expectSameFloatList
                                (result.Series
                                 |> Seq.collect (fun kv -> kv.Value.Values |> Seq.map (fun v -> v.Fit)))
                                (result2.Series
                                 |> Seq.collect (fun kv -> kv.Value.Values |> Seq.map (fun v -> v.Fit)))
                                "Different expected series"

                            expectSameFloat result.Trace result2.Trace "Different traces"

                    // testProperty "Time-series relating to model equations must overlap"
                    // <| fun t1 t2 resolution data1 data2 ->
                    //     let ts =
                    //         [ Time.TimeSeries.fromSeq t1 (Time.FixedTemporalResolution.Years resolution) data1
                    //           Time.TimeSeries.fromSeq t2 (Time.FixedTemporalResolution.Years resolution) data2 ]

                    //     let result =
                    //         Bristlecone.Fit.observationsToCommonTimeFrame
                    //             (TestModels.twoEquationConstant Language.noConstraints 0. 1.).Equations
                    //         |> ignore

                    //     result
                    //     false

                    // testProperty "Time-series relating to model equations are clipped to common (overlapping) time" <| fun () ->
                    //     false

                    // testProperty "Time-series of external forcings are clipped to common (overlapping) time" <| fun () ->
                    //     false

                    // testProperty "External forcing series must cover the whole common period of dynamical series" <| fun () ->
                    //     false
                    ]

              testList
                  "Setting up parameter constraints"
                  [

                    testPropertyWithConfig
                        Config.config
                        "Positive only parameter is transformed when optimising in transformed space"
                    <| fun (data: float list) startDate months (b1: NormalFloat) (b2: NormalFloat) ->
                        let testModel b1 b2 =
                            TestModels.twoEquationConstant Language.notNegative b1 b2

                        if b1.Get = b2.Get || b1.Get = 0. || b2.Get = 0. then
                            Expect.throws
                                (fun () -> testModel b1.Get b2.Get |> ignore)
                                "Model compiled despite having no difference between parameter bounds"
                        else
                            let b1 = if b1.Get < 0. then b1.Get * -1. else b1.Get
                            let b2 = if b2.Get < 0. then b2.Get * -1. else b2.Get
                            let mutable inOptimMin = nan

                            let optimTest =
                                InTransformedSpace
                                <| fun _ _ _ domain _ f ->
                                    let point = [| for (min, _, _) in domain -> min |]
                                    inOptimMin <- point.[0]
                                    [ f point, point ]

                            let engine =
                                { defaultEngine with
                                    OptimiseWith = optimTest }

                            let data =
                                [ (ShortCode.create "x").Value; (ShortCode.create "y").Value ]
                                |> Seq.map (fun c ->
                                    c,
                                    Time.TimeSeries.fromSeq startDate (Time.FixedTemporalResolution.Months months) data)
                                |> Map.ofSeq

                            let result =
                                Expect.wantOk
                                    (Bristlecone.tryFit engine defaultEndCon data (testModel b1 b2))
                                    "Errored when should be OK"

                            Expect.equal
                                inOptimMin
                                (min (log (b1)) (log (b2)))
                                "The lower bound was not transformed inside the optimiser" ]

              ]


//     [<Tests>]
//     let fitTests =
//         testList "Time-invariant models" [

//             testProperty "MLE cannot fall outside given parameter constraints" <| fun x ->
//                 false

//             testProperty "It works" <| fun x ->

//                 let fit =
//                     Bristlecone.Invariant.fitWithoutTime
//                         Optimisation.None.passThrough
//                         NoConditioning
//                         Detached
//                         ignore
//                         (Optimisation.EndConditions.afterIteration 1000)

//                 let f x = 2.

//                 let model = {
//                     Equations = [ shortCode "f"; f ]
//                 }

//                 let r = fit model
//                 printfn "R is %A" r
//                 ()




//         ]
