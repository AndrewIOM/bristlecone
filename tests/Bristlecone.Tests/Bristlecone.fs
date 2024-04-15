module BristleconeTests

open Bristlecone
open Expecto
open FsCheck
open Bristlecone.EstimationEngine

// Checks floats are equal, but accounting for nan <> nan
let expectSameFloat a b message =
    Expect.isTrue (LanguagePrimitives.GenericEqualityER a b) message

let expectSameFloatList a b message =
    Seq.zip a b
    |> Seq.iter(fun (a,b) -> expectSameFloat a b message)



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

let defaultEngine =
    { TimeHandling = Continuous <| Integration.MathNet.integrate
      OptimiseWith = Optimisation.None.none
      LogTo = ignore
      Random = MathNet.Numerics.Random.MersenneTwister(1000, true)
      Conditioning = Conditioning.RepeatFirstDataPoint }

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

            ]

module ``Fit`` =

    [<Tests>]
    let conditioningTest =
        testList
            "Conditioning"
            [

                testPropertyWithConfig Config.config "Repeating first data point sets t0 as t1"
                <| fun time resolution (data: float list) ->
                    if data.IsEmpty || data.Length = 1 then ()
                    else
                        let data =
                            [ (Language.code "x").Value,
                                Time.TimeSeries.fromSeq time (Time.FixedTemporalResolution.Years resolution) data ]
                            |> Map.ofList

                        let result = Bristlecone.Fit.t0 data Conditioning.RepeatFirstDataPoint ignore
                        expectSameFloatList
                            (result) (data |> Map.map (fun k v -> v.Values |> Seq.head)) "t0 did not equal t1"

              // testProperty "t0 is set as a custom point when specified" <| fun () ->
              //     false
              ]

    [<Tests>]
    let fitTests =
        testList
            "Model-fitting"
            [

              testList
                  "Establish common timelines"
                  [

                    testPropertyWithConfig Config.config "Core fitting functions are reproducible"
                    <| fun b1 b2 seedNumber (obs: float list) startDate months ->
                        if System.Double.IsNaN b1 || b1 = infinity || b1 = -infinity ||
                            System.Double.IsNaN b2 || b2 = infinity || b2 = -infinity
                        then ()
                        else
                            let data: CodedMap<Time.TimeSeries.TimeSeries<float>> =
                                [ (Language.code "x").Value,
                                Time.TimeSeries.fromSeq startDate (Time.FixedTemporalResolution.Months months) obs ]
                                |> Map.ofList

                            let result =
                                Expect.wantOk
                                    (Bristlecone.fit defaultEngine defaultEndCon data (TestModels.constant b1 b2))
                                    "Fitting did not happen successfully."

                            let result2 =
                                Expect.wantOk
                                    (Bristlecone.fit
                                        { defaultEngine with
                                            Random = MathNet.Numerics.Random.MersenneTwister(seedNumber, true) }
                                        defaultEndCon
                                        data
                                        (TestModels.constant b1 b2))
                                    ""

                            expectSameFloat result.Likelihood result2.Likelihood "Different likelihoods"
                            expectSameFloat result.InternalDynamics result.InternalDynamics "Different internal dynamics"
                            expectSameFloat result.Parameters result2.Parameters "Different parameters"
                            expectSameFloatList (result.Series |> Seq.collect(fun kv -> kv.Value.Values |> Seq.map(fun v -> v.Fit))) (result2.Series |> Seq.collect(fun kv -> kv.Value.Values |> Seq.map(fun v -> v.Fit))) "Different expected series"
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

                    testPropertyWithConfig Config.config "Positive only parameter is transformed when optimising in transformed space"
                    <| fun (data: float list) startDate months (b1: NormalFloat) (b2: NormalFloat) ->
                        let testModel b1 b2 = TestModels.twoEquationConstant Language.notNegative b1 b2
                        if b1.Get = b2.Get || b1.Get = 0. || b2.Get = 0.
                        then
                            Expect.throws (fun () -> testModel b1.Get b2.Get |> ignore) "Model compiled despite having no difference between parameter bounds"
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
                                |> Seq.map(fun c -> c, Time.TimeSeries.fromSeq startDate (Time.FixedTemporalResolution.Months months) data)
                                |> Map.ofSeq

                            let result =
                                Expect.wantOk
                                    (Bristlecone.fit engine defaultEndCon data (testModel b1 b2))
                                    "Errored when should be OK"

                            Expect.equal
                                inOptimMin
                                (min (log(b1)) (log(b2)))
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
