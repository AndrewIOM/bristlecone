module BristleconeTests

open Bristlecone
open Expecto
open FsCheck
open Bristlecone.EstimationEngine

// Checks floats are equal, but accounting for nan <> nan
let expectSameFloat a b message =
    Expect.isTrue (LanguagePrimitives.GenericEqualityER a b) message

let expectSameFloatList a b message =
    Seq.zip a b |> Seq.iter (fun (a, b) -> expectSameFloat a b message)


module TestModels =

    open Bristlecone.Language

    let constant bound1 bound2 =
        let X = state "X"
        let a = parameter "a" noConstraints (min bound1 bound2) (max bound1 bound2)        
        Model.empty
        |> Model.addRateEquation X (P a)
        |> Model.estimateParameter a
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ X.Code ])
        |> Model.compile

    let twoEquationConstant cons bound1 bound2 =
        let X = state "X"
        let Y = state "Y"
        let a = parameter "a" cons (min bound1 bound2) (max bound1 bound2)        
        Model.empty
        |> Model.addRateEquation X (P a)
        |> Model.addRateEquation Y (P a)
        |> Model.estimateParameter a
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ X.Code; Y.Code ])
        |> Model.compile


let indexBySpan (ts : System.TimeSpan) = float ts.Days * 1.<Time.day> * 12.<Time.``time index``/Time.day>

let defaultEngine () =
    { TimeHandling = Continuous <| Integration.RungeKutta.rk4
      OptimiseWith = Optimisation.None.none
      LogTo = ignore
      ToModelTime = indexBySpan
      Random = MathNet.Numerics.Random.MersenneTwister(1000, true)
      Conditioning = Conditioning.RepeatFirstDataPoint }

let defaultEndCon = Optimisation.EndConditions.atIteration 1000<iteration>


module ``Fit`` =

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
                            let data =
                                [ (Language.code "X").Value,
                                  Time.TimeSeries.fromSeq
                                      Time.DateMode.calendarDateMode
                                      startDate
                                      (Time.Resolution.FixedTemporalResolution.Months months)
                                      obs ]
                                |> Map.ofList

                            let result =
                                Expect.wantOk
                                    (Bristlecone.tryFit (defaultEngine()) defaultEndCon data (TestModels.constant b1 b2))
                                    "Fitting did not happen successfully."

                            let result2 =
                                Expect.wantOk
                                    (Bristlecone.tryFit
                                        { defaultEngine () with
                                            Random = MathNet.Numerics.Random.MersenneTwister(seedNumber, true) }
                                        defaultEndCon
                                        data
                                        (TestModels.constant b1 b2))
                                    ""

                            expectSameFloat result.Likelihood result2.Likelihood "Different likelihoods"

                            expectSameFloat
                                result.InternalDynamics
                                result.InternalDynamics
                                "Different internal dynamics"

                            // expectSameFloat result.Parameters result2.Parameters "Different parameters"

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
                                "Model compiled despite having no difference between parameter bounds or zero bound."
                        else
                            let b1 = if b1.Get < 0. then b1.Get * -1. else b1.Get
                            let b2 = if b2.Get < 0. then b2.Get * -1. else b2.Get
                            let mutable inOptimMin = nan

                            let optimTest =
                                Optimisation.InTransformedSpace
                                <| fun _ _ _ domain _ f ->
                                    let point = [| for (min, _, _) in domain -> min |] |> Tensors.Typed.ofVector
                                    inOptimMin <- point |> Tensors.Typed.itemAt 0 |> Tensors.Typed.toFloatScalar |> Units.removeUnitFromFloat
                                    [ f point |> Tensors.Typed.toFloatScalar, point ]

                            let engine =
                                { defaultEngine () with
                                    OptimiseWith = optimTest }

                            let data =
                                [ (ShortCode.create "X").Value; (ShortCode.create "Y").Value ]
                                |> Seq.map (fun c ->
                                    c,
                                    Time.TimeSeries.fromSeq
                                        Time.DateMode.calendarDateMode
                                        startDate
                                        (Time.Resolution.FixedTemporalResolution.Months months)
                                        data)
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
