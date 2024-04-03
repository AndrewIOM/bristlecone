module BristleconeTests

open Bristlecone
open Expecto
open FsCheck
open Bristlecone.EstimationEngine

let config = TimeTests.config

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

              testProperty "Repeating first data point sets t0 as t1"
              <| fun time resolution (data: float list) ->
                  let data =
                      [ (Language.code "x").Value,
                        Time.TimeSeries.fromSeq time (Time.FixedTemporalResolution.Years resolution) data ]
                      |> Map.ofList

                  let result = Bristlecone.Fit.t0 data Conditioning.NoConditioning ignore
                  Expect.equal result (data |> Map.map (fun k v -> v.Values |> Seq.head))

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

                    testPropertyWithConfig TimeTests.config "Core fitting functions are reproducable"
                    <| fun b1 b2 seedNumber (obs: float list) startDate months ->
                        let data: CodedMap<Time.TimeSeries.TimeSeries<float>> =
                            [ (Language.code "x").Value,
                              Time.TimeSeries.fromSeq startDate (Time.FixedTemporalResolution.Months months) obs ]
                            |> Map.ofList

                        let result =
                            Expect.wantOk
                                (Bristlecone.fit defaultEngine defaultEndCon data (TestModels.constant b1 b2))
                                ""

                        let result2 =
                            Expect.wantOk
                                (Bristlecone.fit
                                    { defaultEngine with
                                        Random = MathNet.Numerics.Random.MersenneTwister(seedNumber, true) }
                                    defaultEndCon
                                    data
                                    (TestModels.constant b1 b2))
                                ""

                        Expect.equal result.Likelihood result2.Likelihood "Different likelihoods"
                        Expect.equal result.InternalDynamics result.InternalDynamics "Different internal dynamics"
                        Expect.equal result.Parameters result2.Parameters "Different parameters"
                        Expect.equal result.Series result2.Series "Different expected series"
                        Expect.equal result.Trace result2.Trace "Different traces"

                    testProperty "Time-series relating to model equations must overlap"
                    <| fun t1 t2 resolution data1 data2 ->
                        let ts =
                            [ Time.TimeSeries.fromSeq t1 (Time.FixedTemporalResolution.Years resolution) data1
                              Time.TimeSeries.fromSeq t2 (Time.FixedTemporalResolution.Years resolution) data2 ]

                        let result =
                            Bristlecone.Fit.observationsToCommonTimeFrame
                                (TestModels.twoEquationConstant Language.noConstraints 0. 1.).Equations
                            |> ignore

                        result
                        false

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

                    testProperty "Positive only parameter is transformed when optimising in transformed space"
                    <| fun data (b1: NormalFloat) (b2: NormalFloat) ->
                        let testModel = TestModels.twoEquationConstant Language.notNegative b1.Get b2.Get
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

                        let result =
                            Expect.wantOk
                                (Bristlecone.fit defaultEngine defaultEndCon data testModel)
                                "Errored when should be OK"

                        Expect.equal
                            inOptimMin
                            (min b1.Get b2.Get)
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
