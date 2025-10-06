module BristleconeTests

open Bristlecone
open Expecto
open FsCheck
open Bristlecone.EstimationEngine

// Helpers
let expectSameFloat a b message =
    Expect.isTrue (LanguagePrimitives.GenericEqualityER a b) message

let expectSameFloatList a b message =
    Seq.zip a b |> Seq.iter (fun (a, b) -> expectSameFloat a b message)

module TestModels =
    open Bristlecone.Language

    let rateConstant bound1 bound2 =
        let X = state "X"
        let a = parameter "a" noConstraints (min bound1 bound2) (max bound1 bound2)        
        Model.empty
        |> Model.addRateEquation X (P a)
        |> Model.estimateParameter a
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ X.Code ])
        |> Model.compile

    /// Scaffold a discrete model where there is one parameter (a)
    /// setup to be estimated.
    let discreteModel bound1 bound2 =
        let X = state "X"
        let a = parameter "a" noConstraints (min bound1 bound2) (max bound1 bound2)        
        Model.discrete
        |> Model.addDiscreteEquation X (P a)
        |> Model.estimateParameter a
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ X.Code ])
        |> Model.compile

    let rateTwoEquationConstant cons bound1 bound2 =
        let X = state "X"
        let Y = state "Y"
        let a = parameter "a" cons (min bound1 bound2) (max bound1 bound2)        
        Model.empty
        |> Model.addRateEquation X (P a)
        |> Model.addRateEquation Y (P a)
        |> Model.estimateParameter a
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ X.Code; Y.Code ])
        |> Model.compile

    /// dX/dt = Temperature
    let tempDrivenModel () =
        let X = state "X"
        let temp = environment "Temp"
        let dummy = parameter "a" noConstraints (min 0.1 0.2) (max 0.1 0.2)        
        Model.empty
        |> Model.addRateEquation X (Environment temp)   // derivative of X is just the env forcing
        |> Model.estimateParameter dummy
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ X.Code ])
        |> Model.compile


let indexBySpan (ts:System.TimeSpan) =
    float ts.Days * 1.<Time.day> * 12.<Time.``time index``/Time.day>

let defaultEngine () =
    { TimeHandling = Continuous <| Integration.RungeKutta.rk4
      OptimiseWith = Optimisation.None.none
      LogTo = ignore
      ToModelTime = indexBySpan
      Random = MathNet.Numerics.Random.MersenneTwister(1000,true)
      InterpolationGlobal = Solver.InterpolationMode.Lower
      InterpolationPerVariable = Map.empty
      Conditioning = Conditioning.RepeatFirstDataPoint }

let defaultEndCon = Optimisation.EndConditions.atIteration 1000<iteration>

module ``Fit`` =

    [<Tests>]
    let fitTests =
      testList "Integration tests for tryFit" [

        // --- Conditioning modes ---
        testPropertyWithConfig Config.config "Conditioning modes produce aligned predictions"
        <| fun (PositiveInt steps) (x0:NormalFloat) (useRepeat:bool) ->
            let steps = max 3 steps
            let startDate = System.DateTime(2000,1,1)
            let obs =
              [| for i in 0..steps-1 -> (x0.Get + float i) * 1.<ModelSystem.state>, startDate.AddDays(float i) |]
              |> Time.TimeSeries.fromNeoObservations
            let data = Map.ofList [ (Language.code "X").Value, obs ]
            let engine =
              { defaultEngine() with
                  Conditioning = if useRepeat then Conditioning.RepeatFirstDataPoint else Conditioning.NoConditioning }
            let result = Expect.wantOk (Bristlecone.tryFit engine defaultEndCon data (TestModels.rateConstant 0. 10.)) "Fit failed"
            let xs = result.Series.[(Language.code "X").Value]

            Expect.equal xs.Length (if useRepeat then obs.Length else obs.Length - 1)
                "Predictions were different length from observations"

            Expect.equal (snd xs.StartDate)
                        (if useRepeat then snd obs.StartDate else (snd obs.StartDate).AddDays 1)
                "Start dates differed between prediction and observation"

            Config.sequenceEqualTol
                (xs.Values |> Seq.map (fun v -> v.Obs))
                (if useRepeat then obs.Values else Seq.tail obs.Values)
                "True observed data was not returned by tryFit"

        // --- Discrete vs differential ---
        testPropertyWithConfig Config.config "Discrete/Differential equations can be fitted"
        <| fun isDifferential (PositiveInt steps) (x0:NormalFloat) (useRepeat:bool) ->
            let steps = max 3 steps
            let startDate = System.DateTime(2000,1,1)
            let obs =
              [| for i in 0..steps-1 -> (x0.Get + float i) * 1.<ModelSystem.state>, startDate.AddDays(float i) |]
              |> Time.TimeSeries.fromNeoObservations
            let data = Map.ofList [ (Language.code "X").Value, obs ]
            let xs =
                if isDifferential
                then
                    let engine =
                        { defaultEngine() with
                            Conditioning = if useRepeat then Conditioning.RepeatFirstDataPoint else Conditioning.NoConditioning }
                    let result = Expect.wantOk (Bristlecone.tryFit engine defaultEndCon data (TestModels.rateConstant 0. 10.)) "Fit failed"
                    result.Series.[(Language.code "X").Value]
                else
                    let engine =
                        { defaultEngine() with
                            TimeHandling = Discrete
                            Conditioning = if useRepeat then Conditioning.RepeatFirstDataPoint else Conditioning.NoConditioning }
                        // |> Bristlecone.withTimeConversion (fun _ -> 1.<1>)
                    let result = Expect.wantOk (Bristlecone.tryFit engine defaultEndCon data (TestModels.discreteModel 0. 10.)) "Fit failed"
                    result.Series.[(Language.code "X").Value]
            Expect.hasLength xs.Values (if useRepeat then obs.Length else obs.Length - 1) "Predictions were different length from observations"
            Expect.equal (snd xs.StartDate) (if useRepeat then snd obs.StartDate else (snd obs.StartDate).AddDays 1) "Start dates differed between prediction and observation"
            Config.sequenceEqualTol (xs.Values |> Seq.map(fun v -> v.Obs)) (if useRepeat then obs.Values else Seq.tail obs.Values)  "True observed data was not returned by tryFit"


        // --- Optimiser trace length ---
        // testCase "Trace length equals optimiser iterations" <| fun _ ->
        //     // TODO: plug in dummy optimiser that runs N iterations, assert result.Trace.Length = N

        // // --- Resolution combinations ---
        // testCase "Fixed-step dynamic with matching env resolution" <| fun _ ->
        //     // TODO: build fixed-step dynamic + env, run tryFit, assert no error

        // testCase "Fixed-step dynamic with higher-res env" <| fun _ ->
        //     // TODO: build env with sub-daily resolution, dynamic daily, assert interpolation/downsampling works

        // testCase "Fixed-step dynamic with lower-res env" <| fun _ ->
        //     // TODO: build env monthly, dynamic daily, assert interpolation works

        // testCase "Variable-step dynamic with no env" <| fun _ ->
        //     // TODO: build irregular dynamic series, run tryFit, assert no error

        // testCase "Variable-step dynamic with matching env" <| fun _ ->
        //     // TODO: irregular dynamic and env with same timestamps, assert alignment

        // testCase "Variable-step dynamic with mismatched env" <| fun _ ->
        //     // TODO: irregular dynamic and env with different timestamps, assert interpolation

        // // --- Error cases ---
        // testCase "Missing dynamic series returns error" <| fun _ ->
        //     // TODO: call tryFit with empty map, assert Error

        // testCase "Environment coverage missing at solver start fails" <| fun _ ->
        //     // TODO: build env starting after dynamic start, assert invalidOp

        // // --- Internal vs External ---
        // testCase "Internal vs External step types differ in length" <| fun _ ->
        //     // TODO: run solver with both step types, assert Internal length > External length

        // // --- Parameter handling ---
        // testCase "Detached vs Transformed optimisation spaces work" <| fun _ ->
        //     // TODO: build model with positive-only parameter, run tryFit with transformed space, assert log-transform applied

      ]


    [<Tests>]
    let interpolationTests =
        testList "Interpolation behaviour" [

            // Test required:
            // - Cannot run with 0 parameters
            // - Cannot run without environmental coverage of conditioning period

            // - Environment can have negative index values, so that e.g. a time-point far before
            // the conditioning period can be interpolated from.

            testCase "Interpolation mode affects state trajectory" <| fun _ ->
                let startDate = System.DateTime(2000,1,1)

                // Environment sampled coarsely: 0 at day 0, 10 at day 10
                let env =
                    [ (-3., startDate.AddDays -1); (0., startDate); (10., startDate.AddDays 10.) ]
                    |> Time.TimeSeries.fromNeoObservations

                let obs =
                    [| for i in 0. .. 10. -> i, startDate.AddDays(float i) |]
                    |> Time.TimeSeries.fromNeoObservations

                let data =
                    Map.ofList [
                        (ShortCode.create "Temp").Value, env
                        (ShortCode.create "X").Value, obs
                    ]

                let engineLower =
                    { defaultEngine() with InterpolationGlobal = Solver.InterpolationMode.Lower }

                let engineLinear =
                    { defaultEngine() with InterpolationGlobal = Solver.InterpolationMode.Linear }

                let model = TestModels.tempDrivenModel()

                let resultLower = Bristlecone.tryFit engineLower defaultEndCon data model |> fun r -> Expect.wantOk r "Fit failed"
                let resultLinear = Bristlecone.tryFit engineLinear defaultEndCon data model |> fun r -> Expect.wantOk r "Fit failed"

                let xsLower = resultLower.Series.[(ShortCode.create "X").Value].Values |> Seq.map (fun v -> v.Fit) |> Seq.toArray
                let xsLinear = resultLinear.Series.[(ShortCode.create "X").Value].Values |> Seq.map (fun v -> v.Fit) |> Seq.toArray

                // At midpoint (day 5), the two interpolation schemes should give different X values
                Expect.notEqual xsLower.[5] xsLinear.[5] "Interpolation mode should affect state trajectory"


        ]
