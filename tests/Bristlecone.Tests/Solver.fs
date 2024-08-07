module SolverTests

open System
open Bristlecone
open Bristlecone.Time
open Bristlecone.Solver
open Bristlecone.Language
open Expecto

[<Tests>]
let shortCode =
    testList
        "Fixed-resolution solver"
        [

          testPropertyWithConfig Config.config "Removes conditioned t0 and returns correct data"
          <| fun date (data: float list) resolution stepping t0Stepping ->

            let ode = fun _ _ _ -> 1.0
            let actualData = data |> TimeSeries.fromSeq date resolution
            let c = ShortCode.create "x" |> Option.get
            let dynamicSeries = Time.TimeFrame.tryCreate (Map.ofList [ c, actualData ]) |> Option.get

            let solve =
                fixedResolutionSolver 
                    resolution
                    stepping
                    dynamicSeries
                    None
                    { Bristlecone.mkContinuous with LogTo = ignore }
                    (Map.ofList [c, 0.], t0Stepping)

            let result =
                Map.ofList [ c, ode ]
                |> solve

            let expected =
                Integration.MathNet.integrate ignore (-1.) actualData.Length 1. (Map.ofList [c, -1.0]) Map.empty (Map.ofList [ c, ode ])
                |> Map.map (fun _ v -> v |> Array.tail |> Seq.toArray |> Array.tail)

            Expect.equal actualData.Length result.[c].Length 
                "The number of time-points differed between the data and the solver output"
            Expect.equal (result.[c].Length) (expected.[c].Length)
                "The solver did not correctly discard the conditioned time-point (t0)"
            Expect.sequenceEqual (result.[c]) (expected.[c])
                "The solver returned unexpected data from the ODE"


          testPropertyWithConfig Config.config "Data is returned at internal or external resolution correctly. t0 conditioning is applied and removed correctly."
          <| fun date (data: float list) dailyResolution stepping t0Stepping ->

            let ode = fun _ _ _ -> 1.0
            let actualData = data |> TimeSeries.fromSeq date (Resolution.Days dailyResolution)
            let c = ShortCode.create "x" |> Option.get
            let dynamicSeries = Time.TimeFrame.tryCreate (Map.ofList [ c, actualData ]) |> Option.get

            // Solver should only report values from t1 onwards 
            // (i.e. drop t0 and any values between t0 and t1)
            let conditioningPointsThatShouldBeRemoved =
                match t0Stepping with
                | Conditioning.ConditionTimeline.EnvironmentalData -> 1
                | Conditioning.ConditionTimeline.ObservedData -> dailyResolution.Value

            // Setup environment data that covers the whole period of dynamic data but
            // at higher resolution. Includes data for the conditioning period
            // (t0 should be at 0 minus dailyResolution).
            let environment = 
                [ c, TimeSeries.fromSeq (date.AddDays(-conditioningPointsThatShouldBeRemoved)) (FixedTemporalResolution.Days ((PositiveInt.create 1).Value)) [ 1. .. float (conditioningPointsThatShouldBeRemoved + (data.Length * dailyResolution.Value)) ] ]
                |> Map.ofList
                |> TimeFrame.tryCreate
                |> Option.get

            let solve =
                fixedResolutionSolver 
                    (Resolution.Days dailyResolution)
                    stepping
                    dynamicSeries
                    (Some environment)
                    { Bristlecone.mkContinuous with LogTo = ignore }
                    (Map.ofList [c, 1. - float conditioningPointsThatShouldBeRemoved], t0Stepping)

            let result =
                Map.ofList [ c, ode ]
                |> solve

            Expect.equal result.[c].[0] 1.0 "The first time-value from the solver should be 1.0 (i.e. each value is t. t1 = 1)"

            // NB as t1 = 1.0 the number of days from start at t1 = 0.0.
            Expect.equal ((result.[c] |> Seq.last) - 1.0) (actualData.EndDate - (actualData.StartDate |> snd)).Days
                "The last solved time-step must correspond to the number of days between the start and end date of the actual data"

            match stepping with
            | External ->
                Expect.equal result.[c].Length (actualData |> TimeSeries.toObservations |> Seq.length)
                    (sprintf "The number of time points in solver output did not match the lower daily resolution (%A)" dailyResolution)
            | Internal ->
                let envTimeline = environment.Series.[c] |> TimeSeries.bound (actualData.StartDate |> snd) actualData.EndDate |> Option.get

                Expect.equal result.[c].Length envTimeline.Length
                    "The number of time-points differed between the high-res environmental data and solver output"

        ]