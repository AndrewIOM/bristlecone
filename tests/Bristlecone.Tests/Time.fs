module TimeTests

open System
open Expecto
open Bristlecone
open Bristlecone.Time
open FsCheck

let genMultiList minLength maxLength =
    gen {
        let! length = Gen.choose (minLength, maxLength)
        let! list = Gen.listOfLength length Arb.generate<float>
        return list
    }

type CustomGen() =
    static member Floats() : Arbitrary<float list> = genMultiList 2 1000 |> Arb.fromGen

    static member PositveInt: Arbitrary<PositiveInt.PositiveInt> =
        Gen.choose (1, 5) //Int32.MaxValue)
        |> Gen.map (PositiveInt.create >> Option.get)
        |> Arb.fromGen

    static member RealTimeSpan =
        Gen.choose (1, Int32.MaxValue)
        |> Gen.map (int64 >> TimeSpan.FromTicks >> RealTimeSpan.create >> Option.get)
        |> Arb.fromGen

    static member Observations: Arbitrary<TimeSeries.Observation<float> list> =
        gen {
            let! length = Gen.choose (2, 100)
            let! list1 = Gen.listOfLength length Arb.generate<DateTime>
            let! list2 = Gen.listOfLength length (Arb.generate<NormalFloat> |> Gen.map (fun f -> f.Get))
            return List.zip list2 list1
        }
        |> Arb.fromGen



let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<CustomGen> ] }


[<Tests>]
let timeSeries =
    testList
        "Time series"
        [

          testPropertyWithConfig config "Resolution is always the same as creation-time"
          <| fun date (data: float list) resolution ->
              let ts = data |> TimeSeries.fromSeq date resolution
              Expect.equal ts.Resolution (Resolution.Fixed resolution) "Resolution was transformed during processing"

          testPropertyWithConfig config "Start time is always the same as creation-time"
          <| fun date (data: float list) resolution ->
              let ts = data |> TimeSeries.fromSeq date resolution
              ts.StartDate.Deconstruct() |> snd = date

          testPropertyWithConfig config "Create from observations fails without 2+ observations"
          <| fun (data: TimeSeries.Observation<float> list) ->
              if data |> List.length < 2 then
                  Expect.throws
                      (fun () -> TimeSeries.fromObservations data |> ignore)
                      "Did not fail with less than two observations"

          testPropertyWithConfig config "Create from observations orders data by time"
          <| fun (data: TimeSeries.Observation<float> list) ->
              let ts = TimeSeries.fromObservations data

              Expect.sequenceEqual
                  ts.Values
                  (data |> Seq.sortBy snd |> Seq.map fst)
                  "Values were not ordered forwards in time"

          testPropertyWithConfig
              config
              "Transforming observations to time series and back again has the same 
            observations, but not necessarily in the same order"
          <| fun (observations: TimeSeries.Observation<float> list) ->
              let transformed =
                  observations |> TimeSeries.fromObservations |> TimeSeries.toObservations

              Expect.sequenceEqual
                  (transformed |> Seq.sort)
                  (observations |> Seq.sort)
                  "Observations were transformed by TimeSeries functions"

          testPropertyWithConfig config "'map' transforms all values"
          <| fun (observations: TimeSeries.Observation<float> list) f ->
              let ts: TimeSeries<float> = observations |> TimeSeries.fromObservations
              let original = ts.Values

              Expect.sequenceEqual
                  (ts |> TimeSeries.map (fun (d, _) -> f d)).Values
                  (original |> Seq.map f)
                  "map behaved differently between Seq.map and TimeSeries.map"

          testList
              "Bounding functions"
              [ testPropertyWithConfig config "Trimming start removes all time points before specified date"
                <| fun startDate (observations: TimeSeries.Observation<float> list) ->
                    let trimmed =
                        observations |> TimeSeries.fromObservations |> TimeSeries.trimStart startDate

                    let x = observations |> Seq.filter (fun (_, d) -> d > startDate) |> Seq.map fst

                    if x |> Seq.length < 2 then
                        Expect.isNone trimmed "There were less than two observations left"
                    else
                        Expect.isSome trimmed "There were more than one observation"

                        Expect.equal
                            (trimmed.Value.Values |> Seq.length)
                            (x |> Seq.length)
                            "Count differed between seq and TimeSeries"

                        Expect.sequenceEqual
                            (trimmed.Value.Values |> Seq.sort)
                            (x |> Seq.sort)
                            "Values differed between seq and TimeSeries"

                testPropertyWithConfig config "Trimming end removes all time points after specified date"
                <| fun date (obs: TimeSeries.Observation<float> list) ->
                    let trimmed = obs |> TimeSeries.fromObservations |> TimeSeries.trimEnd date
                    let x = obs |> Seq.filter (fun (_, d) -> d < date) |> Seq.map fst

                    if x |> Seq.length < 2 then
                        Expect.isNone trimmed "There were less than two observations left"
                    else
                        Expect.isSome trimmed "There were more than one observation"

                        Expect.equal
                            (trimmed.Value.Values |> Seq.length)
                            (x |> Seq.length)
                            "Count differed between seq and TimeSeries"

                        Expect.sequenceEqual
                            (trimmed.Value.Values |> Seq.sort)
                            (x |> Seq.sort)
                            "Values differed between seq and TimeSeries"

                testPropertyWithConfig config "Start date cannot be before given start date when bounding"
                <| fun startDate endDate (obs: TimeSeries.Observation<float> list) ->
                    let bounded =
                        obs |> TimeSeries.fromObservations |> TimeSeries.bound startDate endDate

                    match bounded with
                    | Some ts ->
                        Expect.isGreaterThanOrEqual
                            (ts.StartDate.Deconstruct() |> snd)
                            startDate
                            "Start date was earlier than start cut-off"
                    | None -> ()

                testPropertyWithConfig config "Bounding removes time points before and after dates"
                <| fun startDate endDate (obs: TimeSeries.Observation<float> list) ->
                    let trimmed =
                        obs |> TimeSeries.fromObservations |> TimeSeries.bound startDate endDate

                    let x =
                        obs |> Seq.filter (fun (_, d) -> d >= startDate && d <= endDate) |> Seq.map fst

                    if x |> Seq.length < 2 then
                        Expect.isNone trimmed "There were less than two observations left"
                    else
                        Expect.isSome trimmed "There were more than one observation"

                        Expect.equal
                            (trimmed.Value.Values |> Seq.length)
                            (x |> Seq.length)
                            "Count differed between seq and TimeSeries functions"

                        Expect.sequenceEqual
                            (trimmed.Value.Values |> Seq.sort)
                            (x |> Seq.sort)
                            "Values differed between seq and TimeSeries"

                ] ]

[<Tests>]
let timeIndex =
    testList
        "Time index"
        [

          testProperty "Years elapsed are whole numbers when same day is used"
          <| fun (date: DateTime) (yearDiff: NormalFloat) ->
              let date2 = date.AddYears(int yearDiff.Get)
              let diff = TimeIndex.totalYearsElapsed date date2
              Expect.equal (diff % 1.) 0. "The year difference was not an integer"

          testProperty "Months elapsed are whole numbers when same day of year"
          <| fun (date: DateTime) (monthDiff: NormalFloat) ->
              let date2 = date.AddMonths(int monthDiff.Get)
              let diff = TimeIndex.totalMonthsElapsed date date2
              Expect.equal (diff % 1.) 0. "The month difference was not an integer"

          testPropertyWithConfig config "Time series created at a specific resolution indexes as whole numbers"
          <| fun startDate (data: float list) resolution ->
              let ts = TimeSeries.fromSeq startDate resolution data

              let index =
                  TimeIndex.TimeIndex(startDate, resolution, TimeIndex.IndexMode.Exact, ts)

              let timeSteps = index.Values |> Seq.map fst
              Expect.all timeSteps (fun s -> s % 1. = 0.) "The time steps contained decimal places"

          ]
