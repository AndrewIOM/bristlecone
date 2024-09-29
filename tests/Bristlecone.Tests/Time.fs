module TimeTests

open System
open Expecto
open Bristlecone
open Bristlecone.Time
open FsCheck

let config = Config.config

type DateModeToUse =
    | CalendarDates
    | Radiocarbon

[<Tests>]
let timeSeries =
    testList
        "Time series (calendar dates)"
        [

          testPropertyWithConfig config "Resolution is always the same as creation-time"
          <| fun date (data: float list) resolution ->
              let resolution = Resolution.map (fun (x:ObservationalTimeSpan.ObservationalTimeSpan) -> x.Value) resolution
              let ts = data |> TimeSeries.fromSeq DateMode.calendarDateMode date resolution
              Expect.equal (TimeSeries.resolution ts) (Resolution.Fixed resolution) "Resolution was transformed during processing"

          testPropertyWithConfig config "Start time is always the same as creation-time"
          <| fun date (data: float list) (resolution: Resolution.FixedTemporalResolution<ObservationalTimeSpan.ObservationalTimeSpan>) ->
              let resolution = Resolution.map (fun (x:ObservationalTimeSpan.ObservationalTimeSpan) -> x.Value) resolution
              let ts = data |> TimeSeries.fromSeq DateMode.calendarDateMode date resolution
              Expect.equal (ts.StartDate.Deconstruct() |> snd) date "The start dates were different"

          testPropertyWithConfig config "Create from observations fails without 2+ observations"
          <| fun (data: TimeSeries.Observation<float, DateTime> list) ->
              if data |> List.length < 2 then
                  Expect.throws
                      (fun () -> TimeSeries.fromNeoObservations data |> ignore)
                      "Did not fail with less than two observations"

          testPropertyWithConfig config "Create from observations orders data by time"
          <| fun (data: TimeSeries.Observation<float, DateTime> list) ->
              let ts = TimeSeries.fromNeoObservations data

              Expect.sequenceEqual
                  ts.Values
                  (data |> Seq.sortBy snd |> Seq.map fst)
                  "Values were not ordered forwards in time"

          testPropertyWithConfig
              config
              "Transforming observations to time series and back again has the same 
            observations, but not necessarily in the same order"
          <| fun (observations: TimeSeries.Observation<float, DateTime> list) ->
              let transformed =
                  observations |> TimeSeries.fromNeoObservations |> TimeSeries.toObservations

              Expect.sequenceEqual
                  (transformed |> Seq.sort)
                  (observations |> Seq.sort)
                  "Observations were transformed by TimeSeries functions"

          testPropertyWithConfig config "'map' transforms all values"
          <| fun (observations: TimeSeries.Observation<float, DateTime> list) f ->
              let ts = observations |> TimeSeries.fromNeoObservations
              let original = ts.Values

              Expect.sequenceEqual
                  (ts |> TimeSeries.map (fun (d, _) -> f d)).Values
                  (original |> Seq.map f)
                  "map behaved differently between Seq.map and TimeSeries.map"

          testList
              "Bounding functions"
              [ testPropertyWithConfig config "Trimming start removes all time points before specified date"
                <| fun startDate (observations: TimeSeries.Observation<float, DateTime> list) ->
                    let trimmed =
                        observations |> TimeSeries.fromNeoObservations |> TimeSeries.trimStart startDate

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
                <| fun date (obs: TimeSeries.Observation<float, DateTime> list) ->
                    let trimmed = obs |> TimeSeries.fromNeoObservations |> TimeSeries.trimEnd date
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
                <| fun startDate endDate (obs: TimeSeries.Observation<float, DateTime> list) ->
                    let bounded =
                        obs |> TimeSeries.fromNeoObservations |> TimeSeries.bound startDate endDate

                    match bounded with
                    | Some ts ->
                        Expect.isGreaterThanOrEqual
                            (ts.StartDate.Deconstruct() |> snd)
                            startDate
                            "Start date was earlier than start cut-off"
                    | None -> ()

                testPropertyWithConfig config "Bounding removes time points before and after dates"
                <| fun startDate endDate (obs: TimeSeries.Observation<float, DateTime> list) ->
                    let trimmed =
                        obs |> TimeSeries.fromNeoObservations |> TimeSeries.bound startDate endDate

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
let oldDateTimeSeriesTests =
    testList
        "Time series (old dates)"
        [

            testPropertyWithConfig config "BP dates are ordered oldest to youngest"
            <| fun (data: TimeSeries.Observation<float, int<``BP (radiocarbon)``>> list) ->
                let radiocarbonDates = data |> Seq.map(fun (d,r) -> d, DatingMethods.Radiocarbon r)
                if data |> Seq.map snd |> Seq.hasDuplicates
                then Expect.throws (fun _ -> TimeSeries.fromRadiocarbonObservations radiocarbonDates |> ignore) "Dates contained duplicates; should have thrown"
                else
                    let ts = TimeSeries.fromRadiocarbonObservations radiocarbonDates
                    let orderedInTime = data |> Seq.sortByDescending snd |> Seq.map fst
                    Expect.sequenceEqual ts.Values orderedInTime "BP Dates were not ordered with largest BP values first"


        //   testProperty "Years elapsed are whole numbers when same day is used"
        //   <| fun (date: DateTime) (yearDiff: NormalFloat) ->
        //       let date2 = date.AddYears(int yearDiff.Get)
        //       let diff = DateTime.totalYearsElapsed date date2
        //       Expect.equal (diff % 1.<year>) 0.<year> "The year difference was not a round number"

        //   testPropertyWithConfig config "Time series created at a specific resolution indexes as whole numbers"
        //   <| fun startDate (data: float list) resolution ->
        //       let ts = TimeSeries.fromSeq DateMode.calendarDateMode startDate resolution data

        //       let index =
        //           TimeIndex.TimeIndex(startDate, resolution, TimeIndex.IndexMode.Exact, ts)

        //       let timeSteps = index.Values |> Seq.map fst
        //       Expect.all timeSteps (fun s -> s % 1.<``time index``> = 0.<``time index``>) "The time steps contained decimal places"

          ]

module TemporalIndex =

    [<Tests>]
    let calendarDates =
        testList
            "Time index (calendar dates)"
            [

            testProperty "Years elapsed are whole numbers when same day is used"
            <| fun (date: DateTime) (yearDiff: NormalFloat) ->
                let date2 = date.AddYears(int yearDiff.Get)
                let diff = DateTime.totalYearsElapsed date date2
                Expect.equal (diff % 1.<year>) 0.<year> "The year difference was not a round number"

            testProperty "Months elapsed are whole numbers when same day of year"
            <| fun (date: DateTime) (monthDiff: NormalFloat) ->
                let date2 = date.AddMonths(int monthDiff.Get)
                let diff = DateTime.totalMonthsElapsed date date2
                Expect.equal (diff % 1.<month>) 0.<month> "The month difference was not a round number"

            testPropertyWithConfig config "Time series of specific resolution indexes as whole numbers"
            <| fun startDate (data: float list) resolution ->
                    let resolution = Resolution.map (fun (x:ObservationalTimeSpan.ObservationalTimeSpan) -> x.Value) resolution
                    let ts = TimeSeries.fromSeq DateMode.calendarDateMode startDate resolution data
                    let index =
                        TimeIndex.TimeIndex(startDate, resolution, TimeIndex.IndexMode.Exact, ts)
                    let timeSteps = index.Values |> Seq.map fst
                    Expect.all timeSteps (fun s -> abs s % 1.<``time index``> = 0.<``time index``>) "The time steps contained decimal places"
            ]


    [<Tests>]
    let oldDates =
        testList
            "Time index (old dates)"
            [

            testPropertyWithConfig config "Does not allow monthly resolution for old dates" 
            <| fun startDate months (data:float list) ->
                    let mkTs () = TimeSeries.fromSeq DateMode.radiocarbonDateMode startDate (Resolution.Months months) data
                    if months.Value < 12<month>
                    then Expect.throws(fun _ -> mkTs () |> ignore) "Should throw when monthly timestep is zero - 12 months"
                    else 
                        Expect.throws(fun _ ->
                            TimeIndex.TimeIndex(startDate, Resolution.Months months, TimeIndex.IndexMode.Exact, mkTs()) |> ignore
                            )
                            "Should not allow monthly resolution"

            testProperty "Years elapsed correctly calculates (as whole numbers)"
            <| fun (initialYear:NormalFloat) (yearDiff: NormalFloat) ->
                let date1 = int initialYear.Get * 1<``BP (radiocarbon)``> |> DatingMethods.Radiocarbon
                let date2 = DateMode.radiocarbonDateMode.AddYears date1 (int yearDiff.Get * 1<year>)
                let diff = DateMode.radiocarbonDateMode.Difference date1 date2
                Expect.equal (diff.YearFraction % 1.<year>) 0.<year> "The year difference was not a round number"

            testPropertyWithConfig config "Time series indexes as whole numbers when resolutions match"
            <| fun startDate (data: float list) resolution ->
                let runTest res =
                    let ts = TimeSeries.fromSeq DateMode.radiocarbonDateMode startDate resolution data
                    let index = TimeIndex.TimeIndex(startDate, resolution, TimeIndex.IndexMode.Exact, ts)
                    let timeSteps = index.Values |> Seq.map fst
                    Expect.all timeSteps (fun s -> abs s % 1.<``time index``> = 0.<``time index``>) "The time steps contained decimal places"
                match resolution with
                | Resolution.Months m when m.Value >= 12<month> -> runTest resolution
                | Resolution.Days d when d.Value >= 365<day> -> runTest resolution
                // | Resolution.CustomEpoch e when e > 0<``BP (radiocarbon)``> -> runTest resolution
                | Resolution.Years _ -> runTest resolution
                | _ ->
                    Expect.throws (fun _ -> runTest resolution) "Did not throw when resolution was outside permitted range"
            ]
