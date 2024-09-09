module Bristlecone.Time

open System

/// Contains F#-friendly extension methods for .NET time types.
[<AutoOpen>]
module TimeExtensions =

    type TimeSpan with
        static member Multiply (two: int) (one: TimeSpan) = one.Ticks * (int64 two) |> TimeSpan

    type DateTime with
        static member Create day month year =
            let d = DateTime(year = year, month = month, day = day)
            DateTime.SpecifyKind(d, DateTimeKind.Utc)

[<RequireQualifiedAccess>]
module Resolution =

    /// Represents the width of equally-spaced steps in time.
    type FixedTemporalResolution =
        | Years of PositiveInt
        | Months of PositiveInt
        | Days of PositiveInt
        | CustomEpoch of RealTimeSpan

    /// Represents the maximum resolution of the time series
    type TemporalResolution =
        | Fixed of FixedTemporalResolution
        | Variable

    /// Increment time by an increment defined as a fixed temporal resolution.
    let increment res (t: DateTime) =
        match res with
        | Years i -> t.AddYears i.Value
        | Months i -> t.AddMonths i.Value
        | Days i -> t.AddDays(float i.Value)
        | CustomEpoch ticks -> t + ticks.Value


type FixedTemporalResolution = Resolution.FixedTemporalResolution

[<RequireQualifiedAccess>]
module TimeSeries =

    /// A raw observation, at a given time
    type Observation<'T> = 'T * DateTime

    /// Change in value from one timepoint to another.
    type Epoch<'T> = 'T * TimeSpan

    /// A sequence of data observed at time points.
    type FloatingTimeSeries<'T> = private TimeSteps of Epoch<'T>[]

    /// A sequence of data observed at time intervals (regular or irregular),
    /// where the sampling intervals occur following a fixed start observation.
    type TimeSeries<'T> =
        private
        | FixedTimeSeries of Observation<'T> * FloatingTimeSeries<'T>

        member this.StartDate =
            let startPoint (FixedTimeSeries(s, _)) = s
            this |> startPoint


    let private innerSeries (FixedTimeSeries(s, TimeSteps ts)) = ts
    let private unwrap (FixedTimeSeries(s, TimeSteps ts)) = (s, ts)
    let private createFixedSeries start series = (start, series) |> FixedTimeSeries

    /// Arrange existing observations as a bristlecone `TimeSeries`.
    /// Observations become ordered and indexed by time.
    let fromObservations (dataset: seq<Observation<'a>>) : TimeSeries<'a> =
        if dataset |> Seq.length < 2 then
            invalidArg "dataset" "The data must be at least two elements long"

        let sorted = dataset |> Seq.sortBy snd
        let baseline = sorted |> Seq.head

        let timePoints =
            sorted
            |> Seq.tail // The first time point is used as initial state in the scan
            |> Seq.scan
                (fun (v1, d1, ts) (v2, d2) -> (v2, d2, d2 - d1))
                (baseline |> fst, baseline |> snd, TimeSpan.Zero)
            |> Seq.tail // The first time point is removed (it forms the baseline instead)
            |> Seq.map (fun (v, _, ts) -> (v, ts))
            |> Seq.toArray
            |> TimeSteps

        FixedTimeSeries(baseline, timePoints)

    /// Create a time series from a sequence of existing data, where each
    /// observation is equally spaced in time.
    let fromSeq t1 resolution (data: seq<'a>) =
        data
        |> Seq.tail // The first time point is used as initial state for the scan
        |> Seq.scan (fun (_, t) v -> (v, t |> Resolution.increment resolution)) (data |> Seq.head, t1)
        |> fromObservations

    /// Turn a time series into a sequence of observations
    let toObservations series : seq<Observation<'a>> =
        let baseline, ts = series |> unwrap
        ts |> Seq.scan (fun (_, lastDate) (v, ts) -> (v, lastDate + ts)) baseline

    /// Map a function to each value in the time series.
    let map f series =
        series
        |> innerSeries
        |> Array.map (fun (v, ts) -> (f (v, ts), ts))
        |> TimeSteps
        |> createFixedSeries (f (series.StartDate |> fst, TimeSpan.Zero), series.StartDate |> snd)

    /// The time intervals - or *epochs* - that form the time series.
    let epochs series = series |> innerSeries |> Array.map snd

    let checkMoreThanEqualTo n seq =
        if seq |> Seq.length >= n then Some seq else None

    /// Remove all time points that occur before the desired start date.
    let trimStart startDate series =
        let obs = series |> toObservations

        match obs |> Seq.tryFindIndex (fun (_, t) -> t >= startDate) with
        | Some startIndex ->
            obs
            |> Seq.toArray
            |> Array.splitAt startIndex
            |> snd
            |> checkMoreThanEqualTo 2
            |> Option.map fromObservations
        | None -> None

    /// Remove all time points that occur after the desired end date.
    let trimEnd endDate series =
        series
        |> toObservations
        |> Seq.takeWhile (fun (_, t) -> t <= endDate)
        |> checkMoreThanEqualTo 2
        |> Option.map fromObservations

    /// Bound a time series inclusively
    let bound startDate endDate series =
        series
        |> toObservations
        |> Seq.skipWhile (fun (_, t) -> t < startDate)
        |> Seq.takeWhile (fun (_, t) -> t <= endDate)
        |> checkMoreThanEqualTo 2
        |> Option.map fromObservations

    /// Date of the last observation within the time series.
    let endDate (series: TimeSeries<'a>) =
        series |> toObservations |> Seq.last |> snd

    /// Time points of sampling within the time series.
    let dates (series: TimeSeries<'a>) = series |> toObservations |> Seq.map snd

    /// Find an observation by its exact time of occurrence.
    let findExact time (series: TimeSeries<'a>) =
        series |> toObservations |> Seq.find (fun (v, t) -> t = time)

    /// Calculates the temporal resolution of a time series
    let resolution (series: TimeSeries<'a>) =
        let epochs = series |> epochs

        if epochs |> Seq.distinct |> Seq.length = 1 then // There is a fixed time period.
            let fixedEpoch = epochs |> Seq.head

            if fixedEpoch = TimeSpan.Zero then
                failwith "Cannot have a resolution of zero"

            if fixedEpoch.TotalDays % 1. = 0. then
                Resolution.Fixed
                <| FixedTemporalResolution.Days(PositiveInt.create (int fixedEpoch.TotalDays) |> Option.get)
            else
                Resolution.Fixed
                <| FixedTemporalResolution.CustomEpoch(RealTimeSpan.create fixedEpoch |> Option.get)
        else // A variable time interval exists. This could be months, years, or some other unit.
            let observations = series |> toObservations
            let startDate = observations |> Seq.head |> snd

            let monthDifferences =
                observations
                |> Seq.map snd
                |> Seq.tail
                |> Seq.scan
                    (fun (t1, m) t2 -> (t2, (t2.Year * 12) + t2.Month))
                    (startDate, (startDate.Year * 12) + startDate.Month)
                |> Seq.map snd
                |> Seq.pairwise
                |> Seq.map (fun (m1, m2) -> m2 - m1)

            if monthDifferences |> Seq.distinct |> Seq.length = 1 then // There is a fixed monthly stepping
                let fixedMonths = monthDifferences |> Seq.head

                if fixedMonths % 12 = 0 then
                    Resolution.Fixed
                    <| FixedTemporalResolution.Years(PositiveInt.create (fixedMonths / 12) |> Option.get)
                else
                    Resolution.Fixed
                    <| FixedTemporalResolution.Months(PositiveInt.create fixedMonths |> Option.get)
            else // The time series has variable increments not on day or month cycle
                Resolution.Variable

    /// Reduces the temporal resolution of `series`.
    /// The time series must be able to be split exactly into the lower resolution. For example,
    /// when upscaling from one to three years, the time series must be a multiple of three years.
    let generalise desiredResolution (upscaleFunction: seq<Observation<'a>> -> 'a) (series: TimeSeries<'a>) =
        let resolution = series |> resolution
        let obs = series |> toObservations

        match resolution with
        | Resolution.Variable -> invalidArg "desiredResolution" "Cannot generalise a variable-resolution time series"
        | Resolution.Fixed res ->
            match res with
            | FixedTemporalResolution.Years oldYears ->
                match desiredResolution with
                | FixedTemporalResolution.Years newYears ->
                    if
                        newYears > oldYears
                        && ((float newYears.Value) % (float oldYears.Value) = 0.)
                        && ((obs |> Seq.length |> float) % (float newYears.Value) = 0.)
                    then
                        obs
                        |> Seq.chunkBySize (newYears.Value / oldYears.Value)
                        |> Seq.map (fun bin -> (bin |> upscaleFunction, bin |> Seq.head |> snd))
                        |> fromObservations
                    else
                        invalidArg
                            "desiredResolution"
                            "The upscaled resolution was not a whole multiple of the old resolution"
                | _ -> invalidArg "desiredResolution" "Cannot generalise an annual time series to a lower resolution"
            | FixedTemporalResolution.Days oldDays ->
                match desiredResolution with
                | FixedTemporalResolution.Months newMonths ->
                    obs
                    |> Seq.groupBy (fun (_, t) -> (t.Year, t.Month))
                    |> Seq.map (fun ((y, m), g) -> (g |> upscaleFunction, g |> Seq.map snd |> Seq.max))
                    |> fromObservations
                | _ -> invalidOp "Not implemented"
            | _ -> invalidArg "desiredResolution" "Not implemented"

    /// Interpolates missing values in a time series, where missing values
    /// are represented as an `Option` type.
    let interpolate (series: TimeSeries<float option>) : TimeSeries<float> =
        let observations = series |> toObservations |> Seq.toArray

        observations
        |> Seq.mapi (fun i obs ->
            match fst obs with
            | Some _ -> Some(obs, i, i, i)
            | None ->
                let lastIndex =
                    observations |> Seq.take i |> Seq.tryFindIndexBack (fun (o, _) -> o.IsSome)

                let nextIndex =
                    observations |> Seq.skip i |> Seq.tryFindIndex (fun (o, _) -> o.IsSome)

                if lastIndex.IsSome && nextIndex.IsSome then
                    Some(obs, i, lastIndex.Value, nextIndex.Value + i)
                else
                    None)
        |> Seq.choose id
        |> Seq.map (fun (obs, i, last, next) ->
            if (next - last) = 0 then
                (observations.[i] |> fst).Value, observations.[i] |> snd
            else
                let lastValue = (observations.[last] |> fst).Value
                let nextValue = (observations.[next] |> fst).Value

                (lastValue
                 + ((nextValue - lastValue) / (float next - float last)) * (float i - float last)),
                (obs |> snd))
        |> fromObservations

    ///**Description**
    /// Determines if multiple time series have the same temporal extent and time steps.
    ///**Output Type**
    ///  * A `TimeSpan [] option` containing the common timeline, or `None` if there is no common timeline.
    let commonTimeline (series: TimeSeries<'a> list) =
        let timeSteps = series |> List.map (toObservations >> Seq.map snd >> Seq.toList)

        match timeSteps with
        | Single
        | AllIdentical -> Some(series |> List.head |> epochs)
        | Empty
        | Neither -> None

    /// Removing a step takes account of leap years.
    /// NB Never removes the original start point.
    let bootstrapFixedStep stepping i series =
        let start, _ = series |> unwrap

        let newData =
            series
            |> innerSeries
            |> Array.map fst
            |> Array.toList
            |> List.remove i
            |> List.toArray

        (List.init (Seq.length newData) (fun _ -> stepping))
        |> Seq.zip newData
        |> Seq.toArray
        |> TimeSteps
        |> createFixedSeries start


    type TimeSeries<'T> with
        member this.Length = (this |> innerSeries |> Array.length) + 1

        member this.Head = this |> innerSeries |> Array.head

        member this.TimeSteps = this |> innerSeries |> Array.map snd

        member this.Values = this |> toObservations |> Seq.map fst

        member this.Resolution = this |> resolution

        member this.EndDate = this |> toObservations |> Seq.last |> snd


type TimeSeries<'T> = TimeSeries.TimeSeries<'T>

/// Contains functions for bootstrapping time series.
module Bootstrap =

    /// Randomly remove a single time point from a time-series.
    let removeSingle (random: System.Random) (data: CodedMap<TimeSeries<float<'v>>>) =
        let commonTimeSeries =
            TimeSeries.commonTimeline (data |> Map.toList |> List.map snd)

        match commonTimeSeries with
        | None -> invalidOp "The time series are not on common time"
        | Some ts ->
            match ts.Length with
            | 0 -> invalidOp "The time series was empty"
            | _ ->
                let selection = random.Next(0, ts.Length - 1)

                data
                |> Map.map (fun _ v -> v |> TimeSeries.bootstrapFixedStep (TimeSpan.FromDays(366.)) selection)

module TimeIndex =

    type IndexMode<'a> =
        | Interpolate of ((float * 'a) -> (float * 'a) -> float -> 'a)
        | Exact

    /// Calculates the fractional number of years elapsed between two dates.
    /// The basis used is actual/actual.
    let totalYearsElapsedFraction (d1: DateTime) (d2: DateTime) =
        let yf1 = float d1.DayOfYear / (if DateTime.IsLeapYear d1.Year then 366. else 365.)
        let yf2 = float d2.DayOfYear / (if DateTime.IsLeapYear d2.Year then 366. else 365.)

        let wholeYears =
            let rec addYear (d: DateTime) i =
                let x = d.AddYears(1)
                if x > d2 then i else addYear x (i + 1)

            addYear d1 0

        yf2 - yf1 + float wholeYears

    /// Algorithm that ignores leap year days.
    /// Truncation occurs for 29th Feburary.
    /// Actual days count basis.
    let totalYearsElapsed' (d1: DateTime) (d2: DateTime) =
        let feb29th = 60

        let nonLeapDay (d1: DateTime) =
            if d1.DayOfYear > feb29th && DateTime.IsLeapYear d1.Year then
                d1.DayOfYear
            else
                d1.DayOfYear - 1

        let wholeYears, latestWholeYear =
            let rec addYear (d: DateTime) i =
                let x = d.AddYears 1 // DateTime method truncates leap year
                if x > d2 then (i, d) else addYear x (i + 1)

            addYear d1 0

        let yearFraction =
            let day2 = nonLeapDay d2

            if latestWholeYear.Year < d2.Year then
                if DateTime.IsLeapYear d2.Year && day2 > feb29th then
                    float (abs (day2 - latestWholeYear.DayOfYear)) / 366.
                else
                    float (abs (day2 - latestWholeYear.DayOfYear)) / 365.
            else
                let dayDiff = d2.DayOfYear - latestWholeYear.DayOfYear
                let daysInYear = if DateTime.IsLeapYear d2.Year then 366 else 365

                if latestWholeYear.DayOfYear < feb29th && d2.DayOfYear > feb29th then
                    float (dayDiff - 1) / float daysInYear
                else
                    float dayDiff / float daysInYear

        float wholeYears + yearFraction

    let totalYearsElapsed d1 d2 =
        if d2 > d1 then
            totalYearsElapsed' d1 d2
        else
            totalYearsElapsed' d2 d1

    // TODO Take account of day of month. Currently does not handle varying month lengths
    let totalMonthsElapsed (d1: DateTime) (d2: DateTime) : float =
        (d2.Month - d1.Month) + (d2.Year - d1.Year) * 12 |> float

    /// Indexes the time series in accordance with a `baselineTime` and fixed `resolution`.
    /// Where the time series is of a lower resolution
    let indexSeries (t0: DateTime) targetResolution (series: TimeSeries<'a>) : seq<float * 'a> =
        let obs = series |> TimeSeries.toObservations

        match targetResolution with
        | FixedTemporalResolution.Years y ->
            obs |> Seq.map (fun (v, tn) -> (((totalYearsElapsed t0 tn) / float y.Value), v))
        | FixedTemporalResolution.Months m ->
            obs
            |> Seq.map (fun (v, tn) -> (((totalMonthsElapsed t0 tn) / float m.Value), v))
        | FixedTemporalResolution.Days d -> obs |> Seq.map (fun (v, tn) -> (((tn - t0).TotalDays / float d.Value), v))
        | FixedTemporalResolution.CustomEpoch t ->
            obs
            |> Seq.map (fun (v, tn) -> ((((tn - t0).Ticks / t.Value.Ticks) |> float), v))

    /// A representation of temporal data as fractions of a common fixed temporal resolution,
    /// from a given baseline. The baseline must be greater than or equal to the baseline
    /// of the time series.
    type TimeIndex<'a>(baseDate, resolution, mode, series: TimeSeries<'a>) =
        let table = series |> indexSeries baseDate resolution |> Map.ofSeq
        let tablePairwise = table |> Map.toArray |> Array.pairwise // Ordered in time

        member __.Item

            with get (t): 'a =
                match mode with
                | Exact -> table.[t]
                | Interpolate i ->
                    if table.ContainsKey t then
                        table.[t]
                    else
                        // Find the closest points in the index before and after t
                        match
                            tablePairwise
                            |> Array.tryFind (fun ((k1, _), (k2, _)) -> (t - k1) > 0. && (t - k2) < 0.)
                        with
                        | Some((k1, v1), (k2, v2)) -> i (k1, v1) (k2, v2) t
                        | None ->
                            invalidOp
                            <| sprintf
                                "Could not interpolate to time %f because it falls outside the range of the temporal index"
                                t

        member __.Baseline = baseDate

        member __.Values = table |> Map.toSeq


/// A `TimeFrame` contains multiple time-series that use the same
/// temporal index.
[<RequireQualifiedAccess>]
module TimeFrame =

    type TimeFrame<'a> = private TimeFrame of CodedMap<TimeSeries.TimeSeries<'a>>

    let tryCreate series =
        let commonTimeline =
            series |> Map.toList |> List.map snd |> TimeSeries.commonTimeline

        match commonTimeline with
        | Some _ -> TimeFrame series |> Some
        | None -> None

    let inner (TimeFrame frame) = frame

    type TimeFrame<'a> with
        member this.Resolution = (this |> inner |> Seq.head).Value.Resolution
        member this.StartDate = (this |> inner |> Seq.head).Value.StartDate |> snd
        member this.Series = this |> inner


[<RequireQualifiedAccess>]
module GrowthSeries =

    type GrowthSeries<[<Measure>] 'u> =
        | Cumulative of TimeSeries<float<'u>>
        | Absolute of TimeSeries<float<'u>>
        | Relative of TimeSeries<float<'u>>

    let growthToTime growth =
        match growth with
        | Absolute g -> g
        | Cumulative g -> g
        | Relative g -> g
