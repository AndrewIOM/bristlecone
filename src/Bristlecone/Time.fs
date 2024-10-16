module Bristlecone.Time

open System

[<Measure>]
type day

[<Measure>]
type month

[<Measure>]
type year

[<Measure>]
type ticks

// Support the following dating methods:
[<Measure>]
type ``cal yr BP`` // calibrated calendar years before present




[<Measure>]
type ``BP (radiocarbon)`` // uncalibrated years before present




[<Measure>]
type CE // common era




[<Measure>]
type BC

[<Measure>]
type AD

[<Measure>]
type ``time index``

type TimeDifference<'timespan> =
    { DayFraction: float<day>
      MonthFraction: float<month>
      YearFraction: float<year>
      RealDifference: 'timespan
      Ticks: float<ticks> }

// Unit conversions:
let internal convertYearsToMonths (x: float<year>) = x * 12.<month / year>
let internal convertYearsToMonthsInt (x: int<year>) = x * 12<month / year>
let internal convertMonthsToYears (x: float<month>) = x / 12.<month / year>
let internal convertMonthsToYearsInt (x: int<month>) = x / 12<month / year>
let internal monthsPerYear = 12<month year^-1>


/// <summary>Contains F#-friendly extension methods for .NET time types.</summary>
[<AutoOpen>]
module TimeExtensions =

    type TimeSpan with
        static member Multiply (two: int) (one: TimeSpan) = one.Ticks * (int64 two) |> TimeSpan

    type DateTime with
        static member Create day month year =
            let d = DateTime(year, month, day)
            DateTime.SpecifyKind(d, DateTimeKind.Utc)


/// <summary>Helper functions for working with
/// `DateTime` values.</summary>
module DateTime =

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

    let internal totalYearsElapsed' (d1: DateTime) (d2: DateTime) =
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

    /// <summary>Calculates the years elapsed between two dates, from
    /// the earlier to the later date. The algorithm ignores leap year days
    /// (truncation occurs for the 29th Feburary).</summary>
    /// <param name="d1">The first date</param>
    /// <param name="d2">The second date</param>
    /// <returns>The fraction of years elapsed between the two dates</returns>
    let totalYearsElapsed d1 d2 =
        if d2 > d1 then
            totalYearsElapsed' d1 d2 * 1.<year>
        else
            totalYearsElapsed' d2 d1 * 1.<year>

    // TODO Take account of day of month. Currently does not handle varying month lengths
    /// <summary>Calculates the fractional number of total
    /// months elapsed between two dates.</summary>
    /// <param name="d1">The first date</param>
    /// <param name="d2">The second date</param>
    /// <returns>The fraction of years elapsed between the two dates</returns>
    let totalMonthsElapsed (d1: DateTime) (d2: DateTime) =
        (d2.Month - d1.Month) + (d2.Year - d1.Year) * 12 |> float |> (*) 1.<month>

    let fractionalDifference d1 d2 =
        let d1, d2 = if d1 < d2 then d1, d2 else d2, d1
        { YearFraction = totalYearsElapsed d1 d2
          MonthFraction = totalMonthsElapsed d1 d2
          DayFraction = (d2 - d1).TotalDays * 1.<day>
          RealDifference = d2 - d1
          Ticks = (d2 - d1).Ticks |> float |> (*) 1.<ticks> }


/// <summary>Represents a realistic timespan between two dates when
/// working with observational data and model-fitting. The timespan
/// is limited to being non-zero and below 200 years.</summary>
[<RequireQualifiedAccess>]
module ObservationalTimeSpan =

    type ObservationalTimeSpan = private ObservationalTimeSpan of TimeSpan

    let create t =
        if t = TimeSpan.Zero || t > TimeSpan.FromDays(365. * 200.) then
            None
        else
            t |> ObservationalTimeSpan |> Some

    let private unwrap (ObservationalTimeSpan t) = t

    type ObservationalTimeSpan with
        member this.Value = unwrap this


[<RequireQualifiedAccess>]
module Resolution =

    /// Represents the width of equally-spaced steps in time.
    type FixedTemporalResolution<'timespan> =
        | Years of PositiveInt.PositiveInt<year>
        | Months of PositiveInt.PositiveInt<month>
        | Days of PositiveInt.PositiveInt<day>
        | CustomEpoch of 'timespan

    /// Represents the maximum resolution of the time series
    type TemporalResolution<'timespan> =
        | Fixed of FixedTemporalResolution<'timespan>
        | Variable

    let map fn res =
        match res with
        | Years i -> Years i
        | Months i -> Months i
        | Days i -> Days i
        | CustomEpoch ts -> fn ts |> CustomEpoch

/// Contains types representing common dating methods in
/// long term data analysis.
module DatingMethods =

    /// We assume that dates in old date formats are fixed to
    /// 365 days per year.
    let daysPerYearInOldDates = 365.<day / year>

    /// Converts 'old years' (i.e. with 365 days per year) into
    /// a temporal resolution.
    let internal oldYearsToResolution fixedEpoch =
        Resolution.FixedTemporalResolution.Years(
            PositiveInt.create ((fixedEpoch |> Units.removeUnitFromInt) * 1<year>)
            |> Option.get
        )

    /// <summary>Represents a date made by radiocarbon measurement</summary>
    type Radiocarbon =
        | Radiocarbon of int<``BP (radiocarbon)``>

        static member Unwrap(Radiocarbon bp) = bp

        member this.Value = Radiocarbon.Unwrap this

        static member (-)(e1, e2) =
            (e1 |> Radiocarbon.Unwrap) - (e2 |> Radiocarbon.Unwrap)

        static member (+)(e1, e2) =
            (e1 |> Radiocarbon.Unwrap) + e2 |> Radiocarbon

        static member AddYears date (years: int<year>) =
            date
            |> Radiocarbon.Unwrap
            |> fun date -> date - (years |> Units.removeUnitFromInt |> (*) 1<``BP (radiocarbon)``>)
            |> Radiocarbon

        static member TotalYearsElapsed d1 d2 =
            if d2 > d1 then
                Radiocarbon.Unwrap d2 - Radiocarbon.Unwrap d1
            else
                Radiocarbon.Unwrap d1 - Radiocarbon.Unwrap d2

        static member FractionalDifference d1 d2 =
            let d1, d2 = if d1 < d2 then d1, d2 else d2, d1
            let yearFraction =
                Radiocarbon.Unwrap d2 - Radiocarbon.Unwrap d1 |> float |> (*) 1.<year>

            { YearFraction = yearFraction
              MonthFraction = convertYearsToMonths yearFraction
              DayFraction = yearFraction * daysPerYearInOldDates
              RealDifference = d2 - d1
              Ticks = 0.<ticks> }

    type Annual =
        | Annual of int<year>

        static member Unwrap(Annual bp) = bp

        member this.Value = Annual.Unwrap this

        static member (-)(e1, e2) =
            (e1 |> Annual.Unwrap) - (e2 |> Annual.Unwrap)

        static member (+)(e1, e2) =
            (e1 |> Annual.Unwrap) + e2 |> Annual

        static member AddYears date (years: int<year>) =
            date
            |> Annual.Unwrap
            |> fun date -> date - years
            |> Annual

        static member TotalYearsElapsed d1 d2 =
            if d2 > d1 then
                Annual.Unwrap d2 - Annual.Unwrap d1
            else
                Annual.Unwrap d1 - Annual.Unwrap d2

        static member FractionalDifference d1 d2 =
            let d1, d2 = if d1 < d2 then d1, d2 else d2, d1
            let yearFraction =
                Annual.Unwrap d2 - Annual.Unwrap d1 |> float |> (*) 1.<year>

            { YearFraction = yearFraction
              MonthFraction = convertYearsToMonths yearFraction
              DayFraction = (DateTime(d2.Value |> Units.removeUnitFromInt,01,01) - DateTime(d1.Value |> Units.removeUnitFromInt,01,01)).TotalDays * 1.<day>
              RealDifference = d2 - d1
              Ticks = 0.<ticks> }



module DateMode =

    open DatingMethods

    let internal sort sortBackwards this other =
        if sortBackwards then
            (other :> IComparable<_>).CompareTo this
        else
            (this :> IComparable<_>).CompareTo other


    /// <summary>Represents the configuration for handling the specific
    /// date type `'T` in a time series.</summary>
    type DateMode<'T, 'timeunits, 'timespan> =
        { Resolution: MaximumResolution<'T>
          GetYear: 'T -> 'timeunits
          AddYears: 'T -> int<year> -> 'T
          AddMonths: 'T -> int<month> -> 'T
          AddDays: 'T -> int<day> -> 'T
          AddTime: 'T -> 'timespan -> 'T
          Difference: 'T -> 'T -> TimeDifference<'timespan>
          SortOldestFirst: 'T -> 'T -> int
          ZeroSpan: 'timespan
          TotalDays: 'timespan -> float<day>
          SpanToResolution: 'timespan -> Resolution.FixedTemporalResolution<'timespan>
          ResolutionToSpan: Resolution.FixedTemporalResolution<'timespan> -> 'timespan
          Divide: 'timespan -> 'timespan -> float
          Minus: 'T -> 'T -> 'timespan }

    /// <summary>Represents the maximum resolution possible
    /// given a date type representation.</summary>
    and MaximumResolution<'T> =
        | Ticks of days: ('T -> int<day>) * months: ('T -> int<month>)
        | Year

    let calendarDateMode: DateMode<DateTime, int<year>, TimeSpan> =
        { Resolution = Ticks((fun d -> d.Day * 1<day>), (fun d -> d.Month * 1<month>))
          GetYear = fun d -> d.Year * 1<year>
          AddYears = fun d years -> d.AddYears(years |> Units.removeUnitFromInt)
          AddMonths = fun d months -> months |> Units.removeUnitFromInt |> d.AddMonths
          AddDays = fun d days -> days |> Units.removeUnitFromInt |> d.AddDays
          AddTime = fun d timeSpan -> d + timeSpan
          Difference = DateTime.fractionalDifference
          ZeroSpan = TimeSpan.Zero
          TotalDays = fun ts -> ts.TotalDays * 1.<day>
          SpanToResolution = fun epoch ->
            epoch.Days |> (*) 1<day> |> PositiveInt.create |> Option.get |> Resolution.FixedTemporalResolution.Days
          ResolutionToSpan = fun res -> failwith "not finished"
          Minus = fun d1 d2 -> d1 - d2
          Divide = fun ts1 ts2 -> float ts1.Ticks / float ts2.Ticks
          SortOldestFirst = fun d1 d2 -> if d1 < d2 then -1 else 1 }

    let annualDateMode: DateMode<Annual, int<year>, int<year>> =
        { Resolution = Year
          GetYear = fun d -> d |> Annual.Unwrap
          AddYears = Annual.AddYears
          AddMonths = fun d _ -> d
          AddDays = fun d _ -> d
          AddTime = fun d timeSpan -> d + timeSpan
          Difference = fun d1 d2 -> Annual.FractionalDifference d1 d2
          SortOldestFirst = fun d1 d2 -> if d1 < d2 then -1 else 1
          ZeroSpan = 0<year>
          TotalDays =
            fun years ->
                (years |> Units.removeUnitFromInt |> float |> LanguagePrimitives.FloatWithMeasure)
                * daysPerYearInOldDates
          SpanToResolution = fun epoch -> oldYearsToResolution epoch
          ResolutionToSpan = fun res -> failwith "not finished"
          Divide = fun ts1 ts2 -> ts1 / ts2 |> float
          Minus = fun d1 d2 -> d1 - d2 }

    let radiocarbonDateMode: DateMode<Radiocarbon, int<``BP (radiocarbon)``>, int<``BP (radiocarbon)``>> =
        { Resolution = Year
          GetYear = fun d -> d |> Radiocarbon.Unwrap
          AddYears = Radiocarbon.AddYears
          AddMonths = fun d months -> d
          AddDays = fun d days -> d
          AddTime = fun d timeSpan -> d + timeSpan
          Difference = fun d1 d2 -> Radiocarbon.FractionalDifference d1 d2
          SortOldestFirst = fun d1 d2 -> if d1 > d2 then -1 else 1
          ZeroSpan = 0<``BP (radiocarbon)``>
          TotalDays =
            fun years ->
                (years |> Units.removeUnitFromInt |> float |> LanguagePrimitives.FloatWithMeasure)
                * daysPerYearInOldDates
          SpanToResolution = fun epoch -> oldYearsToResolution epoch
          ResolutionToSpan = fun res -> failwith "not finished"
          Divide = fun ts1 ts2 -> ts1 / ts2 |> float
          Minus = fun d1 d2 -> d1 - d2 }


module TimePoint =

    /// Increment time by an increment defined as a fixed temporal resolution.
    let increment<'date, 'timespan, 'timeunits>
        res
        (mode: DateMode.DateMode<'date, 'timeunits, 'timespan>)
        (date: 'date)
        =
        match res with
        | Resolution.Years i -> mode.AddYears date i.Value
        | Resolution.Months i -> mode.AddMonths date i.Value
        | Resolution.Days i -> mode.AddDays date i.Value
        | Resolution.CustomEpoch ticks -> mode.AddTime date ticks


/// <summary>Contains functions and types to create and manipulate
/// `TimeSeries` values, which represent observations ordered in time.</summary>
[<RequireQualifiedAccess>]
module TimeSeries =

    /// A raw observation, at a given time
    type Observation<'T, 'date> = 'T * 'date

    /// Change in value from one timepoint to another.
    type Epoch<'T, 'timespan> = 'T * 'timespan

    /// A sequence of data observed at time points.
    type FloatingTimeSeries<'T, 'timespan> = private TimeSteps of Epoch<'T, 'timespan>[]

    /// A sequence of data observed at time intervals (regular or irregular),
    /// where the sampling intervals occur following a fixed start observation.
    type TimeSeries<'T, 'date, 'timeunit, 'timespan> = //when 'date : (member TimeUnitProperties : TimeUnitProperties<'timespan>)> =
        private
        | FixedTimeSeries of
            DateMode.DateMode<'date, 'timeunit, 'timespan> *
            Observation<'T, 'date> *
            FloatingTimeSeries<'T, 'timespan>

        member this.StartDate =
            let startPoint (FixedTimeSeries(_, s, _)) = s
            this |> startPoint

        member this.DateMode = this |> fun (FixedTimeSeries(m, _, _)) -> m


    let private innerSeries (FixedTimeSeries(_, s, TimeSteps ts)) = ts
    let private unwrap (FixedTimeSeries(_, s, TimeSteps ts)) = (s, ts)

    let private createFixedSeries dateType start series =
        (dateType, start, series) |> FixedTimeSeries

    let internal checkMoreThanEqualTo n seq =
        if seq |> Seq.length >= n then Some seq else None

    /// <summary>Arrange existing observations as a bristlecone `TimeSeries`.
    /// Observations become ordered and indexed by time.</summary>
    /// <param name="dateType">A `DateMode` that handles the dating mode of choosing.</param>
    /// <param name="dataset">A sequence of observations, which consist of data and dates / times</param>
    /// <typeparam name="'T">The data type of the observations</typeparam>
    /// <typeparam name="'date">The representaion of dates to use</typeparam>
    /// <typeparam name="'dateUnit">The unit in which the dates are represented</typeparam>
    /// <typeparam name="'timespan">The representation of timespans for `'date`</typeparam>
    /// <returns>A time-series of observations ordered in time from oldest to newest.</returns>
    let fromObservations<'T, 'date, 'dateUnit, 'timespan when 'date: equality>
        (dateType: DateMode.DateMode<'date, 'dateUnit, 'timespan>)
        (dataset: seq<Observation<'T, 'date>>)
        =
        if dataset |> Seq.length < 2 then
            invalidArg "dataset" "The data must be at least two elements long"

        if dataset |> Seq.map snd |> Seq.hasDuplicates then
            invalidArg "dataset" "The data cannot have multiple values at the same point in time"

        let sorted: Observation<'T, 'date> seq =
            dataset
            |> Seq.sortWith (fun t1 t2 -> dateType.SortOldestFirst (snd t1) (snd t2))

        let baseline: Observation<'T, 'date> = sorted |> Seq.head

        let timePoints =
            sorted
            |> Seq.tail // The first time point is used as initial state in the scan
            |> Seq.scan
                (fun (_, d1: 'date, _) (v2, d2: 'date) -> (v2, d2, (dateType.Difference d2 d1).RealDifference |> Some))
                (baseline |> fst, baseline |> snd, None)
            |> Seq.tail // The first time point is removed (it forms the baseline instead)
            |> Seq.map (fun (v, d: 'date, ts: 'timespan option) -> (v, ts.Value))
            |> Seq.toArray
            |> TimeSteps

        FixedTimeSeries(dateType, baseline, timePoints)

    /// <summary>Create a time-series where time is represented by uncalibrated
    /// radiocarbon dates (BP dates).</summary>
    /// <param name="dataset">A sequence of observations, which consist of data and dates / times</param>
    /// <typeparam name="'a">The type of the data in the series</typeparam>
    /// <returns>A time-series of BP date observations ordered oldest to newest.</returns>
    let fromRadiocarbonObservations dataset =
        fromObservations DateMode.radiocarbonDateMode dataset

    /// <summary>Create a time-series where time is represented by standard
    /// (modern) calendars and dates and times through the built-in .NET
    /// DateTime type.</summary>
    /// <param name="dataset">A sequence of observations, which consist of data and dates / times</param>
    /// <typeparam name="'a">The type of the data in the series</typeparam>
    /// <returns>A time-series of DateTime observations ordered oldest to newest.</returns>
    let fromNeoObservations dataset =
        fromObservations DateMode.calendarDateMode dataset

    /// <summary>Create a time series from a sequence of existing data, where each
    /// observation is equally spaced in time.</summary>
    /// <param name="timeUnitMode">A `DateMode` that handles the dating mode of choosing.</param>
    /// <param name="t1">The time to fix to the initial observation</param>
    /// <param name="resolution">The temporal resolution of the time-series</param>
    /// <param name="data">The underlying data points</param>
    /// <typeparam name="'date">The representaion of dates to use</typeparam>
    /// <typeparam name="'dateUnit">The unit in which the dates are represented</typeparam>
    /// <typeparam name="'timespan">The representation of timespans for `'date`</typeparam>
    /// <typeparam name="'a">The data type of the observations</typeparam>
    /// <returns></returns>
    let fromSeq (timeUnitMode: DateMode.DateMode<'date, 'timeunit, 'timespan>) t1 resolution (data: seq<'a>) =
        data
        |> Seq.tail // The first time point is used as initial state for the scan
        |> Seq.scan (fun (_, t) v -> (v, t |> TimePoint.increment resolution timeUnitMode)) (data |> Seq.head, t1)
        |> fromObservations timeUnitMode

    /// <summary>Turn a time series into a sequence of observations</summary>
    /// <param name="series">A time-series</param>
    /// <typeparam name="'a">The underlying data type</typeparam>
    /// <typeparam name="'b">The date/time representation</typeparam>
    /// <returns></returns>
    let toObservations series =
        let baseline, ts = unwrap series

        ts
        |> Seq.scan (fun (_, lastDate) (v, ts) -> (v, series.DateMode.AddTime lastDate ts)) baseline

    /// <summary>Apply a function to each observation in the time series.</summary>
    /// <param name="f">A function that takes an observation and returns a transformed data value</param>
    /// <param name="series">A time-series to transform</param>
    /// <typeparam name="'T">The original data type</typeparam>
    /// <typeparam name="'T2">The new data type</typeparam>
    /// <typeparam name="'timespan">The representation of timespans for `'date`</typeparam>
    /// <typeparam name="'date">The representaion of dates to use</typeparam>
    /// <typeparam name="'dateUnit">The unit in which the dates are represented</typeparam>
    /// <returns>A new time-series in which the data values have been transformed</returns>
    let map f (series: TimeSeries<'T, 'date, 'dateUnit, 'timespan>) : TimeSeries<'T2, 'date, 'dateUnit, 'timespan> =
        series
        |> innerSeries
        |> Array.map (fun (v, ts) -> (f (v, ts), ts))
        |> TimeSteps
        |> createFixedSeries
            series.DateMode
            (f (series.StartDate |> fst, series.DateMode.ZeroSpan), series.StartDate |> snd)

    /// <summary>The time intervals - or epochs - that form the time series, where the baseline
    /// time is that defined in the time-series.</summary>
    let epochs series = series |> innerSeries |> Array.map snd

    /// <summary>The date of the last observation within a time series.</summary>
    let endDate series =
        series |> toObservations |> Seq.last |> snd

    /// <summary>The times at which observations that form the time-series were made.</summary>
    let dates series = series |> toObservations |> Seq.map snd

    /// Find an observation by its exact time of occurrence.
    let findExact time series =
        series |> toObservations |> Seq.find (fun (v, t) -> t = time)

    /// <summary>Remove all time points that occur before the desired start date,
    /// still including the specified start date if present.</summary>
    /// <param name="startDate">A start date to clip from</param>
    /// <param name="series">A time-series</param>
    /// <typeparam name="'a">The data type</typeparam>
    /// <typeparam name="'b">The dating method type</typeparam>
    /// <returns>A new time-series with the clipped observations removed</returns>
    let trimStart startDate series =
        let obs = series |> toObservations

        match obs |> Seq.tryFindIndex (fun (_, t) -> t >= startDate) with
        | Some startIndex ->
            obs
            |> Seq.toArray
            |> Array.splitAt startIndex
            |> snd
            |> checkMoreThanEqualTo 2
            |> Option.map (fromObservations series.DateMode)
        | None -> None

    /// <summary>Remove all time points that occur after the desired end date.</summary>
    /// <param name="endDate">An end date to clip beyond</param>
    /// <param name="series">A time-series</param>
    /// <typeparam name="'a">The data type</typeparam>
    /// <typeparam name="'b">The dating method type</typeparam>
    let trimEnd endDate series =
        series
        |> toObservations
        |> Seq.takeWhile (fun (_, t) -> t <= endDate)
        |> checkMoreThanEqualTo 2
        |> Option.map (fromObservations series.DateMode)

    /// <summary>Bound a time series inclusively of the specified
    /// start and end date (if either are present, they will
    /// be included in the new time series).</summary>
    /// <param name="startDate">A start date to clip from</param>
    /// <param name="endDate">An end date to clip beyond</param>
    /// <param name="series">A time-series</param>
    /// <typeparam name="'a">The data type</typeparam>
    /// <typeparam name="'b">The dating method type</typeparam>
    /// <returns></returns>
    let bound startDate endDate series =
        series
        |> toObservations
        |> Seq.skipWhile (fun (_, t) -> t < startDate)
        |> Seq.takeWhile (fun (_, t) -> t <= endDate)
        |> checkMoreThanEqualTo 2
        |> Option.map (fromObservations series.DateMode)

    /// <summary>Calculates the temporal resolution of a time series.</summary>
    /// <param name="series">A time-series of observations</param>
    /// <typeparam name="'a"></typeparam>
    /// <returns>A temporal resolution in days, months, years, or a custom epoch;
    /// the most precise will be returned (e.g. 2 months rather than 61 days).</returns>
    let resolution (series: TimeSeries<'T, 'date, 'dateUnit, 'timespan>) =
        let epochs = series |> epochs

        if epochs |> Seq.distinct |> Seq.length = 1 then // There is a fixed time period.
            let fixedEpoch = epochs |> Seq.head

            if fixedEpoch = series.DateMode.ZeroSpan then
                failwith "Cannot have a resolution of zero"

            if (series.DateMode.TotalDays fixedEpoch) % 1.<day> = 0.<day> then
                fixedEpoch
                |> series.DateMode.SpanToResolution
                |> Resolution.Fixed
            else
                Resolution.Fixed <| Resolution.FixedTemporalResolution.CustomEpoch fixedEpoch
        else // A variable time interval exists. This could be months, years, or some other unit.

            match series.DateMode.Resolution with
            | DateMode.MaximumResolution.Ticks(_, getMonth) ->

                let monthDifferences =
                    series
                    |> toObservations
                    |> Seq.map snd
                    |> Seq.pairwise
                    |> Seq.map (fun (d1, d2) -> (series.DateMode.Difference d1 d2).MonthFraction)

                if monthDifferences |> Seq.distinct |> Seq.length = 1 then // There is a fixed monthly stepping
                    let fixedMonths = monthDifferences |> Seq.head

                    if fixedMonths % 12.<month> = 0.<month> then
                        Resolution.Fixed
                        <| Resolution.FixedTemporalResolution.Years(
                            PositiveInt.create (convertMonthsToYears fixedMonths |> Units.floatToInt)
                            |> Option.get
                        )
                    else
                        Resolution.Fixed
                        <| Resolution.FixedTemporalResolution.Months(
                            PositiveInt.create (fixedMonths |> Units.floatToInt) |> Option.get
                        )
                else // The time series has variable increments not on day or month cycle
                    Resolution.Variable
            | DateMode.MaximumResolution.Year ->

                let yearDifferences =
                    series
                    |> toObservations
                    |> Seq.pairwise
                    |> Seq.map (fun ((_, d1), (_, d2)) -> (series.DateMode.Difference d2 d1).YearFraction)

                if yearDifferences |> Seq.distinct |> Seq.length = 1 then
                    let fixedYears = (yearDifferences |> Seq.head)

                    if fixedYears = (Units.round fixedYears) then
                        Resolution.Fixed
                        <| Resolution.FixedTemporalResolution.Years(
                            PositiveInt.create (fixedYears |> Units.floatToInt) |> Option.get
                        )
                    else
                        Resolution.Variable
                else
                    Resolution.Variable


    /// Reduces the temporal resolution of `series`.
    /// The time series must be able to be split exactly into the lower resolution. For example,
    /// when upscaling from one to three years, the time series must be a multiple of three years.
    let generalise
        desiredResolution
        (upscaleFunction: seq<Observation<'T, 'date>> -> 'T2)
        (series: TimeSeries<'T, 'date, 'timeunit, 'timespan>)
        =
        let resolution = series |> resolution
        let obs = series |> toObservations

        match resolution with
        | Resolution.Variable -> invalidArg "desiredResolution" "Cannot generalise a variable-resolution time series"
        | Resolution.Fixed res ->
            match res with
            | Resolution.FixedTemporalResolution.Years oldYears ->
                match desiredResolution with
                | Resolution.FixedTemporalResolution.Years newYears ->
                    if
                        newYears > oldYears
                        && ((float newYears.Value) % (float oldYears.Value) = 0.)
                        && ((obs |> Seq.length |> float) % (float newYears.Value) = 0.)
                    then
                        obs
                        |> Seq.chunkBySize (newYears.Value / oldYears.Value)
                        |> Seq.map (fun bin -> (bin |> upscaleFunction, bin |> Seq.head |> snd))
                        |> fromObservations series.DateMode
                    else
                        invalidArg
                            "desiredResolution"
                            "The upscaled resolution was not a whole multiple of the old resolution"
                | _ -> invalidArg "desiredResolution" "Cannot generalise an annual time series to a lower resolution"
            | Resolution.FixedTemporalResolution.Days oldDays ->
                match desiredResolution with
                | Resolution.FixedTemporalResolution.Months newMonths ->
                    match series.DateMode.Resolution with
                    | DateMode.MaximumResolution.Year ->
                        invalidArg
                            "series"
                            "Cannot generalise a series that uses year-based dating to monthly resolution"
                    | DateMode.MaximumResolution.Ticks(_, getMonth) ->
                        obs
                        |> Seq.groupBy (fun (_, t) -> (series.DateMode.GetYear t, getMonth t))
                        |> Seq.map (fun ((y, m), g) -> (g |> upscaleFunction, g |> Seq.map snd |> Seq.max))
                        |> fromObservations series.DateMode
                | Resolution.FixedTemporalResolution.Years _
                | Resolution.FixedTemporalResolution.Days _
                | Resolution.FixedTemporalResolution.CustomEpoch _ ->
                    invalidArg
                        "desiredResolution"
                        "Generalising to [year/day/custom-epoch] resolution is currently unsupported"
            | Resolution.FixedTemporalResolution.Months _
            | Resolution.FixedTemporalResolution.CustomEpoch _ ->
                invalidArg "series" "Generalising a monthly or custom-epoch series is currently unsupported"

    /// <summary>Interpolates missing values in a time series, where missing values
    /// are represented as an `Option` type.</summary>
    /// <param name="series">A time-series of option values to interpolate</param>
    /// <typeparam name="'date">A dating method used in the time-series</typeparam>
    /// <typeparam name="'timeunit">A unit of time as used by the dating method</typeparam>
    /// <typeparam name="'timespan">The timespan representation for the dating method</typeparam>
    /// <returns></returns>
    let interpolateFloats (series: TimeSeries<float option, 'date, 'timeunit, 'timespan>) =
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
        |> fromObservations series.DateMode

    /// <summary>Determines if multiple time series have the same temporal extent and time steps.</summary>
    /// <param name="series">A sequence of time-series to assess for commonality</param>
    /// <typeparam name="'T">The data type of the observations</typeparam>
    /// <typeparam name="'date">The representaion of dates to use</typeparam>
    /// <typeparam name="'timeunit">The unit in which the dates are represented</typeparam>
    /// <typeparam name="'timespan">The representation of timespans for `'date`</typeparam>
    /// <returns>An array of 'timespan containing the epochs of the common timeline,
    /// or `None` if there is no common timeline.</returns>
    let commonTimeline (series: TimeSeries<'T, 'date, 'timeunit, 'timespan> seq) =
        let timeSteps =
            series |> Seq.map (toObservations >> Seq.map snd >> Seq.toList) |> Seq.toList

        match timeSteps with
        | Single
        | AllIdentical -> Some(series |> Seq.head |> epochs)
        | Empty
        | Neither -> None

    /// Contains functions for bootstrapping one or many time series.
    module Bootstrap =

        /// <summary> Removes a single observation from a time-series.
        /// For relevant date types, removing a step takes account of leap years.
        /// The starting point is never removed.</summary>
        /// <param name="stepping"></param>
        /// <param name="i"></param>
        /// <param name="series"></param>
        /// <returns>A time series that has the observation at position i removed</returns>
        let bootstrapFixedStep stepping i (series: TimeSeries<'T, 'date, 'timeunit, 'timespan>) =
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
            |> createFixedSeries series.DateMode start

        /// <summary>Randomly remove a single time point from a time-series.</summary>
        /// <param name="random">A random instance to use</param>
        /// <param name="data">A map of time-series values with at least one entry.</param>
        /// <returns>A new map of time-series values with the index entry i removed, and
        /// all time-points after shifted forward in time by one stepping.</returns>
        let removeSingle (random: System.Random) stepping data =
            let commonTimeSeries = commonTimeline (data |> Map.toList |> List.map snd)

            match commonTimeSeries with
            | None -> invalidOp "The time series are not on common time"
            | Some ts ->
                match ts.Length with
                | 0 -> invalidOp "The time series was empty"
                | _ ->
                    let selection = random.Next(0, ts.Length - 1)

                    data |> Map.map (fun _ v -> v |> bootstrapFixedStep stepping selection)


    // Extensions to add convenience methods to time-series
    type TimeSeries<'T, 'date, 'timeunit, 'timespan> with

        member this.Length = this |> innerSeries |> Array.length

        member this.Head = this |> innerSeries |> Array.head

        member this.TimeSteps = this |> innerSeries |> Array.map snd

        member this.Values = this |> toObservations |> Seq.map fst


type TimeSeries<'T, 'date, 'timeunit, 'timespan> = TimeSeries.TimeSeries<'T, 'date, 'timeunit, 'timespan>


/// <summary>Functions to index individual time-series to a common
/// timeline, where time is represented in 'time index' units.</summary>
[<RequireQualifiedAccess>]
module TimeIndex =

    /// When using a time index, if a value is requested within the bounds
    /// of the time-series but not falling on an observed time, a lookup
    /// value may be interpolated using an interpolation function. Here,
    /// `'T` is the data value type.
    type IndexMode<'T> =
        | Interpolate of ((float<``time index``> * 'T) -> (float<``time index``> * 'T) -> float<``time index``> -> 'T)
        | Exact

    /// <summary>Index a single time series in accordance with the baseline 0<``time index``> set as
    /// the specified t0 date.</summary>
    /// <remarks>The temporal index 'time index' values will correspond to the specified
    /// target resolution. For example, if a resolution of two-yearly is specified, an
    /// observation one year after t0 would be indexed as t 0.5, and at three years
    /// t 1.5.</remarks>
    /// <param name="t0">The date to fix as time = 0</param>
    /// <param name="targetResolution">A target resolution that scales the resultant time index values.</param>
    /// <param name="series">A time-series to index.</param>
    /// <typeparam name="'T">The underlying data type of the time-series</typeparam>
    /// <returns>A temporal index </returns>
    let create
        (t0: 'date)
        targetResolution
        (series: TimeSeries.TimeSeries<'T, 'date, 'timeunit, 'timespan>)
        : seq<float<``time index``> * 'T> =
        let obs = series |> TimeSeries.toObservations

        match series.DateMode.Resolution with
        | DateMode.MaximumResolution.Ticks _ ->
            match targetResolution with
            | Resolution.FixedTemporalResolution.Years y ->
                obs
                |> Seq.map (fun (v, tn) ->
                    (((series.DateMode.Difference t0 tn).YearFraction / Units.intToFloat y.Value)
                     |> float
                     |> (*) 1.0<``time index``>,
                     v))
            | Resolution.FixedTemporalResolution.Months m ->
                obs
                |> Seq.map (fun (v, tn) ->
                    (((series.DateMode.Difference t0 tn).MonthFraction / Units.intToFloat m.Value
                      |> float
                      |> (*) 1.0<``time index``>),
                     v))
            | Resolution.FixedTemporalResolution.Days d ->
                obs
                |> Seq.map (fun (v, tn) ->
                    (((series.DateMode.Difference t0 tn).DayFraction / Units.intToFloat d.Value)
                     |> float
                     |> (*) 1.<``time index``>,
                     v))
            | Resolution.FixedTemporalResolution.CustomEpoch(t: 'timespan) ->
                obs
                |> Seq.map (fun (v, tn) ->
                    (series.DateMode.Divide (series.DateMode.Difference tn t0).RealDifference t)
                    |> Units.removeUnitFromFloat
                    |> (*) 1.0<``time index``>,
                    v)

        | DateMode.MaximumResolution.Year ->
            match targetResolution with
            | Resolution.FixedTemporalResolution.Years y ->
                obs
                |> Seq.map (fun (value, tn) ->
                    (((series.DateMode.Difference tn t0).YearFraction / Units.intToFloat y.Value)
                     * 1.0<``time index``>,
                     value))
            | Resolution.FixedTemporalResolution.CustomEpoch t ->
                failwith "Cannot use a custom epoch when the date unit has a maximum resolution of annual"
            | Resolution.FixedTemporalResolution.Months _
            | Resolution.FixedTemporalResolution.Days _ ->
                failwith "Cannot index timeseries that has a maximum resolution of annual by month / day."

    /// <summary>A representation of temporal data as fractions of a common fixed temporal resolution,
    /// from a given baseline. The baseline must be greater than or equal to the baseline
    /// of the time series.</summary>
    type TimeIndex<'T, 'date, 'timeunit, 'timespan>
        (
            baseDate: 'date,
            resolution,
            mode: IndexMode<'T>,
            series: TimeSeries.TimeSeries<'T, 'date, 'timeunit, 'timespan>
        ) =

        let table = series |> create baseDate resolution |> Map.ofSeq
        let tablePairwise = table |> Map.toArray |> Array.pairwise // Ordered in time

        member __.Item

            with get (t): 'T =
                match mode with
                | Exact -> table.[t]
                | Interpolate interpolateFn ->
                    if table.ContainsKey t then
                        table.[t]
                    else
                        // Find the closest points in the index before and after t
                        match
                            tablePairwise
                            |> Array.tryFind (fun ((k1, _), (k2, _)) ->
                                (t - k1) > 0.<``time index``> && (t - k2) < 0.<``time index``>)
                        with
                        | Some((k1, v1), (k2, v2)) -> interpolateFn (k1, v1) (k2, v2) t
                        | None ->
                            invalidOp
                            <| sprintf
                                "Could not interpolate to time %f because it falls outside the range of the temporal index"
                                t

        member __.Baseline = baseDate

        member __.Values = table |> Map.toSeq

        member __.Index = table |> Map.keys


/// A `TimeFrame` contains multiple time-series that use the same
/// temporal index.
[<RequireQualifiedAccess>]
module TimeFrame =

    /// <summary>A 'data frame' that contains multiple time-series
    /// that are aligned and indexed in time.</summary>
    type TimeFrame<'T, 'date, 'timeunit, 'timespan> =
        private | TimeFrame of CodedMap<TimeSeries.TimeSeries<'T, 'date, 'timeunit, 'timespan>>

    /// <summary>Create a timeframe from one or more individual time-series.</summary>
    /// <param name="series"></param>
    /// <typeparam name="'T">The data type of the observations</typeparam>
    /// <typeparam name="'date">The representaion of dates to use</typeparam>
    /// <typeparam name="'timeunit">The unit in which the dates are represented</typeparam>
    /// <typeparam name="'timespan">The representation of timespans for `'date`</typeparam>
    /// <returns>Returns None if the time-series are not on a common timeline.
    /// Otherwise, returns a timeframe containing all of the input time-series</returns>
    let tryCreate (series: CodedMap<TimeSeries.TimeSeries<'T, 'date, 'timeunit, 'timespan>>) =
        let commonTimeline =
            series |> Map.toList |> List.map snd |> TimeSeries.commonTimeline

        match commonTimeline with
        | Some _ -> TimeFrame series |> Some
        | None -> None

    let private inner (TimeFrame frame) = frame

    /// <summary>Identifies the temporal resolution of the data in a timeframe.</summary>
    /// <param name="frame">A timeframe to get the resolution for.</param>
    /// <returns>The temporal resolution of the timeframe.</returns>
    let resolution frame =
        (frame |> inner |> Seq.head).Value |> TimeSeries.resolution

    type TimeFrame<'T, 'date, 'timeunit, 'timespan> with
        member this.StartDate = (this |> inner |> Seq.head).Value.StartDate |> snd
        member this.Series = this |> inner


/// <summary>A specific type of time series that represents
/// growth in a phenomena over time.</summary>
[<RequireQualifiedAccess>]
module GrowthSeries =

    type GrowthSeries<[<Measure>] 'u, [<Measure>] 'time, 'date, 'timeunit, 'timespan> =
        | Cumulative of TimeSeries<float<'u>, 'date, 'timeunit, 'timespan>
        | Absolute of TimeSeries<float<'u / 'time>, 'date, 'timeunit, 'timespan>
        | Relative of TimeSeries<float<'u / 'u>, 'date, 'timeunit, 'timespan>

    let internal dates growth =        
        match growth with
        | Absolute ts -> ts |> TimeSeries.dates
        | Cumulative ts -> ts |> TimeSeries.dates
        | Relative ts -> ts |> TimeSeries.dates

    let bound d1 d2 growth =        
        match growth with
        | Absolute ts -> ts |> TimeSeries.bound d1 d2 |> Option.map Absolute
        | Cumulative ts -> ts |>  TimeSeries.bound d1 d2 |> Option.map Cumulative
        | Relative ts -> ts |>  TimeSeries.bound d1 d2 |> Option.map Relative

    /// <summary>Filter a growth series to only contain observations at the specified dates.</summary>
    let filter times growth =
        let filter' ts =
            times |> Seq.map (fun t -> ((ts |> TimeSeries.findExact t)))
            |> TimeSeries.fromObservations ts.DateMode
        match growth with
        | Absolute ts -> filter' ts |> Absolute
        | Cumulative ts -> filter' ts |> Cumulative
        | Relative ts -> filter' ts |> Relative

    /// <summary>Converts the given growth series into its
    /// cumulative representation (i.e. added over time).</summary>
    let cumulative (growth: GrowthSeries<'u,'time,'date,'timeunit,'timespan>)
        : TimeSeries<float<'u>, 'date, 'timeunit, 'timespan> =
        match growth with
        | Absolute g ->
            let time = g |> TimeSeries.toObservations |> Seq.map snd
            let agr = g |> TimeSeries.toObservations |> Seq.map fst
            let biomass = agr |> Seq.scan (+) 0.<_> |> Seq.tail |> Seq.toList
            TimeSeries.fromObservations g.DateMode (Seq.zip biomass time)
        | Relative _ -> failwith "Not implemented"
        | Cumulative g -> g

    /// <summary>Converts the given growth series into its
    /// relative representation (i.e. current growth rate divided
    /// by current cumulative size).</summary>
    let relative (growth: GrowthSeries<'u,'v,'date,'timeunit,'timespan>)
        : TimeSeries<float<'u / 'u>, 'date, 'timeunit, 'timespan> =
        match growth with
        | Absolute g ->
            let time = g |> TimeSeries.toObservations |> Seq.map snd
            let agr = g |> TimeSeries.toObservations |> Seq.map fst
            let biomass = agr |> Seq.scan (+) 0.<_> |> Seq.tail |> Seq.toList
            let rgr = agr |> Seq.mapi (fun i a -> a / biomass.[i])
            TimeSeries.fromObservations g.DateMode (Seq.zip rgr time)
        | Relative g -> g
        | Cumulative g ->
            // Calculate agr and divide agr by biomass
            failwith "Not implemented"

    let asCumulative plant = plant |> cumulative |> Cumulative
    let asRelative plant = plant |> relative |> Relative

    let internal stripUnits growth =
        match growth with
        | Absolute ts -> ts |> TimeSeries.map(fun (t,v) -> Units.removeUnitFromFloat t)
        | Cumulative ts -> ts |> TimeSeries.map(fun (t,v) -> Units.removeUnitFromFloat t)
        | Relative ts -> ts |> TimeSeries.map(fun (t,v) -> Units.removeUnitFromFloat t)
