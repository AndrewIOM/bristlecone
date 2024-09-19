module Bristlecone.Time

open System

[<Measure>] type day
[<Measure>] type month
[<Measure>] type year

// Support the following dating methods:
[<Measure>] type ``cal yr BP`` // calibrated calendar years before present
[<Measure>] type ``BP (radiocarbon)`` // uncalibrated years before present
[<Measure>] type CE // common era
[<Measure>] type BC
[<Measure>] type AD

[<Measure>] type ``time index``

type TimeDifference = {
    DayFraction: float<day>
    MonthFraction: float<month>
    YearFraction: float<year>
}

// Unit conversions:
let internal convertYearsToMonths (x : float<year>) = x * 12.<month/year>


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

    let fractionalDifference d1 d2 = {
        YearFraction = totalYearsElapsed d1 d2
        MonthFraction = totalMonthsElapsed d1 d2
        DayFraction = (d2 - d1).TotalDays * 1.<day>
    }


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

/// Contains types representing common dating methods in
/// long term data analysis.
module DatingMethods =

    /// We assume that dates in old date formats are fixed to
    /// 365 days per year.
    let daysPerYearInOldDates = 365.<day/year>

    /// <summary>Represents a date made by radiocarbon measurement</summary>
    type Radiocarbon = Radiocarbon of int<``BP (radiocarbon)``> with
        static member Unwrap (Radiocarbon bp) = bp
        static member (-) (e1, e2) =
            (e1 |> Radiocarbon.Unwrap) - (e2 |> Radiocarbon.Unwrap)
        static member (+) (e1, e2) =
            (e1 |> Radiocarbon.Unwrap) + e2 |> Radiocarbon

        static member TotalYearsElapsed d1 d2 =
            if d2 > d1 then Radiocarbon.Unwrap d2 - Radiocarbon.Unwrap d1
            else Radiocarbon.Unwrap d1 - Radiocarbon.Unwrap d2

        static member FractionalDifference d1 d2 =
            let yearFraction = Radiocarbon.Unwrap d2 - Radiocarbon.Unwrap d1 |> float |> (*) 1.<year>
            {
                YearFraction = yearFraction
                MonthFraction = convertYearsToMonths yearFraction
                DayFraction = yearFraction * daysPerYearInOldDates
            }


module DateMode =
    
    open DatingMethods

    /// <summary>Represents the configuration for handling the specific
    /// date type `'T` in a time series.</summary>
    [<CustomComparison>]
    type DateMode<'T, 'timeunits, 'timespan> = {
        Resolution: MaximumResolution<'T>
        GetYear: 'T -> 'timeunits
        AddYears : 'T -> 'timeunits -> 'T
        AddMonths : 'T -> int<month> -> 'T
        AddDays : 'T -> int<day> -> 'T
        AddTime : 'T -> 'timespan -> 'T
        Difference : 'T -> 'T -> TimeDifference
        SortBackwards: bool
        ZeroSpan: 'timespan
        TotalDays: 'timespan -> float<day>
    }
    with
        interface IComparable<DateMode<'T, 'timeunits, 'timespan>> with
            member this.CompareTo other =
                if this.SortBackwards
                then (other :> IComparable<_>).CompareTo this
                else (this :> IComparable<_>).CompareTo other

    /// <summary>Represents the maximum resolution possible
    /// given a date type representation.</summary>
    and MaximumResolution<'T> =
        | Ticks of days:('T -> int<day>) * months:('T -> int<month>)
        | Year

    let calendarDateMode : DateMode<DateTime, int, TimeSpan> = {
        Resolution = Ticks((fun d -> d.Day * 1<day>), (fun d -> d.Month * 1<month>))
        GetYear = fun d -> d.Year
        AddYears = fun d years -> d.AddYears years
        AddMonths = fun d months -> months |> Units.removeUnitFromInt |> d.AddMonths
        AddDays = fun d days -> days |> Units.removeUnitFromInt |> d.AddDays
        AddTime = fun d timeSpan -> d + timeSpan
        Difference = DateTime.fractionalDifference
        SortBackwards = false
        ZeroSpan = TimeSpan.Zero
        TotalDays = fun ts -> ts.TotalDays * 1.<day>
    }

    let radiocarbonDateMode : DateMode<Radiocarbon, int<``BP (radiocarbon)``>, int<``BP (radiocarbon)``>> = {
        Resolution = Year
        GetYear = fun d -> d |> Radiocarbon.Unwrap
        AddYears = fun d years -> d + years
        AddMonths = fun d months -> d
        AddDays = fun d days -> d
        AddTime = fun d timeSpan -> d + timeSpan
        Difference = fun d1 d2 -> Radiocarbon.FractionalDifference d1 d2
        SortBackwards = true
        ZeroSpan = 0<``BP (radiocarbon)``>
        TotalDays = fun ts -> (ts |> Units.removeUnitFromInt |> float |> LanguagePrimitives.FloatWithMeasure) * daysPerYearInOldDates
    }

module TimePoint =

    /// Increment time by an increment defined as a fixed temporal resolution.
    let increment<'date, 'timespan> res (mode: DateMode.DateMode<'date,_,'timespan>) (date:'date) =
        match res with
        | Resolution.Years i -> mode.AddYears date i.Value
        | Resolution.Months i -> mode.AddMonths date i.Value
        | Resolution.Days i -> mode.AddDays date i.Value
        | Resolution.CustomEpoch ticks -> mode.AddTime date ticks


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
        | FixedTimeSeries of DateMode.DateMode<DatingMethods.Radiocarbon, 'timeunit, int<``BP (radiocarbon)``>> * Observation<'T, DatingMethods.Radiocarbon> * FloatingTimeSeries<'T, int<``BP (radiocarbon)``>>

        member this.StartDate =
            let startPoint (FixedTimeSeries(_,s, _)) = s
            this |> startPoint

        member this.DateMode = this |> fun (FixedTimeSeries(m,_,_)) -> m


    let private innerSeries (FixedTimeSeries(_,s, TimeSteps ts)) = ts
    let private unwrap (FixedTimeSeries(_,s, TimeSteps ts)) = (s, ts)
    let private createFixedSeries dateType start series = (dateType, start, series) |> FixedTimeSeries

    /// Arrange existing observations as a bristlecone `TimeSeries`.
    /// Observations become ordered and indexed by time.
    let fromObservations (dateType:DateMode.DateMode<'date,'dateUnit,'timespan>) (dataset: seq<Observation<'T, 'date>>) =
        if dataset |> Seq.length < 2 then
            invalidArg "dataset" "The data must be at least two elements long"

        let sorted = dataset |> Seq.sortBy snd
        let baseline : Observation<'T, 'date> = sorted |> Seq.head

        let timePoints =
            sorted
            |> Seq.tail // The first time point is used as initial state in the scan
            |> Seq.scan
                (fun (_, d1: 'date, _) (v2, d2: 'date) ->
                    (v2, d2, Some (d2 - d1)))
                (baseline |> fst, baseline |> snd, None)
            |> Seq.tail // The first time point is removed (it forms the baseline instead)
            |> Seq.map (fun (v, _, ts) -> (v, ts.Value))
            |> Seq.toArray
            |> TimeSteps

        FixedTimeSeries(dateType, baseline, timePoints)

    /// Create a time series from a sequence of existing data, where each
    /// observation is equally spaced in time.
    let fromSeq (timeUnitMode:DateMode.DateMode<'date,'b,'timespan>) t1 resolution (data: seq<'a>) =
        data
        |> Seq.tail // The first time point is used as initial state for the scan
        |> Seq.scan (fun (_, t) v -> (v, t |> TimePoint.increment resolution timeUnitMode)) (data |> Seq.head, t1)
        |> fromObservations timeUnitMode

    /// Turn a time series into a sequence of observations
    let toObservations (series:TimeSeries<'a,'b,'c,'d>) =
        let baseline, ts = unwrap series
        ts |> Seq.scan (fun (_, lastDate) (v, ts) -> (v, lastDate + ts)) baseline

    /// Map a function to each value in the time series.
    let map f series =
        series
        |> innerSeries
        |> Array.map (fun (v, ts) -> (f (v, ts), ts))
        |> TimeSteps
        |> createFixedSeries series.DateMode (f (series.StartDate |> fst, series.DateMode.ZeroSpan), series.StartDate |> snd)

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
            |> Option.map (fromObservations series.DateMode)
        | None -> None

    /// Remove all time points that occur after the desired end date.
    let trimEnd endDate series =
        series
        |> toObservations
        |> Seq.takeWhile (fun (_, t) -> t <= endDate)
        |> checkMoreThanEqualTo 2
        |> Option.map (fromObservations series.DateMode)

    /// Bound a time series inclusively
    let bound startDate endDate series =
        series
        |> toObservations
        |> Seq.skipWhile (fun (_, t) -> t < startDate)
        |> Seq.takeWhile (fun (_, t) -> t <= endDate)
        |> checkMoreThanEqualTo 2
        |> Option.map (fromObservations series.DateMode)

    /// Date of the last observation within the time series.
    let endDate series =
        series |> toObservations |> Seq.last |> snd

    /// Time points of sampling within the time series.
    let dates series = series |> toObservations |> Seq.map snd

    /// Find an observation by its exact time of occurrence.
    let findExact time series =
        series |> toObservations |> Seq.find (fun (v, t) -> t = time)

    /// Calculates the temporal resolution of a time series
    let resolution series =
        let epochs = series |> epochs

        if epochs |> Seq.distinct |> Seq.length = 1 then // There is a fixed time period.
            let fixedEpoch = epochs |> Seq.head

            if fixedEpoch = series.DateMode.ZeroSpan then
                failwith "Cannot have a resolution of zero"

            if (series.DateMode.TotalDays fixedEpoch) % 1.<day> = 0.<day> then
                Resolution.Fixed
                <| Resolution.FixedTemporalResolution.Days(PositiveInt.create (series.DateMode.TotalDays fixedEpoch |> Units.floatToInt) |> Option.get)
            else
                Resolution.Fixed
                <| Resolution.FixedTemporalResolution.CustomEpoch fixedEpoch
        else // A variable time interval exists. This could be months, years, or some other unit.
            let observations = series |> toObservations
            let startDate = observations |> Seq.head |> snd

            match series.DateMode.Resolution with
            | DateMode.MaximumResolution.Ticks (_, getMonth) ->

                let monthDifferences =
                    observations
                    |> Seq.map snd
                    |> Seq.tail
                    |> Seq.scan
                        (fun (t1, m) t2 -> (t2, (series.DateMode.GetYear t2 * 12) + getMonth t2))
                        (startDate, (series.DateMode.GetYear startDate * 12) + getMonth startDate)
                    |> Seq.map snd
                    |> Seq.pairwise
                    |> Seq.map (fun (m1, m2) -> m2 - m1)

                if monthDifferences |> Seq.distinct |> Seq.length = 1 then // There is a fixed monthly stepping
                    let fixedMonths = monthDifferences |> Seq.head

                    if fixedMonths % 12 = 0 then
                        Resolution.Fixed
                        <| Resolution.FixedTemporalResolution.Years(PositiveInt.create (fixedMonths / 12) |> Option.get)
                    else
                        Resolution.Fixed
                        <| Resolution.FixedTemporalResolution.Months(PositiveInt.create fixedMonths |> Option.get)
                else // The time series has variable increments not on day or month cycle
                    Resolution.Variable
            | DateMode.MaximumResolution.Year ->

                let yearDifferences =
                    observations
                    |> Seq.map(fun t -> t |> snd |> series.DateMode.GetYear)
                    |> Seq.pairwise
                    |> Seq.map (fun (m1, m2) -> m2 - m1)

                if yearDifferences |> Seq.distinct |> Seq.length = 1 then
                    let fixedYears = (yearDifferences |> Seq.head) * 1<year>
                    Resolution.Fixed
                    <| Resolution.FixedTemporalResolution.Years(PositiveInt.create fixedYears |> Option.get)
                else Resolution.Variable


    /// Reduces the temporal resolution of `series`.
    /// The time series must be able to be split exactly into the lower resolution. For example,
    /// when upscaling from one to three years, the time series must be a multiple of three years.
    let generalise desiredResolution (upscaleFunction: seq<Observation<'T,'date>> -> 'T2) (series: TimeSeries<'T,'date,'timeunit,'timespan>) =
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
                    | DateMode.MaximumResolution.Year -> invalidArg "series" "Cannot generalise a series that uses year-based dating to monthly resolution"
                    | DateMode.MaximumResolution.Ticks (_, getMonth) ->
                        obs
                        |> Seq.groupBy (fun (_, t) -> (series.DateMode.GetYear t, getMonth t))
                        |> Seq.map (fun ((y, m), g) -> (g |> upscaleFunction, g |> Seq.map snd |> Seq.max))
                        |> fromObservations series.DateMode
                | Resolution.FixedTemporalResolution.Years _
                | Resolution.FixedTemporalResolution.Days _
                | Resolution.FixedTemporalResolution.CustomEpoch _ ->
                    invalidArg "desiredResolution" "Generalising to [year/day/custom-epoch] resolution is currently unsupported"
            | Resolution.FixedTemporalResolution.Months _
            | Resolution.FixedTemporalResolution.CustomEpoch _ ->
                invalidArg "series" "Generalising a monthly or custom-epoch series is currently unsupported"

    /// Interpolates missing values in a time series, where missing values
    /// are represented as an `Option` type.
    let interpolate (series: TimeSeries<'T,'a,'b,'c>) =
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
    /// <param name="series">A sequence of `TimeSeries`</param>
    /// <returns>A `TimeSpan [] option` containing the epochs of the common timeline, 
    /// or `None` if there is no common timeline.</returns>
    let commonTimeline series =
        let timeSteps = series |> Seq.map (toObservations >> Seq.map snd >> Seq.toList) |> Seq.toList

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
            |> createFixedSeries series.DateMode start

        /// <summary>Randomly remove a single time point from a time-series.</summary>
        /// <param name="random">A random instance to use</param>
        /// <param name="data">A map where the values correspond to time series, with at least one entry.</param>
        /// <returns></returns>
        let removeSingle (random: System.Random) data =
            let commonTimeSeries =
                commonTimeline (data |> Map.toList |> List.map snd)

            match commonTimeSeries with
            | None -> invalidOp "The time series are not on common time"
            | Some ts ->
                match ts.Length with
                | 0 -> invalidOp "The time series was empty"
                | _ ->
                    let selection = random.Next(0, ts.Length - 1)

                    data
                    |> Map.map (fun _ v -> v |> bootstrapFixedStep (TimeSpan.FromDays(366.)) selection)


    type TimeSeries<'T, 'date, 'timeunit, 'timespan> with
        member this.Resolution = this |> resolution
        member this.Length = this |> innerSeries |> Array.length

        member this.Head = this |> innerSeries |> Array.head

        member this.TimeSteps = this |> innerSeries |> Array.map snd

        member this.Values = this |> toObservations |> Seq.map fst


type TimeSeries<'T, 'date, 'timeunit, 'timespan> = TimeSeries.TimeSeries<'T, 'date, 'timeunit, 'timespan>


module TimeIndex =

    /// When using a time index, if a value is requested within the bounds
    /// of the time-series but not falling on an observed time, a lookup
    /// value may be interpolated using an interpolation function. Here,
    /// `'T` is the data value type.
    type IndexMode<'T> =
        | Interpolate of ((float<``time index``> * 'T) -> (float<``time index``> * 'T) -> float<``time index``> -> 'T)
        | Exact

    /// Indexes the time series in accordance with a `baselineTime` and fixed `resolution`.
    /// Where the time series is of a lower resolution
    let indexSeries (t0: DatingMethods.Radiocarbon) targetResolution (series: TimeSeries<'T, 'date, 'timeunit, 'timespan>) : seq<float<``time index``> * 'a> =
        let obs = series |> TimeSeries.toObservations

        match series.DateMode.Resolution with
        | DateMode.MaximumResolution.Ticks (getDay, getMonth) ->
            match targetResolution with
            | Resolution.FixedTemporalResolution.Years y ->
                obs 
                |> Seq.map (fun (v, tn) ->
                    (((DateTime.totalYearsElapsed t0 tn) / y.Value), v))
            | Resolution.FixedTemporalResolution.Months m ->
                obs
                |> Seq.map (fun (v, tn) -> (((DateTime.totalMonthsElapsed t0 tn) / float m.Value), v))
            | Resolution.FixedTemporalResolution.Days d -> obs |> Seq.map (fun (v, tn) -> (((tn - t0).TotalDays / float d.Value), v))
            | Resolution.FixedTemporalResolution.CustomEpoch t ->
                obs
                |> Seq.map (fun (v, tn) -> ((((tn - t0).Ticks / t.Value.Ticks) |> float), v))

        | DateMode.MaximumResolution.Year ->
            match targetResolution with
            | Resolution.FixedTemporalResolution.Years y ->
                obs 
                |> Seq.map (fun (value, tn) -> 
                    ((float (tn - t0) / float y.Value) * 1.0<``time index``>, value))
            | Resolution.FixedTemporalResolution.CustomEpoch t ->
                obs 
                |> Seq.map (fun (value, tn) -> 
                    ((float (tn - t0) / t) * 1.0<``time index``>, value))
            | Resolution.FixedTemporalResolution.Months _
            | Resolution.FixedTemporalResolution.Days _ ->
                failwith "Cannot index timeseries that has a maximum resolution of annual by month / day."



    /// <summary>A representation of temporal data as fractions of a common fixed temporal resolution,
    /// from a given baseline. The baseline must be greater than or equal to the baseline
    /// of the time series.</summary>
    type TimeIndex<'T>(baseDate: 'date, resolution, mode: IndexMode<'T>, series: TimeSeries<'T, 'date, 'timeunit, 'timespan>) =
        let table = series |> indexSeries baseDate resolution |> Map.ofSeq
        let tablePairwise = table |> Map.toArray |> Array.pairwise // Ordered in time

        member __.Item

            with get (t) : 'T =
                match mode with
                | Exact -> table.[t]
                | Interpolate interpolateFn ->
                    if table.ContainsKey t then
                        table.[t]
                    else
                        // Find the closest points in the index before and after t
                        match
                            tablePairwise
                            |> Array.tryFind (fun ((k1, _), (k2, _)) -> (t - k1) > 0.<``time index``> && (t - k2) < 0.<``time index``>)
                        with
                        | Some((k1, v1), (k2, v2)) -> interpolateFn (k1, v1) (k2, v2) t
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

    type TimeFrame<'T, 'date, 'timeunit, 'timespan> = private TimeFrame of CodedMap<TimeSeries.TimeSeries<'T, 'date, 'timeunit, 'timespan>>

    let tryCreate series =
        let commonTimeline =
            series |> Map.toList |> List.map snd |> TimeSeries.commonTimeline

        match commonTimeline with
        | Some _ -> TimeFrame series |> Some
        | None -> None

    let private inner (TimeFrame frame) = frame

    type TimeFrame<'T, 'date, 'timeunit, 'timespan> with
        member this.Resolution = (this |> inner |> Seq.head).Value.Resolution
        member this.StartDate = (this |> inner |> Seq.head).Value.StartDate |> snd
        member this.Series = this |> inner


/// <summary>A specific type of time series that represents
/// growth in a phenomena over time.</summary>
[<RequireQualifiedAccess>]
module GrowthSeries =

    [<Measure>] type mm

    type GrowthSeries<[<Measure>] 'u, [<Measure>] 'time, 'date, 'timeunit, 'timespan> =
        | Cumulative of TimeSeries<float<'u>,'date,'timeunit, 'timespan>
        | Absolute of TimeSeries<float<'u / 'time>,'date,'timeunit, 'timespan>
        | Relative of TimeSeries<float<'u / 'u>,'date,'timeunit, 'timespan>
