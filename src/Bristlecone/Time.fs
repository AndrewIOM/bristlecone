namespace Bristlecone

open System

[<AutoOpen>]
module Time =

    type TimeSpan with
        static member Multiply (two:int) (one:TimeSpan) = 
            one.Ticks * (int64 two)
            |> TimeSpan

    type DateTime with
        static member Create day month year =
            let d = DateTime(year, month, day)
            DateTime.SpecifyKind(d, DateTimeKind.Utc)

    type FixedTemporalResolution =
        | Years of int
        | Months of int
        | Days of int
        | CustomEpoch of TimeSpan

    /// Increment time by an increment defined as a fixed temporal resolution.
    let increment res (t:DateTime) =
        match res with
        | Years i -> t.AddYears i
        | Months i -> t.AddMonths i
        | Days i -> t.AddDays(float i)
        | CustomEpoch ticks -> t + ticks

    let (|Single|Empty|AllIdentical|Neither|) (lst:'a list) =
        if lst.Length = 1 then Single
        elif lst.Length = 0 then Empty
        elif List.forall (fun elem -> elem = lst.[0]) lst then
            AllIdentical
        else
            Neither

    [<AutoOpen>]
    module TimeSeries =

        /// A raw observation, at a given time
        type Observation<'T> = 'T * DateTime

        /// Change in value from one timepoint to another.
        type Epoch<'T> = 'T * TimeSpan

        /// Represents the maximum resolution of the time series
        type TemporalResolution =
            | Fixed of FixedTemporalResolution
            | Variable

        /// A sequence of data observed at time points. 
        type FloatingTimeSeries<'T> = private TimeSteps of Epoch<'T>[]
        
        /// A sequence of data observed at time intervals (regular or irregular),
        /// where the sampling intervals occur following a fixed start observation.
        type TimeSeries<'T> = private FixedTimeSeries of Observation<'T> * FloatingTimeSeries<'T>

        with
            member this.StartDate = 
                let startPoint (FixedTimeSeries (s,_)) = s
                this |> startPoint


        let private innerSeries (FixedTimeSeries (s,TimeSteps ts)) = ts
        let private unwrap (FixedTimeSeries (s,TimeSteps ts)) = (s, ts)
        let private createFixedSeries start series = (start, series) |> FixedTimeSeries

        /// Arrange existing observations as a bristlecone `TimeSeries`.
        /// Observations become ordered and indexed by time.
        let fromObservations (dataset:seq<Observation<'a>>) : TimeSeries<'a> =
            if dataset |> Seq.length < 2 then invalidArg "dataset" "The data must be at least two elements long"
            let sorted = dataset |> Seq.sortBy snd
            let baseline = sorted |> Seq.head
            let timePoints =
                sorted
                |> Seq.tail // The first time point is used as initial state in the scan
                |> Seq.scan(fun (v1,d1,ts) (v2,d2) -> (v2, d2, d2 - d1) ) (baseline |> fst, baseline |> snd, TimeSpan.Zero)
                |> Seq.tail // The first time point is removed (it forms the baseline instead)
                |> Seq.map (fun (v,_,ts) -> (v, ts))
                |> Seq.toArray
                |> TimeSteps
            FixedTimeSeries (baseline, timePoints)

        /// Create a time series from a sequence of existing data, where each
        /// observation is equally spaced in time.  
        let fromSeq t1 resolution (data:seq<'a>) =
            data
            |> Seq.tail // The first time point is used as initial state for the scan
            |> Seq.scan(fun (_,t) v -> (v, t |> increment resolution)) (data |> Seq.head, t1)
            |> fromObservations

        /// Turn a time series into a sequence of observations
        let toObservations series : seq<Observation<'a>> =
            let baseline, ts = series |> unwrap
            ts |> Seq.scan(fun (_,lastDate) (v,ts) -> (v, lastDate + ts)) baseline

        /// Map a function to each value in the time series.
        let map f series =
            series 
            |> innerSeries 
            |> Array.map (fun (v,ts) -> (f (v, ts), ts))
            |> TimeSteps
            |> createFixedSeries (f (series.StartDate |> fst, TimeSpan.Zero), series.StartDate |> snd)

        /// The time intervals - or *epochs* - that form the time series.
        let epochs series =
            series |> innerSeries |> Array.map snd

        /// Remove all time points that occur before the desired start date.
        let trimStart startDate series =
            let obs = series |> toObservations
            let startIndex = obs |> Seq.findIndex (fun (_,t) -> t >= startDate)
            obs |> Seq.toArray |> Array.splitAt startIndex |> snd |> fromObservations

        /// Remove all time points that occur after the desired end date.
        let trimEnd endDate series =
            series 
            |> toObservations
            |> Seq.takeWhile (fun (_,t) -> t <= endDate)
            |> fromObservations

        /// Bound a time series inclusively 
        let bound startDate endDate series =
            series 
            |> toObservations
            |> Seq.skipWhile (fun (_,t) -> t < startDate)
            |> Seq.takeWhile (fun (_,t) -> t <= endDate)
            |> fromObservations

        /// Date of the last observation within the time series.
        let endDate (series:TimeSeries<'a>) =
            series |> toObservations |> Seq.last |> snd

        /// Time points of sampling within the time series.
        let dates (series:TimeSeries<'a>) =
            series |> toObservations |> Seq.map snd

        /// Find an observation by its exact time of occurrence.
        let findExact time (series:TimeSeries<'a>) =
            series |> toObservations |> Seq.find(fun (v,t) -> t = time)

        /// Calculates the temporal resolution of a time series
        let resolution (series:TimeSeries<'a>) : TemporalResolution =
            let epochs = series |> epochs
            if epochs |> Seq.distinct |> Seq.length = 1
            then // There is a fixed time period.
                let fixedEpoch = epochs |> Seq.head
                if fixedEpoch.TotalDays % 1. = 0.
                then Fixed <| Days (int (fixedEpoch.TotalDays))
                else Fixed <| CustomEpoch fixedEpoch
            else // A variable time interval exists. This could be months, years, or some other unit.
                let observations = series |> toObservations
                let startDate = observations |> Seq.head |> snd
                let monthDifferences =
                    observations
                    |> Seq.map snd
                    |> Seq.tail
                    |> Seq.scan (fun (t1,m) t2 -> (t2, (t2.Year * 12) + t2.Month)) (startDate, (startDate.Year * 12) + startDate.Month)
                    |> Seq.map snd
                    |> Seq.pairwise
                    |> Seq.map(fun (m1,m2) -> m2 - m1)
                if monthDifferences |> Seq.distinct |> Seq.length = 1
                then // There is a fixed monthly stepping
                    let fixedMonths = monthDifferences |> Seq.head
                    if fixedMonths % 12 = 0
                    then Fixed <| Years (fixedMonths / 12)
                    else Fixed <| Months fixedMonths
                else // The time series has variable increments not on day or month cycle
                    Variable

        /// Reduces the temporal resolution of `series`. 
        /// The time series must be able to be split exactly into the lower resolution. For example,
        /// when upscaling from one to three years, the time series must be a multiple of three years.
        let generalise desiredResolution (upscaleFunction:seq<Observation<'a>>->'a) (series:TimeSeries<'a>) =
            let resolution = series |> resolution
            let obs = series |> toObservations
            match resolution with
            | Variable -> invalidArg "desiredResolution" "Cannot generalise a variable-resolution time series"
            | Fixed res ->
                match res with
                | Years oldYears ->
                    match desiredResolution with
                    | Years newYears ->
                        if newYears > oldYears && ((float newYears) % (float oldYears) = 0.) && ((obs |> Seq.length |> float) % (float newYears) = 0.)
                        then obs |> Seq.chunkBySize (newYears / oldYears) |> Seq.map (fun bin -> (bin |> upscaleFunction, bin |> Seq.head |> snd)) |> fromObservations
                        else invalidArg "desiredResolution" "The upscaled resolution was not a whole multiple of the old resolution"
                    | _ -> invalidArg "desiredResolution" "Cannot generalise an annual time series to a lower resolution"
                | _ -> invalidArg "desiredResolution" "Not implemented"

        ///**Description**  
        /// Determines if multiple time series have the same temporal extent and time steps.
        ///**Output Type**  
        ///  * A `TimeSpan [] option` containing the common timeline, or `None` if there is no common timeline.
        let commonTimeline (series:TimeSeries<'a> list) =
            let timeSteps = series |> List.map (toObservations >> Seq.map snd >> Seq.toList)
            match timeSteps with
            | Single
            | AllIdentical -> Some (series |> List.head |> epochs)
            | Empty
            | Neither -> None

        let validateCommonTimeline series =
            let commonTimeline = 
                series
                |> Map.toList
                |> List.map snd
                |> commonTimeline
            match commonTimeline with
            | Some _ -> series
            | None -> invalidOp (sprintf "These series do not share a common timeline: %A" series)

        /// Removing a step takes account of leap years.
        /// NB Never removes the original start point.
        let bootstrapFixedStep stepping i series =
            let start, _ = series |> unwrap
            let newData =
                series
                |> innerSeries
                |> Array.map fst
                |> Array.toList |> List.remove i |> List.toArray
            (List.init (Seq.length newData) (fun _ -> stepping))
            |> Seq.zip newData
            |> Seq.toArray
            |> TimeSteps
            |> createFixedSeries start


        [<AutoOpen>]
        module TimeSeriesExtensions =

            type TimeSeries<'T> with
                member this.Length =
                    this |> innerSeries |> Array.length

                member this.Head =
                    this |> innerSeries |> Array.head

                member this.TimeSteps =
                    this |> innerSeries |> Array.map snd

                member this.Values =
                    this |> toObservations |> Seq.map fst

                member this.Resolution = 
                    this |> resolution


    module TimeIndex =

            type IndexMode<'a> =
                | Interpolate of ((float*'a) -> (float*'a) -> float -> 'a)
                | Exact

            /// Calculates the fractional number of years elapsed between two dates.
            let totalYearsElapsed (d1:DateTime) (d2:DateTime) =
                let yf1 = float d1.DayOfYear / (if DateTime.IsLeapYear d1.Year then 366. else 365.)
                let yf2 = float d2.DayOfYear / (if DateTime.IsLeapYear d2.Year then 366. else 365.)
                let wholeYears =
                    let rec addYear (d:DateTime) i =
                        let x = d.AddYears(1)
                        if x > d2 then i else addYear x (i + 1)
                    addYear d1 0
                yf2 - yf1 + float wholeYears

            // TODO Take account of day of month. Currently does not handle varying month lengths
            let totalMonthsElapsed (d1:DateTime) (d2:DateTime) : float =
                (d2.Month - d1.Month) + (d2.Year - d1.Year) * 12 |> float

            /// Indexes the time series in accordance with a `baselineTime` and fixed `resolution`.
            /// Where the time series is of a lower resolution 
            let indexSeries (t0:DateTime) targetResolution (series:TimeSeries<'a>) : seq<float*'a> =
                let obs = series |> toObservations
                match targetResolution with
                | Years y -> obs |> Seq.map(fun (v,tn) -> (((totalYearsElapsed t0 tn) / float y), v))
                | Months m -> obs |> Seq.map(fun (v,tn) -> (((totalMonthsElapsed t0 tn) / float m), v))
                | Days d -> obs |> Seq.map(fun (v,tn) -> (((tn - t0).TotalDays / float d), v))
                | CustomEpoch t -> obs |> Seq.map(fun (v,tn) -> ((((tn - t0).Ticks / t.Ticks) |> float), v))


            /// A representation of temporal data as fractions of a common fixed temporal resolution,
            /// from a given baseline. The baseline must be greater than or equal to the baseline
            /// of the time series.
            type TimeIndex<'a>(baseDate,resolution,mode,series:TimeSeries<'a>) =
                let table = series |> indexSeries baseDate resolution |> Map.ofSeq
                member __.Item

                    with get(t) : 'a = 
                        match mode with
                        | Exact -> table.[t]
                        | Interpolate i ->
                            if table.ContainsKey t then table.[t]
                            else
                                // Find the closest points in the index before and after t
                                match table |> Seq.pairwise |> Seq.tryFind(fun (k1,k2) -> (t - k1.Key) > 0. && (t - k2.Key ) < 0.) with
                                | Some (p1,p2) -> i (p1.Key, p1.Value) (p2.Key, p2.Value) t
                                | None -> invalidOp <| sprintf "Could not interpolate to time %f because it falls outside the range of the temporal index" t

                member __.Baseline = baseDate

                member __.Values = table |> Map.toSeq


    [<AutoOpen>]
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
