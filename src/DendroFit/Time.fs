module Time

open System

type TimeSpan with
    static member Multiply (two:int) (one:TimeSpan) = 
        one.Ticks * (int64 two)
        |> TimeSpan

type DateTime with
    static member Create day month year =
        let d = DateTime(year,month,day)
        DateTime.SpecifyKind(d,DateTimeKind.Utc)

type FixedTemporalResolution =
| Annual
| Monthly
| Daily
| CustomTicks of int64

let (|Single|Empty|AllIdentical|Neither|) (lst:'a list) =
    if lst.Length = 1 then Single
    elif lst.Length = 0 then Empty
    elif List.forall (fun elem -> elem = lst.[0]) lst then
        AllIdentical
    else
        Neither

[<AutoOpen>]
module TimeSeries =

    type FloatingTimeSeries<'T> = private TimeSteps of array<'T * TimeSpan>
    type StartDate = DateTime
    type TimeSeries<'T> = private FixedTimeSeries of StartDate * FloatingTimeSeries<'T>

    let private unwrap (TimeSteps ts) = ts
    let private unwrapFixed (FixedTimeSeries (s,ts)) = s,ts
    let private wrap ts = TimeSteps ts
    let private floatingToFixed startDate ts = (startDate, ts) |> FixedTimeSeries
    let private ensureOrdering times = times |> Seq.sort

    // Populate a time series with an existing sequence.
    // Equal time-steps are created using the stepping specified. 
    let create (startTime:DateTime) (stepping:TimeSpan) values =
        (List.init (Seq.length values) (fun t -> stepping |> TimeSpan.Multiply t))
        |> Seq.zip values
        |> Seq.toArray
        |> TimeSteps
        |> floatingToFixed startTime

    let createVarying<'a> (ticks:(DateTime * 'a) seq) : TimeSeries<'a> =
        let startDate = ticks |> Seq.sortBy fst |> Seq.head |> fst
        let timeSpans = ticks |> Seq.sortBy fst |> Seq.map fst |> Seq.windowed 2 |> Seq.map (fun w -> w.[1].Subtract w.[0])
        timeSpans
        |> Seq.zip (ticks |> Seq.map snd)
        |> Seq.toArray
        |> TimeSteps
        |> floatingToFixed startDate

    // Map a function.
    let map (series:TimeSeries<'a>) f : TimeSeries<'b> =
        series
        |> unwrapFixed
        |> snd
        |> unwrap
        |> Array.map f
        |> wrap
        |> floatingToFixed (series |> unwrapFixed |> fst)

    let shift (by:TimeSpan) ts =
        ts 
        |> unwrapFixed
        |> snd
        |> floatingToFixed (ts |> unwrapFixed |> fst |> (fun t -> t + by))

    let fromSeq periods s =
        periods
        |> Seq.map TimeSpan.FromDays
        |> Seq.zip s
        |> Seq.toArray
        |> TimeSteps

    let toFixed start s =
        floatingToFixed start s

    let toFloating s =
        unwrapFixed s
        |> snd

    let timeSteps s =
        s
        |> unwrapFixed
        |> snd
        |> unwrap
        |> Array.map snd

    let commonTimeline (series:TimeSeries<'a> list) =
        let timeSteps = series |> List.map timeSteps
        match timeSteps with
        | Single
        | AllIdentical -> Some timeSteps.Head
        | Empty
        | Neither -> None

    let start (series:TimeSeries<'a>) =
        series
        |> unwrapFixed
        |> fst


    [<AutoOpen>]
    module TimeSeriesExtensions =

        type FloatingTimeSeries<'T> with
            member this.Values =
                this
                |> unwrap
                |> Array.map fst

        type TimeSeries<'T> with
            member this.Length =
                this
                |> unwrapFixed
                |> snd
                |> unwrap
                |> Array.length

            member this.Head =
                this
                |> unwrapFixed
                |> snd
                |> unwrap
                |> Array.head

            member this.TimeSteps =
                this |> timeSteps

            member this.Values =
                this
                |> unwrapFixed
                |> snd
                |> unwrap
                |> Array.map fst

            member this.StartDate =
                this |> start