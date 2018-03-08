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

[<AutoOpen>]
module TimeSeries =

    type FloatingTimeSeries<'T> = private TimeSteps of array<'T * TimeSpan>
    type StartDate = DateTime
    type TimeSeries<'T> = private FixedTimeSeries of StartDate * FloatingTimeSeries<'T>

    let private unwrap (TimeSteps ts) = ts
    let private unwrapFixed (FixedTimeSeries (s,ts)) = s,ts
    let private wrap ts = TimeSteps ts
    let private floatingToFixed startDate ts = (startDate, ts) |> FixedTimeSeries

    // Populate a time series with an existing sequence.
    // Equal time-steps are created using the stepping specified. 
    let create (startTime:DateTime) (stepping:TimeSpan) values =
        (List.init (Seq.length values) (fun t -> stepping |> TimeSpan.Multiply t))
        |> Seq.zip values
        |> Seq.toArray
        |> TimeSteps
        |> floatingToFixed startTime

    // Map a function.
    let map (series:TimeSeries<'a>) f : TimeSeries<'a> =
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
                this
                |> unwrapFixed
                |> snd
                |> unwrap
                |> Array.map snd

            member this.Values =
                this
                |> unwrapFixed
                |> snd
                |> unwrap
                |> Array.map fst