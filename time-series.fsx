(**
[![Script](https://acm.im/bristlecone//img/badge-script.svg)](https://acm.im/bristlecone//time-series.fsx)&emsp;
[![Notebook](https://acm.im/bristlecone//img/badge-notebook.svg)](https://acm.im/bristlecone//time-series.ipynb)

*)
#r "nuget: Bristlecone,3.1.0"
(**
# Time-series and time-frames

Bristlecone includes core representations of time-series,, time-frames, and
time indexes.

A key concept is the date mode. Bristlecone supports using different modes of measuring
time, from simple `DateTime` representation (calendar time) to basic dating methods used
in long-term ecology and archaeology. Core time types require a date mode; as such, you
may implement more date modes if one required is not included.

Date mode | Maximum resolution | Time type | Timespan type
--- | --- | --- | ---
Standard .NET date-time calendars (e.g. Gregorian) | ticks | DateTime | TimeSpan
Recent time: annual data | year | integer (year) | integer (year)
Radiocarbon (calibrated) | fractional | float (cal. yr. BP) | float (cal. yr. BP)
Radiocarbon (uncalibrated) | fractional | float (BP) | float (BP)


The time-related types are included in the `Bristlecone.Time` module:

*)
open System
open Bristlecone
open Bristlecone.Time
open FSharp.Data.UnitSystems.SI.UnitNames
(**
## Time Series

A time-series is a representation of data ordered in time by the date
of observation.

### From calendar-date observations

Time-series may be created from date-time observations using built-in .NET
types and the `TimeSeries.fromNeoObservations` function:

*)
let someObservations =
    [ 2.1, DateTime(2020, 03, 21)
      5.4, DateTime(2020, 03, 22)
      -54.2, DateTime(2020, 03, 23) ]

let ts = TimeSeries.fromNeoObservations someObservations(* output: 
FixedTimeSeries
  ({ Resolution =
      Ticks (<fun:calendarDateMode@446>, <fun:calendarDateMode@446-1>)
     GetYear = <fun:calendarDateMode@447-2>
     AddYears = <fun:calendarDateMode@448-3>
     AddMonths = <fun:calendarDateMode@449-4>
     AddDays = <fun:calendarDateMode@450-5>
     AddTime = <fun:calendarDateMode@451-6>
     SubtractTime = <fun:calendarDateMode@452-7>
     Difference = <fun:calendarDateMode@453-8>
     SignedDifference = <fun:calendarDateMode@454-9>
     SortOldestFirst = <fun:calendarDateMode@466-10>
     ZeroSpan = 00:00:00
     TotalDays = <fun:calendarDateMode@456-11>
     SpanToResolution = <fun:calendarDateMode@458-12>
     Divide = <fun:calendarDateMode@465-13>
     Minus = <fun:calendarDateMode@464-14>
     EqualWithin = <fun:calendarDateMode@467-15> }, (2.1, 3/21/2020 12:00:00 AM),
   TimeSteps [|(5.4, 1.00:00:00); (-54.2, 1.00:00:00)|])*)
(**
### From radiocarbon dates

As an example of an alternative date format, uncalibrated radiocarbon dates
may be used as follows:

*)
let someDatedValues =
    [ 1654, DatingMethods.Radiocarbon 345.<``BP (radiocarbon)``>
      982, DatingMethods.Radiocarbon -2.<``BP (radiocarbon)``>
      5433, DatingMethods.Radiocarbon 1023.<``BP (radiocarbon)``> ]

let tsRadiocarbon =
    TimeSeries.fromObservations DateMode.radiocarbonDateMode someDatedValues(* output: 
FixedTimeSeries
  ({ Resolution =
      Ticks (<fun:radiocarbonDateMode@493>, <fun:radiocarbonDateMode@494-1>)
     GetYear = <fun:radiocarbonDateMode@496-2>
     AddYears = <fun:radiocarbonDateMode@497-3>
     AddMonths = <fun:radiocarbonDateMode@498-4>
     AddDays = <fun:radiocarbonDateMode@499-5>
     AddTime = <fun:radiocarbonDateMode@500-6>
     SubtractTime = <fun:radiocarbonDateMode@501-7>
     Difference = <fun:radiocarbonDateMode@502-8>
     SignedDifference = <fun:radiocarbonDateMode@503-9>
     SortOldestFirst = <fun:radiocarbonDateMode@504-10>
     ZeroSpan = 0.0
     TotalDays = <fun:radiocarbonDateMode@507-11>
     SpanToResolution = <fun:radiocarbonDateMode@513-12>
     Divide = <fun:radiocarbonDateMode@514-13>
     Minus = <fun:radiocarbonDateMode@515-14>
     EqualWithin = <fun:radiocarbonDateMode@516-15> },
   (5433, Radiocarbon 1023.0), TimeSteps [|(1654, 678.0); (982, 347.0)|])*)
(**
Alternatively, the `TimeSeries.fromRadiocarbonObservations` function may be used.
Similarly, calibrated dates can be used as follows:

*)
let someDatedValuesCal =
    [ 1654.2<kilogram>, DatingMethods.Radiocarbon 345.<``cal yr BP``>
      982.2<kilogram>, DatingMethods.Radiocarbon -2.<``cal yr BP``>
      5433.7<kilogram>, DatingMethods.Radiocarbon 1023.<``cal yr BP``> ]

let tsRadiocarbon2 =
    TimeSeries.fromObservations DateMode.radiocarbonCalDateMode someDatedValuesCal(* output: 
No value returned by any evaluator*)
(**
Radiocarbon dates are organised with larger values representing older dates. Bristlecone
handles the ordering of these time-series automatically when fitting models.

## Working with time-series

Bristlecone includes some built-in functions for working with time-series.

**Interpolation**. If you specify a time-series with a data type that is an F# option,
and the core data type is float-based, you may pass this to the
`TimeSeries.interpolateFloats` function.

## Time-frames

A time frame is a container for organising time series that occur along
a common timeline. Here, we add a second time series with points at the
same dates as above to the time series from earlier.

*)
let anotherTimeSeries =
    [ 673.2<kilogram>, DatingMethods.Radiocarbon 345.<``cal yr BP``>
      836.245<kilogram>, DatingMethods.Radiocarbon -2.<``cal yr BP``>
      2578.32<kilogram>, DatingMethods.Radiocarbon 1023.<``cal yr BP``> ]
    |> TimeSeries.fromCalibratedRadiocarbonObservations

let allTs = [
    Language.code "s" |> Option.get, tsRadiocarbon2
    Language.code "s2" |> Option.get, anotherTimeSeries ] |> Map.ofList

let timeframe = TimeFrame.tryCreate allTs(* output: 
Some
  TimeFrame
  (map
     [(ShortCode "s",
       FixedTimeSeries
         ({ Resolution =
             Ticks
               (<fun:radiocarbonCalDateMode@521>,
                <fun:radiocarbonCalDateMode@522-1>)
            GetYear = <fun:radiocarbonCalDateMode@524-2>
            AddYears = <fun:radiocarbonCalDateMode@525-3>
            AddMonths = <fun:radiocarbonCalDateMode@526-4>
            AddDays = <fun:radiocarbonCalDateMode@527-5>
            AddTime = <fun:radiocarbonCalDateMode@528-6>
            SubtractTime = <fun:radiocarbonCalDateMode@529-7>
            Difference = <fun:radiocarbonCalDateMode@530-8>
            SignedDifference = <fun:radiocarbonCalDateMode@531-9>
            SortOldestFirst = <fun:radiocarbonCalDateMode@532-10>
            ZeroSpan = 0.0
            TotalDays = <fun:radiocarbonCalDateMode@535-11>
            SpanToResolution = <fun:radiocarbonCalDateMode@538-12>
            Divide = <fun:radiocarbonCalDateMode@539-13>
            Minus = <fun:radiocarbonCalDateMode@540-14>
            EqualWithin = <fun:radiocarbonCalDateMode@541-15> },
          (5433.7, Radiocarbon 1023.0),
          TimeSteps [|(1654.2, 678.0); (982.2, 347.0)|]));
      (ShortCode "s2",
       FixedTimeSeries
         ({ Resolution =
             Ticks
               (<fun:radiocarbonCalDateMode@521>,
                <fun:radiocarbonCalDateMode@522-1>)
            GetYear = <fun:radiocarbonCalDateMode@524-2>
            AddYears = <fun:radiocarbonCalDateMode@525-3>
            AddMonths = <fun:radiocarbonCalDateMode@526-4>
            AddDays = <fun:radiocarbonCalDateMode@527-5>
            AddTime = <fun:radiocarbonCalDateMode@528-6>
            SubtractTime = <fun:radiocarbonCalDateMode@529-7>
            Difference = <fun:radiocarbonCalDateMode@530-8>
            SignedDifference = <fun:radiocarbonCalDateMode@531-9>
            SortOldestFirst = <fun:radiocarbonCalDateMode@532-10>
            ZeroSpan = 0.0
            TotalDays = <fun:radiocarbonCalDateMode@535-11>
            SpanToResolution = <fun:radiocarbonCalDateMode@538-12>
            Divide = <fun:radiocarbonCalDateMode@539-13>
            Minus = <fun:radiocarbonCalDateMode@540-14>
            EqualWithin = <fun:radiocarbonCalDateMode@541-15> },
          (2578.32, Radiocarbon 1023.0),
          TimeSteps [|(673.2, 678.0); (836.245, 347.0)|]))])*)
(**
## Time index

A time index is a way of indexing a single time series onto a common timeline in relation to
a base date (t0) and target temporal resolution. Bristlecone uses this functionality internally
to align environmental data, but you may find other uses for it.

The time index constructor requires an index mode; this specifies how values are retrieved
when the index is queried for a value that was not directly observed. The `Statistics.Interpolate`
module contains some basic interpolation methods that may be applied. The `Exact` mode may also
be used such that only directly observed values are permitted.

*)
let baseline = DatingMethods.Radiocarbon 5000.<``cal yr BP``>
let resolution () = Resolution.FixedTemporalResolution.Years (PositiveInt.create 1<year> |> Option.get)
let mode = TimeIndex.IndexMode.Interpolate Statistics.Interpolate.bilinear

// let idx = TimeIndex.TimeIndex(baseline, (fun i -> i), mode, tsRadiocarbon2)
// (*** include-value: idx ***)

