(**
[![Script](https://acm.im/bristlecone//img/badge-script.svg)](https://acm.im/bristlecone//time-series.fsx)&emsp;
[![Notebook](https://acm.im/bristlecone//img/badge-notebook.svg)](https://acm.im/bristlecone//time-series.ipynb)

*)
#r "nuget: Bristlecone,3.0.0-beta1"
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
      Ticks (<fun:calendarDateMode@429>, <fun:calendarDateMode@429-1>)
     GetYear = <fun:calendarDateMode@430-2>
     AddYears = <fun:calendarDateMode@431-3>
     AddMonths = <fun:calendarDateMode@432-4>
     AddDays = <fun:calendarDateMode@433-5>
     AddTime = <fun:calendarDateMode@434-6>
     SubtractTime = <fun:calendarDateMode@435-7>
     Difference = <fun:calendarDateMode@436-8>
     SignedDifference = <fun:calendarDateMode@437-9>
     SortOldestFirst = <fun:calendarDateMode@449-10>
     ZeroSpan = 00:00:00
     TotalDays = <fun:calendarDateMode@439-11>
     SpanToResolution = <fun:calendarDateMode@441-12>
     Divide = <fun:calendarDateMode@448-13>
     Minus = <fun:calendarDateMode@447-14>
     EqualWithin = <fun:calendarDateMode@450-15> }, (2.1, 3/21/2020 12:00:00 AM),
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
      Ticks (<fun:radiocarbonDateMode@476>, <fun:radiocarbonDateMode@477-1>)
     GetYear = <fun:radiocarbonDateMode@479-2>
     AddYears = <fun:radiocarbonDateMode@480-3>
     AddMonths = <fun:radiocarbonDateMode@481-4>
     AddDays = <fun:radiocarbonDateMode@482-5>
     AddTime = <fun:radiocarbonDateMode@483-6>
     SubtractTime = <fun:radiocarbonDateMode@484-7>
     Difference = <fun:radiocarbonDateMode@485-8>
     SignedDifference = <fun:radiocarbonDateMode@486-9>
     SortOldestFirst = <fun:radiocarbonDateMode@487-10>
     ZeroSpan = 0.0
     TotalDays = <fun:radiocarbonDateMode@490-11>
     SpanToResolution = <fun:radiocarbonDateMode@496-12>
     Divide = <fun:radiocarbonDateMode@497-13>
     Minus = <fun:radiocarbonDateMode@498-14>
     EqualWithin = <fun:radiocarbonDateMode@499-15> },
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
               (<fun:radiocarbonCalDateMode@504>,
                <fun:radiocarbonCalDateMode@505-1>)
            GetYear = <fun:radiocarbonCalDateMode@507-2>
            AddYears = <fun:radiocarbonCalDateMode@508-3>
            AddMonths = <fun:radiocarbonCalDateMode@509-4>
            AddDays = <fun:radiocarbonCalDateMode@510-5>
            AddTime = <fun:radiocarbonCalDateMode@511-6>
            SubtractTime = <fun:radiocarbonCalDateMode@512-7>
            Difference = <fun:radiocarbonCalDateMode@513-8>
            SignedDifference = <fun:radiocarbonCalDateMode@514-9>
            SortOldestFirst = <fun:radiocarbonCalDateMode@515-10>
            ZeroSpan = 0.0
            TotalDays = <fun:radiocarbonCalDateMode@518-11>
            SpanToResolution = <fun:radiocarbonCalDateMode@521-12>
            Divide = <fun:radiocarbonCalDateMode@522-13>
            Minus = <fun:radiocarbonCalDateMode@523-14>
            EqualWithin = <fun:radiocarbonCalDateMode@524-15> },
          (5433.7, Radiocarbon 1023.0),
          TimeSteps [|(1654.2, 678.0); (982.2, 347.0)|]));
      (ShortCode "s2",
       FixedTimeSeries
         ({ Resolution =
             Ticks
               (<fun:radiocarbonCalDateMode@504>,
                <fun:radiocarbonCalDateMode@505-1>)
            GetYear = <fun:radiocarbonCalDateMode@507-2>
            AddYears = <fun:radiocarbonCalDateMode@508-3>
            AddMonths = <fun:radiocarbonCalDateMode@509-4>
            AddDays = <fun:radiocarbonCalDateMode@510-5>
            AddTime = <fun:radiocarbonCalDateMode@511-6>
            SubtractTime = <fun:radiocarbonCalDateMode@512-7>
            Difference = <fun:radiocarbonCalDateMode@513-8>
            SignedDifference = <fun:radiocarbonCalDateMode@514-9>
            SortOldestFirst = <fun:radiocarbonCalDateMode@515-10>
            ZeroSpan = 0.0
            TotalDays = <fun:radiocarbonCalDateMode@518-11>
            SpanToResolution = <fun:radiocarbonCalDateMode@521-12>
            Divide = <fun:radiocarbonCalDateMode@522-13>
            Minus = <fun:radiocarbonCalDateMode@523-14>
            EqualWithin = <fun:radiocarbonCalDateMode@524-15> },
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

