(**
---
title: Time-Series and Time-Frames
category: Components
categoryindex: 4
index: 1
---

[![Script]({{root}}/img/badge-script.svg)]({{root}}/{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}/img/badge-notebook.svg)]({{root}}/{{fsdocs-source-basename}}.ipynb)
*)

(*** condition: prepare ***)
#nowarn "211"
#r "../src/Bristlecone/bin/Debug/net10.0/Bristlecone.dll"
(*** condition: fsx ***)
#if FSX
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // FSX
(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // IPYNB

(**
Time-series and time-frames
========================

Bristlecone includes core representations of time-series,, time-frames, and
time indexes. 

A key concept is the date mode. Bristlecone supports using different modes of measuring
time, from simple `DateTime` representation (calendar time) to basic dating methods used
in long-term ecology and archaeology. Core time types require a date mode; as such, you
may implement more date modes if one required is not included.

| Date mode | Maximum resolution | Time type | Timespan type |
| --- | --- | --- | --- |
| Standard .NET date-time calendars (e.g. Gregorian) | ticks | DateTime | TimeSpan |
| Recent time: annual data | year | integer (year) | integer (year) |
| Radiocarbon (calibrated) | fractional | float (cal. yr. BP) | float (cal. yr. BP) |
| Radiocarbon (uncalibrated) | fractional | float (BP) | float (BP) |


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

let ts = TimeSeries.fromNeoObservations someObservations
(*** include-value: ts ***)

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
    TimeSeries.fromObservations DateMode.radiocarbonDateMode someDatedValues
(*** include-value: tsRadiocarbon ***)

(**

Alternatively, the `TimeSeries.fromRadiocarbonObservations` function may be used.
Similarly, calibrated dates can be used as follows:
*)

let someDatedValuesCal =
    [ 1654.2<kilogram>, DatingMethods.Radiocarbon 345.<``cal yr BP``>
      982.2<kilogram>, DatingMethods.Radiocarbon -2.<``cal yr BP``>
      5433.7<kilogram>, DatingMethods.Radiocarbon 1023.<``cal yr BP``> ]

let tsRadiocarbon2 =
    TimeSeries.fromObservations DateMode.radiocarbonCalDateMode someDatedValuesCal
(*** include-value: tsRadiocarbonCal ***)

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

let timeframe = TimeFrame.tryCreate allTs
(*** include-value: timeframe ***)

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


let idx = TimeIndex.TimeIndex(baseline, resolution (), mode, tsRadiocarbon2)
(*** include-value: idx ***)
