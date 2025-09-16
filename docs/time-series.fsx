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
#r "../src/Bristlecone/bin/Debug/net5.0/Bristlecone.dll"
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
time, from simple `DateTime` representation (calendar time) to various dating methods
commonly used in long-term ecology and archaeology.

The time-related types are included in the `Bristlecone.Time` module:
*)

open System
open Bristlecone
open Bristlecone.Time

(***
## Time Series

A time-series is a representation of data ordered in time by the date
of observation.

### From calendar-date observations

Time-series may be created from date-time observations using built-in .NET
types and the `TimeSeries.fromNeoObservations` function:
*)

let someObservations = [
    2.1, DateTime(2020, 03, 21)
    5.4, DateTime(2020, 03, 22)
    -54.2, DateTime(2020, 03, 23)
]

let ts = TimeSeries.fromNeoObservations someObservations
(*** include-value: ts ***)

(**
### From radiocarbon dates

As an example of an alternative date format, uncalibrated radiocarbon dates 
may be used as follows:
*)

let someDatedValues = [
    1654, DatingMethods.Radiocarbon 345<``BP (radiocarbon)``>
    982, DatingMethods.Radiocarbon -2<``BP (radiocarbon)``>
    5433, DatingMethods.Radiocarbon 1023<``BP (radiocarbon)``>
]

let tsRadiocarbon = TimeSeries.fromObservations DateMode.radiocarbonDateMode someDatedValues
(*** include-value: tsRadiocarbon ***)
