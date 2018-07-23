module TimeTests

open Xunit
open Types
open Time
open System

[<Fact>]
let ``A time series is populated with equal-width time bins`` () =
    let data = [ 1.2<mm> ; 3.2<mm> ; 0.98<mm>; 1.27<mm> ] //Ring widths
    let startTime = DateTime(2013,01,01)
    let binWidth = TimeSpan.FromDays(365.25)
    let t = data |> TimeSeries.create startTime binWidth
    Assert.Equal (t.Length,(data.Length))
