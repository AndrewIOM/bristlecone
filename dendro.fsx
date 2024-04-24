(**
[![Script](https://acm.im/bristlecone//img/badge-script.svg)](https://acm.im/bristlecone//dendro.fsx)&emsp;
[![Notebook](https://acm.im/bristlecone//img/badge-notebook.svg)](https://acm.im/bristlecone//dendro.ipynb)

*)
#r "nuget: Bristlecone,2.0.0"
(**
## Bristlecone as a tool in dendroecology

Bristlecone has been developed and used for working with
long-term ecological time-series, which includes wood ring
proxy records.

The `Bristlecone.Dendro` package contains additional functions
that can be usesful when conducting top-down mechanistic modelling
with tree- and shrub-ring data, such as ring width and stable isotope
records. You can reference it in an F# script with `#r "nuget: Bristlecone.Dendro"`.

### Seasonal Cycles and Day Length

In high and mid-latitudes, seasonal cycles are the dominant control
on plant productivity. In Bristlecone, we can represent seasonal variability
in day length and environmental conditions - such as soil temperature
and moisture - within model systems using some features of `Bristlecone.Dendro`.

#### Sunrise, Sunset and Hours of Light

To calculate whether there is total sun or darkness, or partial light, at any
location on any day of the year, we can use the `Sunrise.calculate` function.

*)
open Bristlecone.Dendro

let latitude = 54.2
let longitude = -4.2

Sunrise.calculate 2024 04 20 latitude longitude "Europe/London"(* output: 
PartialLight (4/20/2024 6:02:19 AM +01:00, 4/20/2024 8:28:53 PM +01:00)*)
(**
As an illustration, if we looked at Tromsø in January we would see far less available light:

*)
Sunrise.calculate 2024 01 10 69.64961 18.95702 "Europe/London"(* output: 
CompleteDark*)

