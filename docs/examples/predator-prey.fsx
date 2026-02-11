(**
---
title: Predator-Prey Dynamics
category: Examples
categoryindex: 3
index: 2
---

[![Script]({{root}}/img/badge-script.svg)]({{root}}/{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}/img/badge-notebook.svg)]({{root}}/{{fsdocs-source-basename}}.ipynb)
*)

(*** condition: prepare ***)
#nowarn "211"
#r "nuget: DiffSharp-cpu, v=1.0.7"
#r "nuget: MathNet.Numerics.FSharp,5.0.0"
#r "nuget: FSharp.Data,6.6"
#r "../../src/Bristlecone/bin/Debug/net10.0/Bristlecone.dll"

#r "nuget: Plotly.NET, 4.2.0"

(**
Predator-Prey Dynamics: Snowshoe Hare and Lynx
---

Here we use the classic example of snowshoe hare and lynx predator-prey dynamics, 
to demonstrate the basic functions of Bristlecone. The dataset is a 90-year 
time-series of snowshoe hare and lynx pelts purchased by the 
Hudson's Bay Company of Canada. Data is in 1000s. 

To get started, we first load and open the Bristlecone library in
an F# script file (.fsx):
*)

open Bristlecone // Opens Bristlecone core library and estimation engine
open Bristlecone.Language // Open the language for writing Bristlecone models
open Bristlecone.Time

(**### Defining the ecological model

In Bristlecone, a single ecological model (representing a single hypothesis)
is defined through the `ModelSystem` type. A `ModelSystem` needs to include three key
components:

* **Model equations.** When working in continuous time, these are a system of Ordinary Differential Equations (ODEs).
* **Parameters to be estimated.** You must specify the starting bounds and constraints for every parameter included in the model equations.
* **Likelihood function**. The (negative log) likelihood function *-logL* represents the probability of observing the data given the parameter set. We use a negative log likelihood function, which is then minimised during optimisation.

In this example, we demonstrate using the *Lotka–Volterra* predator–prey model as the
model system. For the -logL function we assume log-normal observation error for each
of the two states, as we treat hare and lynx as dimensionless counts and assume multiplicative
observation error; we model this with a lognormal likelihood. This is appropriate here because
the fur‑return counts are strictly positive and vary over orders of magnitude. Any parameters
passed into -logL functions are automatically estimated alongside model parameters;
in this case these are sigma-hare and sigma-lynx.

*Note. If we instead modelled density (e.g. individuals per km²), we would typically use a
Gaussian or Student‑t likelihood on the raw scale, because taking logs of dimensional quantities
is not physically meaningful.*
*)

[<Measure>] type prey = 1 // dimensionless counts
[<Measure>] type predator = 1 // dimensionless counts

// States
let H = state<prey> "hare"
let L = state<predator> "lynx"

// Likelihood parameters
let σH = parameter "σ[H]" Positive 0.001 0.100
let σL = parameter "σ[L]" Positive 0.001 0.100


let ``predator-prey`` =

    // Parameters
    let α = parameter "α" Positive 0.5<1/year> 1.5<1/year> // Maximum prey per‑capita growth rate
    let β = parameter "β" Positive 0.01<1/(predator * year)> 0.05<1/(predator * year)> // Predation rate coefficient
    let γ = parameter "γ" Positive 0.1<1/year> 1.0<1/year> // Predator natural mortality rate
    let δ = parameter "δ" Positive 0.01<1/(prey * year)> 0.1<1/(prey * year)> // Predator growth efficiency

    let ``dH/dt``: ModelExpression<prey / year> =
        (P α - P β * State L) * This<prey>

    let ``dL/dt``: ModelExpression<predator / year> =
        (-P γ + P δ * State H) * This<predator>

    // Likelihood
    let NLL =
        ModelLibrary.NegLogLikelihood.LogNormal (Require.state H) σH +
        ModelLibrary.NegLogLikelihood.LogNormal (Require.state L) σL

    Model.empty
    |> Model.addRateEquation H ``dH/dt``
    |> Model.addRateEquation L ``dL/dt``
    |> Model.estimateParameter α
    |> Model.estimateParameter β
    |> Model.estimateParameter δ
    |> Model.estimateParameter γ
    |> Model.useLikelihoodFunction NLL
    |> Model.compile


(**
### Setting up the *Bristlecone Engine*

A bristlecone engine provides a fixed setup for estimating parameters from data.
Use the same engine for all model fits within a single study.
This engine uses a gradident descent method (Nelder Mead simplex), and a basic
Runge-Kutta 4 integration method provided by MathNet Numerics.
*)

let engine: EstimationEngine.EstimationEngine<DatingMethods.Annual,int<year>,year,1> =
    Bristlecone.mkContinuous ()
    |> Bristlecone.withBristleconeOptimiser
    |> Bristlecone.withConditioning Conditioning.NoConditioning
    |> Bristlecone.withSeed 1500 // We are setting a seed for this example - see below
    |> Bristlecone.withTimeConversion DateMode.Conversion.Annual.toYears

(**
*Note. We have set a seed for random number generation for this worked example. This ensures that the results are the same each time this documentation is generated.*

### Does it all work? Testing the engine and model

Before being confident in the ability of our estimation engine to
be able to arrive at the correct solution, we must run a full test
of the model and estimation engine.

Bristlecone includes the ``Bristlecone.testModel`` function, which
we use here. Given a model system and estimation engine, the function
generates a random parameter set (*θ*) many times; for each *θ*, the
'true' time-series are generated. The test result indicates the effectiveness
of the configuration at estimating *θ* given the auto-generated
time-series. If there is divergence, there is likely an
issue with your model or the Bristlecone configuration.

Bristlecone includes many settings to configure the test
procedure. A simple test configuration is set as `Test.defaultSettings`,
but here we will configure some additional settings:
*)

let testSettings =
    Test.createWithTimeMode DateMode.annualDateMode (Yearly 1<year>) (DatingMethods.Annual 1970<year>)
    |> Test.seriesLength 15
    |> Test.t1 (Require.state H) 50.
    |> Test.t1 (Require.state L) 75.
    |> Test.withObservationError (Require.state H) (Test.Error.logNormal σH)
    |> Test.withObservationError (Require.state L) (Test.Error.logNormal σL)
    |> Test.rule (Require.state H) (Test.GenerationRules.between 10. 10000.)
    |> Test.rule (Require.state L) (Test.GenerationRules.between 10. 10000.)

(**
In our `TestSettings`, we have specified the initial time point (t = 0)
for both modelled time-series. We have also added noise around
each generated time-series, and specified that each time-series
should be 30 years in length.

With these test settings, we can now run the test.
*)

let endCondition = Optimisation.EndConditions.Profiles.mcmc 5000<iteration> engine.LogTo

(*** do-not-eval ***)
let testResult = ``predator-prey`` |> Bristlecone.tryTestModel engine endCondition testSettings

(forceOk testResult).Parameters

(**
In the example using this seed, the ecological parameter set under test is:
> 1.1070 (α), 0.03299 (β), 0.7930 (γ), 0.01487 (δ) [ error: 0.0706 (σH), 0.0868 (σL) ]
Here, the sigma values represent the *standard deviation* of the noise around hare and lynx respectively.

From the test results, the key questions is whether the ecological dynamics were recovered.
To identify this we can look at these factors:

* Was the estimated -logL near the true -logL?
* Were all of the true parameters recovered (within a tolerance)?
* Do the estimated and observed trajectories look similar?
* Are there trends in the error structure over time?

First, the true -logL was 65.8821, while the estimated minimum was
66.5655, which are close given that observation error was added.

Second, the parameters were recovered to reasonable levels, again
given that observation error was added to a short 15-point time-series:

```fsharp
val it: Test.ParameterTestResult list =
  [{ Identifier = "α"
     RealValue = 1.149500728
     EstimatedValue = 1.107033385 }; { Identifier = "β"
                                       RealValue = 0.0329926312
                                       EstimatedValue = 0.03244952328 };
   { Identifier = "γ"
     RealValue = 0.7930286527
     EstimatedValue = 0.8282329879 }; { Identifier = "δ"
                                        RealValue = 0.0148735689
                                        EstimatedValue = 0.0160377167 };
   { Identifier = "σ[H]"
     RealValue = 0.07067647576
     EstimatedValue = 0.05908173058 }; { Identifier = "σ[L]"
                                         RealValue = 0.08681011945
                                         EstimatedValue = 0.09076999054 }]
```
*)

(*** hide ***)
module Graphing =

    open Plotly.NET

    let pairedFits (series: Map<string, ModelSystem.FitSeries<DatingMethods.Annual,int<year>,int<year>>>) =
        match testResult with
        | Ok r ->
            series
            |> Seq.map (fun kv ->
                let lines =
                    kv.Value
                    |> Bristlecone.Time.TimeSeries.toObservations
                    |> Seq.collect (fun (d, v) -> [ v, "Modelled", d.Fit; v, "Observed", d.Obs ])
                    |> Seq.groupBy (fun (_, x, _) -> x)
                    |> Seq.map (fun (_, s) -> s |> Seq.map (fun (x, _, y) -> x.Value, y))
                    |> Seq.toList
                // Each chart has the modelled and observed series
                Chart.combine
                    [ Chart.Line(xy = lines.[0], Name = "Modelled")
                      Chart.Line(xy = lines.[1], Name = "Observed") ]
                |> Chart.withTitle kv.Key)
            |> Chart.Grid(2, 1)
            |> fun x ->
                printfn "%A" x
                x
            |> GenericChart.toChartHTML
        | Error e -> sprintf "Cannot display data, as model fit did not run successfully (%s)" e

    let pairedFitsForTestResult (testResult: Result<Bristlecone.Test.TestResult<DatingMethods.Annual,int<year>,int<year>,'u>, string>) =
        match testResult with
        | Ok r -> pairedFits r.Series
        | Error e -> sprintf "Cannot display data, as model fit did not run successfully (%s)" e

    let pairedFitsForResult (testResult: Result<Bristlecone.ModelSystem.EstimationResult<_, _, _>, string>) =
        match testResult with
        | Ok r -> pairedFits (r.Series |> Seq.map (fun kv -> kv.Key.Value, kv.Value) |> Map.ofSeq)
        | Error e -> sprintf "Cannot display data, as model fit did not run successfully (%s)" e

    let parameterTrace (trace: list<ModelSystem.Trace>) =
        trace
        |> Seq.collect(fun r -> r.Results |> Seq.map snd)
        |> Seq.map Seq.toList
        |> Seq.toList
        |> List.flip
        |> List.map (fun values -> Chart.Line(y = values, x = [ 1 .. values.Length ]))
        |> Chart.Grid(3, 3)
        |> Chart.withSize(800,800)
        |> GenericChart.toChartHTML

    let optimisationPaths paramTraces =
        paramTraces
        |> List.allPairs paramTraces
        |> List.map(fun ((p1,pv1),(p2,pv2)) ->
            let min1, max1 = pv1 |> Seq.min, pv1 |> Seq.max
            let min2, max2 = pv2 |> Seq.min, pv2 |> Seq.max
            let rescaled1 = pv1 |> Seq.map(fun v -> (v - min1) / max1)
            let rescaled2 = pv2 |> Seq.map(fun v -> (v - min2) / max2)

            Chart.Line(x = rescaled1, y = rescaled2, Name = sprintf "%s ~ %s" p1 p2, LineWidth = 0.6)
            |> Chart.withXAxisStyle p1
            |> Chart.withYAxisStyle p2
            )
        |> Chart.Grid(nRows = 6, nCols = 6, Pattern = StyleParam.LayoutGridPattern.Coupled)
        |> Chart.withSize(800, 800)
        |> Chart.withTemplate ChartTemplates.lightMirrored
        |> GenericChart.toChartHTML

    let optimPathsFromTestResult (testResult:Result<Test.TestResult<DatingMethods.Annual,int<year>,int<year>,ModelSystem.state>,string>) =
        let paramNames = (testResult |> forceOk).Parameters |> List.map(fun p -> p.Identifier)
        let paramTraces =
            paramNames
            |> List.mapi(fun pi p ->
                p,
                (testResult |> forceOk).Trace
                |> List.collect(fun trace -> (trace.Results |> List.map(fun i -> (snd i).[pi])))
                |> List.chunkBySize 50
                |> List.map(fun c -> c.Head)
            )
        optimisationPaths paramTraces

(**
Third, we can look at the predicted versus observed time-series:
*)

(*** hide ***)
// Graphing.pairedFitsForTestResult testResult
System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/cached/pred-prey-test-fit.txt")
(*** include-it-raw ***)

(**
The fit is relatively tight. The test result type also includes
`.ErrorStructure`, which shows the per-observation root squared
error in the units state^2, where state here is the count of hare
/ lynx.

We can also examine raw parameter traces to visualise the contours
of the parameter space. Note that the traces are
organised from newest to oldest. The `.Trace` is organised
into a list, where each item is a stage or sub-stage of the
optimisation method applied. Here, this includes first a pre-tuning phase,
then a tuning phase, each heating level, each annealing level,
and finally a 'clean' MCMC chain.
*)

(*** hide ***)
// Graphing.parameterTrace (testResult |> forceOk).Trace
System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/cached/pred-prey-test-param-trace.txt")
(*** include-it-raw ***)

(**
You can see from the traces that - once tuning had occured - the
heating within the fast simulated annealing part allowed the parameter
space to be broadly explored, before settling down to a solution during
the annealing phase.
*)

(*** hide ***)
// Graphing.optimPathsFromTestResult testResult
System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/cached/pred-prey-test-walk.txt")
(*** include-it-raw ***)

(**
### Fitting to real data

First, we must load in the real data, which is in a CSV file. Here, we will use
the FSharp.Data type provider to read in the CSV file (see [the FSharp.Data docs](http://fsprojects.github.io/FSharp.Data/library/CsvProvider.html)
for further information on how to use the library). We place the raw data into
a Bristlecone `TimeSeries` type using `TimeSeries.fromObservations`:
*)

[<Literal>]
let ResolutionFolder = __SOURCE_DIRECTORY__

type PopulationData = FSharp.Data.CsvProvider<"data/lynx-hare.csv", ResolutionFolder=ResolutionFolder>

let data =
    let csv = PopulationData.Load(__SOURCE_DIRECTORY__ + "/data/lynx-hare.csv")

    [ (code "hare").Value, TimeSeries.fromObservations DateMode.annualDateMode (csv.Rows |> Seq.map (fun r -> float r.Hare, r.Year * 1<year> |> DatingMethods.Annual))
      (code "lynx").Value, TimeSeries.fromObservations DateMode.annualDateMode (csv.Rows |> Seq.map (fun r -> float r.Lynx, r.Year * 1<year> |> DatingMethods.Annual)) ]
    |> Map.ofList

(*** include-value: data ***)

(**
Once the data are in Bristlecone `TimeSeries` we can run `Bristlecone.fit`, which is
the main fitting function of the Bristlecone library.
*)

(*** do-not-eval ***)
let result = ``predator-prey`` |> Bristlecone.tryFit engine endCondition data

(**
### Inspecting the model fit

The `Bristlecone.fit` function returns an `EstimationResult`, which contains some
key information that may be used to inspect the model fit:

* Likelihood. The minimum likelihood identified during optimisation.
* Parameters. The parameter set (*θ*) identified at the minimum likelihood.
* Series. A TimeSeries for each variable in the model, which at each time point contains paired Modelled-Observed values.
* Trace. The likelihood and *θ* that occurred at each step in optimisation, with the latest first.

First, we can use the `Series` to inspect by eye the model fit versus the observed time-series:
*)

(*** hide ***)
// Graphing.pairedFitsForResult result
(*** include-it-raw ***)

(**

Next, we can examine the traces to see how parameter values evolved over the course of
the optimisation routine:
*)

// Graphing.parameterTrace (forceOk result).Trace
(*** include-it-raw ***)

(**

For the pelt dataset, we know from other sources that the estimated
parameters should be around this range:
* Prey growth rate (α): around 0.9–1.1 per year.
* Predation rate (β): around 0.02–0.03 per predator per hare per year.
* Predator death rate (γ): ~0.8-0.9 per year.
* Predator growth efficiency (δ): ~0.01–0.02, reflecting conversion of prey into predator growth.

*)