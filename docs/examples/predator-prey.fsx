(**
---
title: Predator-Prey Dynamics
category: Examples
categoryindex: 3
index: 1
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
model system. For the -logL function we use a bivariate normal negative log likelihood function.
This -logL function assumes normally-distributed observation error around each observation
at each time-point, for both the lynx and hare data. The -logL function contains
three parameters that are to be estimated alongside the deterministic model: the variability
in lynx data, the variability in hare data, and their covariance.
*)

[<Measure>] type prey
[<Measure>] type predator
[<Measure>] type km
[<Measure>] type area = km^2

let ``predator-prey`` =

    // States
    let H = state<prey / area> "hare"
    let L = state<predator / area> "lynx"

    // Parameters
    let α = parameter "α" notNegative 0.5< / year> 1.5< / year> // Maximum prey per capita growth rate
    let β = parameter "β" notNegative 0.01<1 / (predator / area * year)> 0.05<1 / (predator / area * year)> // Predation rate
    let δ = parameter "δ" notNegative 0.5< / year> 1.0< / year> // Natural death rate of lynx in the absence of food
    let γ = parameter "γ" notNegative 0.01<1 / (prey / area * year)> 0.1<1 / (prey / area * year)> // Predator growth efficiency

    let ``dH/dt``: ModelExpression<(prey / area) / year> =
        P α * This<prey / area> - P β * This<prey / area> * State L

    let ``dL/dt``: ModelExpression<(predator / area) / year> =
        P γ * State H * This<predator / area> - P δ * This<predator / area>

    Model.empty
    |> Model.addRateEquation H ``dH/dt``
    |> Model.addRateEquation L ``dL/dt``
    |> Model.estimateParameter α
    |> Model.estimateParameter β
    |> Model.estimateParameter δ
    |> Model.estimateParameter γ
    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.bivariateGaussian (Require.state H) (Require.state L))
    |> Model.estimateParameterOld "ρ" noConstraints -0.500 0.500
    |> Model.estimateParameterOld "σ[x]" notNegative 0.001 0.100
    |> Model.estimateParameterOld "σ[y]" notNegative 0.001 0.100
    |> Model.compile


(**
### Setting up the *Bristlecone Engine*

A bristlecone engine provides a fixed setup for estimating parameters from data.
Use the same engine for all model fits within a single study.
This engine uses a gradident descent method (Nelder Mead simplex), and a basic
Runge-Kutta 4 integration method provided by MathNet Numerics.
*)

let engine: EstimationEngine.EstimationEngine<int<year>,year,1> =
    Bristlecone.mkContinuous ()
    |> Bristlecone.withCustomOptimisation ( Optimisation.MonteCarlo.Filzbach.filzbach
           { Optimisation.MonteCarlo.Filzbach.FilzbachSettings.Default with BurnLength = Optimisation.EndConditions.atIteration 5000<iteration> })
    |> Bristlecone.withConditioning Conditioning.NoConditioning
    |> Bristlecone.withSeed 1500 // We are setting a seed for this example - see below
    |> Bristlecone.withTimeConversion (fun (ts: int<Time.year>) -> float ts * 1.<Time.year>)

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
    Test.annualSettings
    |> Test.addStartValues [ "hare", 50.; "lynx", 75. ]
    |> Test.addNoise (Test.Noise.tryAddNormal "σ[y]" "lynx")
    |> Test.addNoise (Test.Noise.tryAddNormal "σ[x]" "hare")
    |> Test.addGenerationRules
        [ Test.GenerationRules.alwaysLessThan 100000. "lynx"
          Test.GenerationRules.alwaysMoreThan 10. "lynx"
          Test.GenerationRules.alwaysLessThan 100000. "hare"
          Test.GenerationRules.alwaysMoreThan 10. "hare" ]
    |> Test.withTimeSeriesLength 30
    |> Test.endWhen (Optimisation.EndConditions.Profiles.mcmc 5000<iteration> ignore)

(**
In our `TestSettings`, we have specified the initial time point (t = 0)
for both modelled time-series. We have also added noise around
each generated time-series, and specified that each time-series
should be 30 years in length.

With these test settings, we can now run the test.
*)

let testResult = ``predator-prey`` |> Bristlecone.tryTestModel engine testSettings
(*** include-output ***)

(**

In the example using this seed, the ecological parameter set under test is:
> 1.4845 (α), 0.0377 (β), 0.0190 (γ), 0.9856 (δ) [ noise = 0.2135 (ρ), 0.0297 (σ_x), 0.0152 (σ_y) ]
Here, the sigma values represent the *standard deviation* of the noise around hare and lynx respectively.

From the test results, the key questions is whether the ecological dynamics were recovered
with the correct parameters. To find this:

* Do the trajectories match? Compare difference between predicted and observed series.
* Were parameters recovered?
     
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
            |> fun x ->
                printfn "%s" x
                x
        | Error e -> sprintf "Cannot display data, as model fit did not run successfully (%s)" e

    let pairedFitsForTestResult (testResult: Result<Bristlecone.Test.TestResult<DatingMethods.Annual,int<year>,int<year>,'u>, string>) =
        match testResult with
        | Ok r -> pairedFits r.Series
        | Error e -> sprintf "Cannot display data, as model fit did not run successfully (%s)" e

    let pairedFitsForResult (testResult: Result<Bristlecone.ModelSystem.EstimationResult<_, _, _>, string>) =
        match testResult with
        | Ok r -> pairedFits (r.Series |> Seq.map (fun kv -> kv.Key.Value, kv.Value) |> Map.ofSeq)
        | Error e -> sprintf "Cannot display data, as model fit did not run successfully (%s)" e

    let parameterTrace (result: Result<ModelSystem.EstimationResult<_, _, _>, 'b>) =
        match result with
        | Ok r ->
            r.Trace
            |> Seq.map snd
            |> Seq.map Seq.toList
            |> Seq.toList
            |> List.flip
            |> List.map (fun values -> Chart.Line(y = values, x = [ 1 .. values.Length ]))
            |> Chart.Grid(3, 3)
            |> GenericChart.toChartHTML
        | Error _ -> "Model did not fit successfully"



(*** hide ***)
Graphing.pairedFitsForTestResult testResult
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

let endCondition = Optimisation.EndConditions.Profiles.mcmc 5000<iteration> engine.LogTo

let result = ``predator-prey`` |> Bristlecone.tryFit engine endCondition data

(*** include-value: result ***)

(**
### Inspecting the model fit

The `Bristlecone.fit` function returns an `EstimationResult`, which contains some
key information that may be used to inspect the model fit:

* Likelihood. The minimum likelihood identified during optimisation.
* Parameters. The parameter set (*θ*) identified at the minimum likelihood.
* Series. A TimeSeries for each variable in the model, which at each time point contains paired Modelled-Observed values.
* Trace. The likelihood and *θ* that occurred at each step in optimisation, with the latest first.
* Internal Dynamics. Not relevant for this simple model.

First, we can use the `Series` to inspect by eye the model fit versus the observed time-series:
*)

(*** hide ***)
Graphing.pairedFitsForResult result
(*** include-it-raw ***)

(**

Next, we can examine the traces to see how parameter values evolved over the course of
the optimisation routine:
*)

Graphing.parameterTrace result
(*** include-it-raw ***)

(**

For the pelt dataset, we know from other sources that the estimated
parameters should be around this range:
* Prey growth rate (α): around 0.9–1.1 per year.
* Predation rate (β): around 0.02–0.03 per predator per hare per year.
* Predator efficiency (γ): ~0.01–0.02, reflecting conversion of prey into predator growth.
* Predator death rate (δ): ~0.8–0.9 per year.



*)