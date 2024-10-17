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
#r "nuget: MathNet.Numerics.FSharp,5.0.0"
#r "nuget: FSharp.Data,6.3"
#r "../../src/Bristlecone/bin/Debug/netstandard2.0/Bristlecone.dll"

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

let ``predator-prey`` =

    let ``dh/dt`` = Parameter "α" * This - Parameter "β" * This * Environment "lynx"
    let ``dl/dt`` = -Parameter "δ" * This + Parameter "γ" * Environment "hare" * This

    Model.empty
    |> Model.addEquation "hare" ``dh/dt``
    |> Model.addEquation "lynx" ``dl/dt``

    |> Model.estimateParameter "α" noConstraints 0.75 1.25 // Natural growth rate of hares in absence of predation
    |> Model.estimateParameter "β" noConstraints 0.01 0.20 // Death rate per encounter of hares due to predation
    |> Model.estimateParameter "δ" noConstraints 0.75 1.25 // Natural death rate of lynx in the absence of food
    |> Model.estimateParameter "γ" noConstraints 0.01 0.20 // Efficiency of turning predated hares into lynx

    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.bivariateGaussian "hare" "lynx")
    |> Model.estimateParameter "ρ" noConstraints -0.500 0.500
    |> Model.estimateParameter "σ[x]" notNegative 0.001 0.100
    |> Model.estimateParameter "σ[y]" notNegative 0.001 0.100

    |> Model.compile

(**
### Setting up the *Bristlecone Engine*

A bristlecone engine provides a fixed setup for estimating parameters from data.
Use the same engine for all model fits within a single study.
This engine uses a gradident descent method (Nelder Mead simplex), and a basic
Runge-Kutta 4 integration method provided by MathNet Numerics.
*)

let engine =
    Bristlecone.mkContinuous
    // |> Bristlecone.withCustomOptimisation (Optimisation.Amoeba.swarm 5 20 Optimisation.Amoeba.Solver.Default)
    |> Bristlecone.withCustomOptimisation (
        Optimisation.MonteCarlo.Filzbach.filzbach
            { Optimisation.MonteCarlo.Filzbach.FilzbachSettings<float>.Default with
                BurnLength = Optimisation.EndConditions.afterIteration 10000 }
    )
    |> Bristlecone.withContinuousTime Integration.MathNet.integrate
    |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint
    |> Bristlecone.withSeed 1000 // We are setting a seed for this example - see below

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
    Test.create
    |> Test.addStartValues [ "hare", 50.; "lynx", 75. ]
    |> Test.addNoise (Test.Noise.tryAddNormal "σ[y]" "lynx")
    |> Test.addNoise (Test.Noise.tryAddNormal "σ[x]" "hare")
    |> Test.addGenerationRules
        [ Test.GenerationRules.alwaysLessThan 100000. "lynx"
          Test.GenerationRules.alwaysMoreThan 10. "lynx"
          Test.GenerationRules.alwaysLessThan 100000. "hare"
          Test.GenerationRules.alwaysMoreThan 10. "hare" ]
    |> Test.withTimeSeriesLength 30
    |> Test.endWhen (Optimisation.EndConditions.afterIteration 100)

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
We can plot the test results to check the fit.
*)

(*** hide ***)
module Graphing =

    open Plotly.NET

    let pairedFits (series: Map<string, ModelSystem.FitSeries<_,_,_>>) =
        match testResult with
        | Ok r ->
            series
            |> Seq.map (fun kv ->
                let lines =
                    kv.Value
                    |> Bristlecone.Time.TimeSeries.toObservations
                    |> Seq.collect (fun (d, v) -> [ v, "Modelled", d.Fit; v, "Observed", d.Obs ])
                    |> Seq.groupBy (fun (_, x, _) -> x)
                    |> Seq.map (fun (_, s) -> s |> Seq.map (fun (x, _, y) -> x, y))
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

    let pairedFitsForTestResult (testResult: Result<Bristlecone.Test.TestResult<_,_,_>, string>) =
        match testResult with
        | Ok r -> pairedFits r.Series
        | Error e -> sprintf "Cannot display data, as model fit did not run successfully (%s)" e

    let pairedFitsForResult (testResult: Result<Bristlecone.ModelSystem.EstimationResult<_,_,_>, string>) =
        match testResult with
        | Ok r -> pairedFits (r.Series |> Seq.map (fun kv -> kv.Key.Value, kv.Value) |> Map.ofSeq)
        | Error e -> sprintf "Cannot display data, as model fit did not run successfully (%s)" e

    let parameterTrace (result: Result<ModelSystem.EstimationResult<_,_,_>, 'b>) =
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

    [ (code "hare").Value, Time.TimeSeries.fromNeoObservations (csv.Rows |> Seq.map (fun r -> float r.Hare, r.Year))
      (code "lynx").Value, Time.TimeSeries.fromNeoObservations (csv.Rows |> Seq.map (fun r -> float r.Lynx, r.Year)) ]
    |> Map.ofList

(*** include-value: data ***)

(**
Once the data are in Bristlecone `TimeSeries` we can run `Bristlecone.fit`, which is
the main fitting function of the Bristlecone library.
*)

let endCondition = Optimisation.EndConditions.afterIteration 10000

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

*NB: this documentation is auto-generated so we cannot comment directly on the randomly generated scenario.*

Next, we can examine the traces to see how parameter values evolved over the course of
the optimisation routine:
*)

Graphing.parameterTrace result
(*** include-it-raw ***)
