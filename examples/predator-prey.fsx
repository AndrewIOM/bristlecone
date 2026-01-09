(**
[![Script](https://acm.im/bristlecone//img/badge-script.svg)](https://acm.im/bristlecone//examples/predator-prey.fsx)&emsp;
[![Notebook](https://acm.im/bristlecone//img/badge-notebook.svg)](https://acm.im/bristlecone//examples/predator-prey.ipynb)

## Predator-Prey Dynamics: Snowshoe Hare and Lynx

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
(**
### Defining the ecological model

In Bristlecone, a single ecological model (representing a single hypothesis)
is defined through the `ModelSystem` type. A `ModelSystem` needs to include three key
components:

* **Model equations.** When working in continuous time, these are a system of Ordinary Differential Equations (ODEs).

* **Parameters to be estimated.** You must specify the starting bounds and constraints for every parameter included in the model equations.

* **Likelihood function**. The (negative log) likelihood function **-logL** represents the probability of observing the data given the parameter set. We use a negative log likelihood function, which is then minimised during optimisation.

In this example, we demonstrate using the **Lotka–Volterra** predator–prey model as the
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
### Setting up the **Bristlecone Engine**

A bristlecone engine provides a fixed setup for estimating parameters from data.
Use the same engine for all model fits within a single study.
This engine uses a gradident descent method (Nelder Mead simplex), and a basic
Runge-Kutta 4 integration method provided by MathNet Numerics.

*)
let engine =
    Bristlecone.mkContinuous ()
    |> Bristlecone.withCustomOptimisation ( Optimisation.MonteCarlo.Filzbach.filzbach
           { Optimisation.MonteCarlo.Filzbach.FilzbachSettings.Default with BurnLength = Optimisation.EndConditions.atIteration 5000<iteration> })
    |> Bristlecone.withConditioning Conditioning.NoConditioning
    |> Bristlecone.withSeed 1500 // We are setting a seed for this example - see below
    |> Bristlecone.withTimeConversion DateMode.Conversion.Annual.toYears
(**
**Note. We have set a seed for random number generation for this worked example. This ensures that the results are the same each time this documentation is generated.**

### Does it all work? Testing the engine and model

Before being confident in the ability of our estimation engine to
be able to arrive at the correct solution, we must run a full test
of the model and estimation engine.

Bristlecone includes the `Bristlecone.testModel` function, which
we use here. Given a model system and estimation engine, the function
generates a random parameter set (**θ**) many times; for each **θ**, the
'true' time-series are generated. The test result indicates the effectiveness
of the configuration at estimating **θ** given the auto-generated
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
let testResult = ``predator-prey`` |> Bristlecone.tryTestModel engine testSettings(* output: 
*)
(**
In the example using this seed, the ecological parameter set under test is:

> 1.4845 (α), 0.0377 (β), 0.0190 (γ), 0.9856 (δ) [ noise = 0.2135 (ρ), 0.0297 (σ_x), 0.0152 (σ_y) ]()
Here, the sigma values represent the **standard deviation** of the noise around hare and lynx respectively.
> 

From the test results, the key questions is whether the ecological dynamics were recovered
with the correct parameters. To find this:

* Do the trajectories match? Compare difference between predicted and observed series.

* Were parameters recovered?

```
No value returned by any evaluator
```

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
    |> Map.ofList(* output: 
map
  [(ShortCode "hare",
    FixedTimeSeries
  ({ Resolution = Year
     GetYear = <fun:annualDateMode@454>
     AddYears = <fun:annualDateMode@455-1>
     AddMonths = <fun:annualDateMode@456-2>
     AddDays = <fun:annualDateMode@457-3>
     AddTime = <fun:annualDateMode@458-4>
     SubtractTime = <fun:annualDateMode@459-5>
     Difference = <fun:annualDateMode@460-6>
     SignedDifference = <fun:annualDateMode@461-7>
     SortOldestFirst = <fun:annualDateMode@462-8>
     ZeroSpan = 0
     TotalDays = <fun:annualDateMode@465-9>
     SpanToResolution = <fun:annualDateMode@468-10>
     Divide = <fun:annualDateMode@469-11>
     Minus = <fun:annualDateMode@470-12>
     EqualWithin = <fun:annualDateMode@471-13> }, (19.58, Annual 1845),
   TimeSteps
     [|(19.6, 1); (19.61, 1); (11.99, 1); (28.04, 1); (58.0, 1); (74.6, 1);
       (75.09, 1); (88.48, 1); (61.28, 1); (74.67, 1); (88.06, 1); (68.51, 1);
       (32.19, 1); (12.64, 1); (21.49, 1); (30.35, 1); (2.18, 1); (152.65, 1);
       (148.36, 1); (85.81, 1); (41.41, 1); (14.75, 1); (2.28, 1); (5.91, 1);
       (9.95, 1); (10.44, 1); (70.64, 1); (50.12, 1); (50.13, 1); (101.25, 1);
       (97.12, 1); (86.51, 1); (72.17, 1); (38.32, 1); (10.11, 1); (7.74, 1);
       (9.67, 1); (43.12, 1); (52.21, 1); (134.85, 1); (134.86, 1); (103.79, 1);
       (46.1, 1); (15.03, 1); (24.2, 1); (41.65, 1); (52.34, 1); (53.78, 1);
       (70.4, 1); (85.81, 1); (56.69, 1); (16.59, 1); (6.16, 1); (2.3, 1);
       (12.82, 1); (4.72, 1); (4.73, 1); (37.22, 1); (69.72, 1); (57.78, 1);
       (28.68, 1); (23.37, 1); (21.54, 1); (26.34, 1); (53.1, 1); (68.48, 1);
       (75.58, 1); (57.92, 1); (40.97, 1); (24.95, 1); (12.59, 1); (4.97, 1);
       (4.5, 1); (11.21, 1); (56.6, 1); (69.63, 1); (77.74, 1); (80.53, 1);
       (73.38, 1); (36.93, 1); (4.64, 1); (2.54, 1); (1.8, 1); (2.39, 1);
       (4.23, 1); (19.52, 1); (82.11, 1); (89.76, 1); (81.66, 1); (15.76, 1)|]));
   (ShortCode "lynx",
    FixedTimeSeries
  ({ Resolution = Year
     GetYear = <fun:annualDateMode@454>
     AddYears = <fun:annualDateMode@455-1>
     AddMonths = <fun:annualDateMode@456-2>
     AddDays = <fun:annualDateMode@457-3>
     AddTime = <fun:annualDateMode@458-4>
     SubtractTime = <fun:annualDateMode@459-5>
     Difference = <fun:annualDateMode@460-6>
     SignedDifference = <fun:annualDateMode@461-7>
     SortOldestFirst = <fun:annualDateMode@462-8>
     ZeroSpan = 0
     TotalDays = <fun:annualDateMode@465-9>
     SpanToResolution = <fun:annualDateMode@468-10>
     Divide = <fun:annualDateMode@469-11>
     Minus = <fun:annualDateMode@470-12>
     EqualWithin = <fun:annualDateMode@471-13> }, (30.09, Annual 1845),
   TimeSteps
     [|(45.15, 1); (49.15, 1); (39.52, 1); (21.23, 1); (8.42, 1); (5.56, 1);
       (5.08, 1); (10.17, 1); (19.6, 1); (32.91, 1); (34.38, 1); (29.59, 1);
       (21.3, 1); (13.69, 1); (7.65, 1); (4.08, 1); (4.09, 1); (14.33, 1);
       (38.22, 1); (60.78, 1); (70.77, 1); (72.77, 1); (42.68, 1); (16.39, 1);
       (9.83, 1); (5.8, 1); (5.26, 1); (18.91, 1); (30.95, 1); (31.18, 1);
       (46.34, 1); (45.77, 1); (44.15, 1); (36.33, 1); (12.03, 1); (12.6, 1);
       (18.34, 1); (35.14, 1); (43.77, 1); (65.69, 1); (79.35, 1); (51.65, 1);
       (32.59, 1); (22.45, 1); (16.16, 1); (14.12, 1); (20.38, 1); (33.33, 1);
       (46.0, 1); (51.41, 1); (46.43, 1); (33.68, 1); (18.01, 1); (8.86, 1);
       (7.13, 1); (9.47, 1); (14.86, 1); (31.47, 1); (60.57, 1); (63.51, 1);
       (54.7, 1); (6.3, 1); (3.41, 1); (5.44, 1); (11.65, 1); (20.35, 1);
       (32.88, 1); (39.55, 1); (43.36, 1); (40.83, 1); (30.36, 1); (17.18, 1);
       (6.82, 1); (3.19, 1); (3.52, 1); (9.94, 1); (20.3, 1); (31.99, 1);
       (42.36, 1); (49.08, 1); (53.99, 1); (52.25, 1); (37.7, 1); (19.14, 1);
       (6.98, 1); (8.31, 1); (16.01, 1); (24.82, 1); (29.7, 1); (35.4, 1)|]))]*)
(**
Once the data are in Bristlecone `TimeSeries` we can run `Bristlecone.fit`, which is
the main fitting function of the Bristlecone library.

*)
let endCondition = Optimisation.EndConditions.Profiles.mcmc 5000<iteration> engine.LogTo

let result = ``predator-prey`` |> Bristlecone.tryFit engine endCondition data(* output: 
Bristlecone.Result+ResultBuilder*)
(**
### Inspecting the model fit

The `Bristlecone.fit` function returns an `EstimationResult`, which contains some
key information that may be used to inspect the model fit:

* Likelihood. The minimum likelihood identified during optimisation.

* Parameters. The parameter set (**θ**) identified at the minimum likelihood.

* Series. A TimeSeries for each variable in the model, which at each time point contains paired Modelled-Observed values.

* Trace. The likelihood and **θ** that occurred at each step in optimisation, with the latest first.

* Internal Dynamics. Not relevant for this simple model.

First, we can use the `Series` to inspect by eye the model fit versus the observed time-series:

```
No value returned by any evaluator
```

Next, we can examine the traces to see how parameter values evolved over the course of
the optimisation routine:

*)
Graphing.parameterTrace result(* output: 
No value returned by any evaluator*)
(**
For the pelt dataset, we know from other sources that the estimated
parameters should be around this range:
* Prey growth rate (α): around 0.9–1.1 per year.
* Predation rate (β): around 0.02–0.03 per predator per hare per year.
* Predator efficiency (γ): ~0.01–0.02, reflecting conversion of prey into predator growth.
* Predator death rate (δ): ~0.8–0.9 per year.

*)

