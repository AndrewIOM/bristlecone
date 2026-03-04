(**
---
title: Model Fitting
category: Techniques
categoryindex: 1
index: 2
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
Model Fitting
========================

Model fitting in Bristlecone is the process of estimating model parameters from data using a chosen estimation engine.  
This page provides a high‑level overview of the workflow, the objects involved, and how to interpret the results.

Bristlecone fits require three components whose types must align:

* **Data** — a time‑series dataset with typed dates and observations.
* **Model** — a Bristlecone model system describing the process.
* **Estimation engine** — a configuration describing how fitting should proceed (time mode, optimiser, conditioning, etc.).

Once these are defined, fitting is performed with `Bristlecone.fit` or `Bristlecone.tryFit`.

Before undertaking fits to real data, Bristlecone also provides a *model testing* framework to check that the model and engine can recover parameters from synthetic data. Although testing is optional, it is strongly recommended to ensure that the model is identifiable in the given model fitting method.

### Testing a model and engine (recommended)

Before relying on a model–engine combination, Bristlecone provides a built‑in testing framework:
*)

open Bristlecone

Bristlecone.testModel
Bristlecone.tryTestModel

(**
Testing works by generating random parameter sets (θ), simulating synthetic time‑series from the model, attempting to re‑estimate θ from the synthetic data, and finally comparing the recovered parameters to the true ones.

Examination of the test result will indicate:

* whether the model is identifiable,
* whether the engine can recover parameters,
* whether the optimiser is stable, and
* whether the likelihood surface is well‑behaved.

If the recovered parameters diverge strongly from the true values, this indicates a model identifiability issue, an optimisation configuration problem, or a mismatch between data resolution and model structure.

The `Test.*` module and functions provides a way to scaffold a test using pre-determined criteria for the synthetic data, creation of observation error around the time-series, and other core testing parameters. A fully worked example of a model test may be seen in the [predator-prey example](/examples/predator-prey.html).

### Fitting the model with real data

Once the model and engine have been validated (or if you choose to skip testing), fitting is performed with:
*)

fun engine endCondition data model ->

    let result1 = Bristlecone.fit engine endCondition data model
    let result2  = Bristlecone.tryFit engine endCondition data model
    ()

(**
As with testing, the try fit function returns a `Result` type instead of an exception on error. The `endCondition` controls when the optimiser or sampler stops (e.g. number of iterations, convergence criteria).

### The esimtation result

Model-fitting returns an `EstimationResult`, which contains:

* the estimated parameter values,
* the full optimisation or sampling trace,
* the final model predictions,
* the likelihood and diagnostics,
* metadata about the engine and model.

#### Accessing estimated parameter values

To access parameter values from an `EstimationResult`, you may use the
`.Get` or `.TryGet` members to retrieve parameter estimates in their
original units of measure. You must pass the original parameter as
declared in the model to access individual values in this way.
*)

open Bristlecone.Language
open FSharp.Data.UnitSystems.SI.UnitNames

let exampleParameter = parameter "e" Positive 0.01<watt/second> 0.10<watt/second>

fun (result:ModelSystem.EstimationResult<'a,'c,'b>) ->

    let e = result.Parameters.Get exampleParameter
    let eR = result.Parameters.TryGet exampleParameter
    ()

(**
Here, you can see that the units of the retrieved parameters after estimation
are watts per second, as defined in the original model's parameters.
*)