(**
---
title: Defining a Hypothesis (or Model System)
category: The Bristlecone Language
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
Defining a Hypothesis (or Model System)
========================

In Bristlecone, a `ModelSystem` represents combination
of mathematical models and their estimatable parameters
that can be used within model-fitting and model-selection.
A `ModelSystem` is defined by:

* at least one equation;
* at least one parameter to estimate;
* a likelihood function; and
* optionally, measurement equations.

To build a `ModelSystem`, first open the Bristlecone language:
*)

open Bristlecone // Opens Bristlecone core library and estimation engine
open Bristlecone.Language // Open the language for writing Bristlecone models
open FSharp.Data.UnitSystems.SI.UnitSymbols // Opens SI units for use in models

(**
Second, define the parts of your hypothesis. In this example,
we will define a simple model of population growth:

*Note: refer to the section on [writing models](language.html) for help writing
Bristlecone expressions.*
*)

[<Measure>]
type individuals

let r = parameter "r" Positive 0.01< / Time.day> 0.50< / Time.day>
let K = parameter "K" Positive 20.<individuals> 50.<individuals>
let N = state<individuals> "N"

(**
When making parameters, first pass an identifiable short code that
you will see in output files.
Next, you must pass the constraint mode. In Bristlecone, although
the optimisation algorithms are *unconstrained*, it is possible
to constrain parameters to specific values using transformations. These
are applied automatically within the model-fitting process. Two available
options are `noConstaints`, which is unconstrained, and `positiveOnly`, which
prevents the value of that parameter dropping below zero. The last 
arguments are two numbers representing a lower and upper bound
from which to draw an initial starting value for the parameter.
The starting value is drawn from a continuous uniform distribution,
using a `Random` that you define in the `EstimationEngine` (see [estimating](estimation-engine.html)).
*)

let ``dN/dt``: ModelExpression<individuals / Time.day> =
    P r * This * (Constant 1. - (This / P K))

(**
From the above code, our model is compiled into the following internal representation:
*)
(*** include-value: ``dN/dt`` ***)

(** 
Our model of the change in *N* (population count) over time has two estimatable parameters 
defined using the `Parameter` term,
which are *r* (the intrinsic growth rate of the population)
and *K* (the carrying capacity of the population). 
The type system verifies that all required parameters are in place, and conducts
unit arithmetic to ensure that specified units make sense at compile-time.

Next, we need to create our `ModelSystem` by using the helper functions
that can be found under `Model.*`.

In this example, we will use a sum-of-squares measure to determine
the goodness-of-fit rather than a likelihood function. A selection of
likelihood functions, including sum-of-squares functions, are included
in Bristlecone within the `ModelLibrary` module.

The `ModelSystem` can be created with forward pipes (`|>`) as follows:
*)

let hypothesis =
    Model.empty
    |> Model.addRateEquation N ``dN/dt``
    |> Model.estimateParameter r
    |> Model.estimateParameter K
    |> Model.useLikelihoodFunction (ModelLibrary.NegLogLikelihood.SumOfSquares [ Require.state N ])
    |> Model.compile

(**
In the above code, we first start with an empty model system,
`Model.empty`. We then add each component with a short code to
represent it, and finally call `Model.compile`. The `Model.compile`
function checks the validity of the model in terms of duplicate short codes,
making sure all parameters are set to be estimated, and that all the
required parts are in place.
*)
