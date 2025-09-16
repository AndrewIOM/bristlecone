(**
---
title: Composing models into many hypotheses
category: The Bristlecone Language
categoryindex: 1
index: 4
---
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
Composing model parts together
======================

Bristlecone has been designed to enable swift *model composition*, whereby
smaller models may be composed into larger systems of models.

To get started, open the Bristlecone libraries.
*)

open Bristlecone // Opens Bristlecone core library and estimation engine
open Bristlecone.Language // Open the language for writing Bristlecone models

(**



### Nested model systems

One key use of compositional modelling is to understand which (ecological) theories
best represent one or many particular time-series data. A *nested model* is one where
one or more processes may be substituted for different theoretical forms and competed
to determine which combination of model components best represents the given data.

Using Bristlecone, we can simply define a nested model as a Bristlecone model but where
the definition takes some additional model components as function parameters. For example,
take the following model:
*)

let someModel someFn =
    Parameter "K" + someFn This - Environment "T" * Parameter "J"

(**
In the above expression, `someFn` is a function of signature `ModelExpression -> ModelExpression`.
Looking at the equation, you can see that `someFn` takes one argument, the model expression
`This`, which represents the current state of the model equation.

We can take this concept further to define a *nestable model*, in which a number of components
may be swapped for alternative mathematical forms based on (ecological) theory. Here, we will
apply a simple model of non-linear plant growth. To do this, we
first define a base model as a `ModelSystem` that takes a number of interchangable components:
*)

let baseModel growthLimit lossRate =
    Model.empty
    |> Model.addEquation "m" (Parameter "r" * (growthLimit This) - lossRate This)
    |> Model.estimateParameter "r" noConstraints 0.01 1.00
    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ "x" ])

(** 
Importantly, unlike when normally building models, we do not call `Model.compile` at the end.
This step will be done later.

We can then design the growth limit and loss rate components that will fit into this model.
These are made using the `modelComponent` and `subComponent` functions as follows:
*)

let growthLimit =
    modelComponent
        "Limitation to growth"
        [

          subComponent "Linear" (fun _ -> Constant 1.)

          subComponent "Monomolecular" (fun m -> Parameter "K" - m)
          |> estimateParameter "K" notNegative 10.0 50.0

          subComponent "Gompertz" (fun m -> m * log (Parameter "K" / m))
          |> estimateParameter "K" notNegative 10.0 50.0

          subComponent "Logisitc (three parameter)" (fun m -> m * (Constant 1. - (m / Parameter "K")))
          |> estimateParameter "K" notNegative 10.0 50.0 ]

(**
If a component requires additional parameters over the base model, these may be added by piping into
the `estimateParameter` function as above. 

We can similarly define the other required component for the base model - the loss rate:
*)

let lossRate =
    modelComponent
        "Biomass loss rate"
        [

          subComponent "None" (fun _ -> Constant 0.)

          subComponent "Density-dependent" (fun m -> m * Parameter "alpha")
          |> estimateParameter "alpha" notNegative 0.001 0.10 ]

(**
Once the components are set up, we can compile the nested model by making all possible
combinations of the model components. We do this using the `Hypotheses` module, which will
compile a list of model hypotheses for us to test.
*)

let hypotheses =
    baseModel
    |> Hypotheses.createFromComponent growthLimit
    |> Hypotheses.useAnother lossRate
    |> Hypotheses.compile

(**
The resultant constructed hypotheses are shown in the following printout:
*)

(***include-value: hypotheses ***)

(**
For further analysis, you may choose to use the orchestration functionality to
run many hypotheses and replicates (multiple fits per hypothesis) in parallel.
*)
