(**
---
title: Writing Models
category: The Bristlecone Language
categoryindex: 1
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
The Bristlecone Language: Writing Models
======================

Bristlecone includes a language for expressing mathematical models, and combining
these to form hypotheses for testing. You can use the language by opening it's
namespace:

*)

open Bristlecone // Opens Bristlecone core library and estimation engine
open Bristlecone.Language // Open the language for writing Bristlecone models

(**

Writing mathematical expressions
---

In Bristlecone, mathematical expressions to be used within
a model may be written using simple terms. Consider the following
example of an ordinary differential equation of logistic growth:
*)

let dNdt = Parameter "r" * This * (Constant 1. - (This / Parameter "K"))

(** 
Here, we have used the following model expressions:

  * `This` - the subject of the equation, which in this 
    case is *N*, the population size.

  * `Parameter` - an estimatable parameter followed by its name. Here, we have
    defined two parameters:
      - `Paramater "r"` - the intrinsic growth rate of the population;
      - `Parameter "K"` - the *carrying capacity* of the population.

  * `Constant` - a fixed parameter or constant value within an equation.

You can use all standard mathematical operators `* / + - % **` to build your model.
There are additional model expressions that may be used to build up more complex
expressions (which are defined in the `ModelExpression` discriminated union).

Additional model expressions
---

### Time-varying parameters

Often, a time-series model needs to take account of environmental
variability. For example, changes in temperature and precipitation
may influence the growth rate of a plant over time. Bristlecone supports
time-varying parameters in equations using the form:
*)

Environment "T[min]"

(**
Here, we have defined the minimum temperature (*T[min]*) as a time-varying (or environment) parameter.
Unlike `Paramater` items, *T[min]* will not be estimated during model-fitting, but
will be read from the environmental properties for the appropriate time interval.

### Conditional

You can define a conditional model fragment by using the
`Conditional` term. A `Conditional` model expression takes a function
with one argument - a function that represents the future
computation of the model expression (`compute` -
with signature `ModelExpression -> float`). Within this function,
you can then evaluate the answer of any model expressions
by passing them as the argument to the `compute` function.
Please see the below example:
*)

let linear =
    Conditional(fun compute ->
        if compute (Parameter "a" * Constant 5.) < 1e-12 then
            Invalid
        else
            Parameter "a" * This)

(**
In this example, the function is limited such that
the combination of parameter value *a* with a theoretical
value of *This* at 5.0 cannot result in an answer that is 
less than 1e-12. If this is the case, the result of the 
model is `Invalid`.

### Invalid

The `Invalid` keyword is useful in combination with the
`Conditional` keyword, as demonstrated above. When any model expression
returns `Invalid`, the model fails. During optimisation, this is useful
to constrain the search within parameter space to values that do not
tend to infinity, or where values may be theoretically implausable.

### Arbitrary

Sometimes, a complex sub-model may need to be inserted where you
do not wish to rewrite it into the Bristlecone language. You may also
not have access to the original source, or be calling from an external
library. In these cases, you can use the `Arbitrary` model expression
to wrap the function for Bristlecone. Helper functions are included
to simplify this process within the `Arbitrary` module:
*)

/// x: population size
/// t: current time
/// z: some environmental value
let someArbitraryFn x t z = x + t * 1.0 - z

// TODO Insert example here

(**

### Time

The `Time` keyword simply retrives the current time index; as such, it
can be used to formulate time-dependent models.
*)
