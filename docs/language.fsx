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
The Bristlecone Language: Writing Models
======================

Bristlecone includes a language for expressing mathematical models, and combining
these to form hypotheses for testing. You can use the language by opening it's
namespace:

*)

open Bristlecone // Opens Bristlecone core library and estimation engine
open Bristlecone.Language // Open the language for writing Bristlecone models
open FSharp.Data.UnitSystems.SI.UnitSymbols // Open F#'s SI units
open Bristlecone.Time // Open the time module to use built-in time units

(**

Writing mathematical expressions
---

In Bristlecone, mathematical expressions to be used within
a model may be written using simple terms. Consider the following
example of an ordinary differential equation of logistic growth.

In the simplest form, first estimatable parameters must be declared with their reference,
any constraints, and the initial bounds. Then, the model may be defined
by applying these parameters. Here, ``B`` is defined in kilograms, with the
``r`` parameter a dimensionless rate scalar.
*)

// Parameters:
let r = parameter "r" Positive 0.01< / day> 0.50< / day>
let K = parameter "K" Positive 20.<kg> 50.<kg>

let dBdt = P r * This<kg> * (Constant 1. - (This / P K))

(** 
Here, the model is defined as a differential (rate) equation, which is evident
in its type signature (``kg/day``). We have used the following model expressions:

  * `This` - the subject of the equation, which in this 
    case is *B*, the biomass. The first use of this
    was type-annotated with a unit for ease of reading.

  * `P` - substitutes as estimatable parameter when followed by the pre-declared parameter.
     Here, we have defined two parameters:
      - `Paramater "r"` - the intrinsic growth rate of the plant;
      - `Parameter "K"` - the maximum biomass of the plant.

  * `Constant` - a fixed parameter or constant value within an equation.

All ``ModelExpression``s can have units of measure attached, and arithmetic will
follow naturally. The compiler will not allow incompatible units.

You can use all standard mathematical operators `* / + - % **` to build your model.
In addition, the following terms are available:

* `Power` - raise a value to a power using a dimensionless index.
* `Logarithm` - the natural logarithm of an expression.
* `Exponent` - the exponent of an expression.

Bristlecone will compile model expressions into a streamlined form
before use. Because of this, you cannot for example use if..then..else
to write conditional statements based on expression values.

Additional model expressions
---

### Time-varying parameters (environmental forcings)

Often, a time-series model needs to take account of environmental
variability. For example, changes in temperature and precipitation
may influence the growth rate of a plant over time. Bristlecone supports
time-varying parameters in equations using the form:
*)

// Declare the environment variable:
let tMin = environment<K> "T[min]"

// Use the environment variable in a model:
let model1 = Environment tMin

(**
Here, we have defined the minimum temperature (*T[min]*) as a time-varying (or environment) parameter in Kelvin.
Unlike `Parameter` items, *T[min]* will not be estimated during model-fitting, but
will be read from the environmental properties for the appropriate time interval.

### Conditional

You can define a conditional model fragment by using the
`Conditional` term. A `Conditional` model expression requires
three arguments: a boolean condition, the model expression to use
if the boolen condition is true, and one for if it is false.

When building boolean values using model expressions, the comparison
operators require a dot (``.<``, ``.>``). In this example, the value
of a specific parameter is guarded such that the model is only valid
if the parameter ``a`` * 5 is not near-zero:
*)

let a = parameter "a" Positive 0.2<m / kg> 0.5<m / kg>

let linear: ModelExpression<m / kg> =
    Conditional (P a * Constant 5. .< Constant 1e-12<m / kg>) Invalid (P a * This)

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
to constrain the search within parameter space to mechanisms that resolve
in ecologically meaningful ways (i.e. away from values that are theoretically implausable).

### Inverse

The `Inverse` keyword may be used to compute the inverse of a model expression
that is monotonic in the variable of interest, allowing Bristlecone to solve
for a state or parameter value that would produce a given measurement. This
is useful when your data provide an observation that is not the state itself,
but a deterministic transformation of it.

The target expression, lower bound, and upper bound are all model expressions
themselves; as such, they may depend on parameters, environmental values, etc.

Below is an example of an allometric equation that converts between
biomass and radius, and is otherwise unstable to calculate the inverse:
*)

module AllometryInverse =

  [<Measure>] type mm
  [<Measure>] type g

  let B = state<g> "B"
  let R  = state<mm> "R"

  let a = parameter "a" Positive 0.1<mm/g> 5.0<mm/g>
  let b = parameter "b" Positive 0.01</g> 1.0</g>

  // Biomass -> Radius
  let forward biomass = (P a * biomass) / (Constant 1.0 + P b * biomass)

  // Inverse: Radius -> Biomass
  let inverse =
      let target = State R
      let low, high = Constant 100.<g>, Constant 1000.<g>
      Inverse forward target low high

(**
### Time

The `Time` keyword simply retrives the current time index; as such, it
can be used to formulate time-dependent models. The value of ``Time``
is tied to the temporal resolution of the model. For example, if a
differential-time model is defined in the time units ``day``, then
``Time`` will present in daily time units.
*)