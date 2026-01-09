(**
[![Script](https://acm.im/bristlecone//img/badge-script.svg)](https://acm.im/bristlecone//language.fsx)&emsp;
[![Notebook](https://acm.im/bristlecone//img/badge-notebook.svg)](https://acm.im/bristlecone//language.ipynb)

*)
#r "nuget: Bristlecone,3.0.0-beta1"
(**
# The Bristlecone Language: Writing Models

Bristlecone includes a language for expressing mathematical models, and combining
these to form hypotheses for testing. You can use the language by opening it's
namespace:

*)
open Bristlecone // Opens Bristlecone core library and estimation engine
open Bristlecone.Language // Open the language for writing Bristlecone models
open FSharp.Data.UnitSystems.SI.UnitSymbols // Open F#'s SI units
open Bristlecone.Time // Open the time module to use built-in time units
(**
## Writing mathematical expressions

In Bristlecone, mathematical expressions to be used within
a model may be written using simple terms. Consider the following
example of an ordinary differential equation of logistic growth.

In the simplest form, first estimatable parameters must be declared with their reference,
any constraints, and the initial bounds. Then, the model may be defined
by applying these parameters. Here, `B` is defined in kilograms, with the
`r` parameter a dimensionless rate scalar.

*)
// Parameters:
let r = parameter "r" notNegative 0.01< / day> 0.50< / day>
let K = parameter "K" notNegative 20.<kg> 50.<kg>

let dBdt = P r * This<kg> * (Constant 1. - (This / P K))
(**
Here, the model is defined as a differential (rate) equation, which is evident
in its type signature (`kg/day`). We have used the following model expressions:

* `This` - the subject of the equation, which in this
case is **N**, the population size. The first use of this
was type-annotated with a unit for ease of reading.
  

* `P` - substitutes as estimatable parameter when followed by the pre-declared parameter.
Here, we have defined two parameters:
  

  * `Paramater "r"` - the intrinsic growth rate of the population;
    
  
  * `Parameter "K"` - the **carrying capacity** of the population.
    
  

* `Constant` - a fixed parameter or constant value within an equation.
  

You can use all standard mathematical operators `* / + - % **` to build your model.
There are additional model expressions that may be used to build up more complex
expressions (which are defined in the `ModelExpression` discriminated union).

All `ModelExpression`s can have units of measure attached, and arithmetic will
follow naturally.

## Additional model expressions

### Time-varying parameters (environmental forcings)

Often, a time-series model needs to take account of environmental
variability. For example, changes in temperature and precipitation
may influence the growth rate of a plant over time. Bristlecone supports
time-varying parameters in equations using the form:

*)
let tMin = environment<K> "T[min]"

// Use in models:
Environment tMin
(**
Here, we have defined the minimum temperature (**T[min]()**) as a time-varying (or environment) parameter in Kelvin.
Unlike `Parameter` items, **T[min]()** will not be estimated during model-fitting, but
will be read from the environmental properties for the appropriate time interval.

### Conditional

You can define a conditional model fragment by using the
`Conditional` term. A `Conditional` model expression requires
three arguments: a boolean condition, the model expression to use
if the boolen condition is true, and one for if it is false.

When building boolean values using model expressions, the comparison
operators require a dot (`.<`, `.>`). In this example, the value
of a specific parameter is guarded such that the model is only valid
if the parameter `a` * 5 is not near-zero:

*)
let a = parameter "a" notNegative 0.2<m / kg> 0.5<m / kg>

let linear: ModelExpression<m / kg> =
    Conditional (P a * Constant 5. .< Constant 1e-12<m / kg>) Invalid (P a * This)
(**
In this example, the function is limited such that
the combination of parameter value **a** with a theoretical
value of **This** at 5.0 cannot result in an answer that is
less than 1e-12. If this is the case, the result of the
model is `Invalid`.

### Invalid

The `Invalid` keyword is useful in combination with the
`Conditional` keyword, as demonstrated above. When any model expression
returns `Invalid`, the model fails. During optimisation, this is useful
to constrain the search within parameter space to values that do not
tend to infinity, or where values may be theoretically implausable.

### Time

The `Time` keyword simply retrives the current time index; as such, it
can be used to formulate time-dependent models. The value of `Time`
is tied to the temporal resolution of the model. For example, if a
differential-time model is defined in the time units `day`, then
`Time` will present in daily time units.

*)

