(**
[![Script](https://acm.im/bristlecone//img/badge-script.svg)](https://acm.im/bristlecone//estimation-engine.fsx)&emsp;
[![Notebook](https://acm.im/bristlecone//img/badge-notebook.svg)](https://acm.im/bristlecone//estimation-engine.ipynb)

*)
#r "nuget: Bristlecone,3.1.0"
(**
# Defining an Estimation Engine

### What is an estimation engine?

An estimation engine in Bristlecone defines how parameters of models are estimated from data. It's key elements are:

* the time mode (discrete- or continuous-time),

* the integration method (for continuous-time models),

* the optimisation or sampling method,

* the conditioning strategy,

* the random seed and reproducibility settings.

A single engine should typically be used across all model hypotheses in a study so that comparisons are fair and reproducible.

Bristlecone provides defaults for most components, but all parts are configurable.
Below, the main configurable parts are outlined. Please check the API docs for further details on each option.
Estimation engines are composed using functions within the `Bristlecone` module, as below.

### Creating an engine

Bristlecone supports two ways of evolving a model through time:

#### Discrete-time engine

The model is evaluated once per time step. This is appropriate for any system where the process is naturally discrete.
Call `Bristlecone.mkDiscrete ()` for this engine type.

#### Continuous-time engine

The model is defined as a system of ordinary differential equations (ODEs) and must be integrated numerically.
Call `Bristlecone.mkContinuous ()` for this engine type.

ODE model systems are appropriate for processes that are most naturally represented as rates. Bristlecone will solve continuous-time models dependent on the time-steps of the datasets provided, whether those are on fixed time-stepping (e.g. annual ring widths) or variable time-stepping (e.g. sediment core data).

Bristlecone includes a classical Runge-Kutta integrator as the default for continuous-time models; see [integration methods](/integration.html) for more details.

Example:

*)
let engine: EstimationEngine.EstimationEngine<System.DateTime,System.TimeSpan,Time.year,1> =
    Bristlecone.mkContinuous ()
(**
### Customising an engine

All engine components can be modified using the builder functions in the `Bristlecone` module.

#### Choosing an optimisation method

Use:

*)
Bristlecone.withCustomOptimisation
(**
to supply any optimiser with the correct type signature.

The default optimisation method is 'Bristlecone-style annealing'; for details of this method and others
see [optimisation methods](/optimisation.html) for more details.

Convenience functions include:

* `Bristlecone.withGradientDescent` - Nelder–Mead simplex.

* `Bristlecone.withTunedMCMC` — homogeneous random‑walk MCMC with optional tuning steps.

* `Bristlecone.withBristleconeOptimiser` - Bristlecone’s hybrid simulated‑annealing + MCMC optimiser.

Example:

*)
engine |> Bristlecone.withCustomOptimisation Optimisation.MonteCarlo.``Adaptive-Metropolis-within Gibbs``
(**
#### Conditioning the first time-point

The data may be conditioned such that the value of the data in the first point in the time-series is not lost. There are three in-built conditioning modes:

* `Conditioning.NoConditioning` (default). The first time point is used as the starting point (t0) in fitting.

* `Conditioning.RepeatFirstDataPoint`. The first time point is repeated so that it is used as both t0 and t1.

* `Conditioning.Custom`. Specify custom starting conditions.

Example:

*)
engine |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint
(**
#### Connecting model time to data time

The temporal resolution of the model must be linked to the temporal system of the data. Bristlecone's models and engines are strongly typed such that conformity of time systems is enforced by the compiler.

To link your model and data time, the Bristlecone builders contains a `Bristlecone.withTimeConversion` function. To illustrate, a model may be defined at annual resolution, with the data defined in radiocarbon dates. The engine just requires a simple conversion function so that it understands how radiocarbon dates relate to years. In the `Bristlecone.Time.DateMode.Conversion` module, each built-in time system (e.g. radiocarbon, annual, .NET calendars) has conversion functions to relate these to the basic temporal resolutions of day, month, and year.

For example, the below conversion makes an estimation engine that can directly consume a model that is defined in monthly resolution, and requires data defined in standard .NET `DateTime` times.

*)
let forMonthlyModel: EstimationEngine.EstimationEngine<System.DateTime,System.TimeSpan,Time.month,1> =
    Bristlecone.mkDiscrete ()
    |> Bristlecone.withTimeConversion Time.DateMode.Conversion.CalendarDates.toMonths
(**
You can see that the estimation engine has typed units of `Time.month` as the model resolution, and `System.DateTime` / `System.TimeSpan` as the date / timespan units.

#### Reproducibility

You may set a seed to ensure that any particular analysis is reproducable by calling `Bristlecone.withSeed`. Note that if a multi-threaded orchestrator is used, this will not guarantee reproducibility, as the orchestrator may run work packages in different orders depending on race conditions.

#### Logging

Use:

*)
Bristlecone.withOutput
(**
to set a custom logger. The default is a simple console logger that outputs a trace every 1000 iterations.
More built-in loggers are defined in the `Bristlecone.Logging` module, such as a multi-threaded table logger.
In addition, the `Bristlecone.Charts.R` package defines a graphical parameter trace output to see real-time
traces during optimisation.

### Example: continuous time with Nelder-Mead optimisation

*)
let engine2: EstimationEngine.EstimationEngine<Time.DatingMethods.Annual,int<Time.year>,Time.year,1> =
    Bristlecone.mkContinuous ()
    |> Bristlecone.withCustomOptimisation (Optimisation.Amoeba.swarm 10 20 Optimisation.Amoeba.Solver.Default)
    |> Bristlecone.withConditioning Conditioning.NoConditioning
    |> Bristlecone.withSeed 1500
    |> Bristlecone.withTimeConversion Time.DateMode.Conversion.Annual.toYears
(**
Here, an estimation engine is made that uses a swarm-based Nelder-Mead (amoeba) optimisation routine to fit annual time-series data to a model defined in terms of years.

*)

