(**
---
title: Defining a Model-Fitting Method (Estimation Engine)
category: The Bristlecone Language
categoryindex: 1
index: 3
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
Defining an Estimation Engine
========================

The *estimation engine* is the definition of the method
through which model-fitting will take place, as defined
by Bristlecone's `EstimationEngine` type. If you are
competing multiple hypotheses, a single `EstimationEngine`
will suffice for all model hypotheses. 

Forthcoming

*)


(**## Time Modes

Bristlecone can run models in either discrete time, or continuous time. When running models in continuous time, an integration function is required:
*)

(**
Currently only fixed timesteps are supported, but variable timestep support (e.g. for sediment core data) is planned. 

Two integration functions are included:

| Solver | Function | Description |
| - | - | - |
| Runge-Kutta 4 (MathNet Numerics) | ``Integration.MathNet.integrate`` | A fourth-order Runge Kutta method to provide approximate solutions to ODE systems. |
| Runge-Kutta 547M (Open Solving Library for ODEs - Microsoft Research) | ``Integration.MsftOslo.integrate`` | A method based on classic Runge-Kutta, but with automatic error and step size control. [See the documentation](https://www.microsoft.com/en-us/research/wp-content/uploads/2014/07/osloUserGuide.pdf).|

## Optimisation

Bristlecone supports optimisation functions that have the following type signature:

```fsharp
type Optimise<'data> = int -> int -> Domain -> ('data[] -> 'data) -> ('data * 'data []) list
```

There are two optimsation techniques currently built-in:

| Method | Function | Description |
| - | - | - |
| Amoeba (Nelder-Mead) | ``Optimisation.Amoeba.solve`` | A gradient-descent method. |
| MCMC Random Walk | ``Optimisation.MonteCarlo.randomWalk`` | A method based on classic Runge-Kutta, but with automatic error and step size control. [See the documentation](https://www.microsoft.com/en-us/research/wp-content/uploads/2014/07/osloUserGuide.pdf).|


## Estimation Engines

To use Bristlecone functions requires a configured ``EstimationEngine``. The easiest way is with the helper functions within the ``Bristlecone`` module:

| Function | Type | Description |
| - | - | - |
| ``mkContinuous`` | EstimationEngine | Default continuous engine |
| ``mkDiscrete`` | EstimationEngine | Default discrete model engine |
| ``withContinuousTime`` | t : Integrate<'a,'b> -> engine: EstimationEngine<'a,'b> -> EstimationEngine<'a,'b> | Transforms engine to continuous time mode, using the given integrator function. |
| ``withConditioning`` | c: Conditioning -> engine: EstimationEngine<'a,'b> -> EstimationEngine<'a,'b> | Choose how the start point is chosen when solving the model system. |

**)
