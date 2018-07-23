
![Bristlecone - ecological time-series modelling in F#](logo.png "Bristlecone")

An F# library for model-fitting model-selection (MFMS) of ecological models to observational data. The library was developed for individual-based plant modelling using tree ring time-series, but can be used for any ecological models. 

[![Build status](https://ci.appveyor.com/api/projects/status/0rh5alxrdct225st/branch/master?svg=true)](https://ci.appveyor.com/project/AndrewIOM/dendro-fit/branch/master)

**Note: this library is currently experimental.**

### Contents

* [Quick Start: Predator-Prey Dynamics]((#quick-start-predator-prey-dynamics))
* Working with Time Series
    * Growth Time-Series: Absolute, Cumulative, Relative
* Setting a Likelihood Function
* Measurement Variables
* Environmental Forcing
* Optimisation and Differentiation: Included Methods

### Quick Start: Predator-Prey Dynamics

An example is available in the script file ``samples/1. lynx-hare.fsx``.

## Time Series

Bristlecone supports models in both continuous time and discrete time. For a system of ordinary differential equations, Bristlecone has the following supported solvers out of the box:

| Solver | Description |
| - | - |
| MathNet Numerics Runge-Kutta 4 | A basic solver. |
| Microsoft Research OSLO (Runge-Kutta 547M) | A solver appropriate for stiff equations. Contains variable stepping. |

## Using Bristlecone in an F# Script

See the examples available in the ``samples`` folder.

---

Logo based on <a href="https://vecteezy.com">Free Vector Art by www.vecteezy.com</a>