(**
---
title: Optimisation
category: Components
categoryindex: 4
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
Optimisation Methods
========================

Bristlecone includes a suite of in-built local and global 
optimisation methods. You can also use a custom optimisation
method defined in another library, or combine these with the
in-built optimisation techniques.

## Defining a custom optimisation method

A custom optimisation 
procedure can be used with Bristlecone by plugging in as follows:
*)

open Bristlecone

let myCustomOptimiser: EstimationEngine.Optimisation.Optimiser =
    EstimationEngine.Optimisation.InDetachedSpace
    <| fun writeOut n domain f -> invalidOp "Doesn't actually do anything!"

Bristlecone.mkContinuous ()
|> Bristlecone.withCustomOptimisation myCustomOptimiser

(**
## Included optimisation methods

The library itself includes Monte Carlo-based methods, and Amoeba (Nelder Mead Simplex) methods.
All methods are included in this namespace:
*)

open Bristlecone.Optimisation

(**
### Simulated Annealing (SA)

Simulated annealing models a minimisation problem in terms of particle 
entropy, where the function value is analogous to energy. At high 
temperatures, particles are in a high-energy state, thus can move 
readily. As temperature gradually decreases, particles are less able 
to move to high energy states, with particles eventually arranging 
into the ‘ground state’ of a solid material.

The SA implementation in Bristlecone includes classical SA that uses
a Boltzmann machine for jumps, and fast simulated annealing (FSA) that 
uses a Cauchy distribution for jumps:
*)

let settings = MonteCarlo.SimulatedAnnealing.AnnealSettings.Default

// Classical Simulated Annealing:
MonteCarlo.SimulatedAnnealing.classicalSimulatedAnnealing 0.01<``optim-space``> false settings

// Fast Simulated Annealing:
MonteCarlo.SimulatedAnnealing.fastSimulatedAnnealing 0.01<``optim-space``> false settings

(**
The FSA approach enables greater exploration of more distant portions of
parameter space more quickly and is therefore less prone to local minima
problems. 

For the SA implementation in Bristlecone, you must specify whether you
wish to use a temperature-dependent or temperature-independent proposal.
When temperature-dependent, the jump size increases at higher temperatures,
allowing coarser exploration of the parameter space initially, progressively
becoming more refined as the system cools.
*)

(**
### Nelder-Mead Methods

The Nelder-Mead simplex (or amoeba method) expands and contracts an n-dimensional 
geometry within an unconstrained parameter space, following a 
‘downhill’ gradient. The implementation here draws a random point from a 
normal distribution, informed by the starting bounds for each parameter. 

The Nelder-Mead simplex is most appropriate for simpler likelihood surfaces.
It can be used as follows:
*)

let settingsNM = Amoeba.Solver.Default
(*** include-value: settingsNM ***)

let single: EstimationEngine.Optimisation.Optimiser = Amoeba.single settingsNM

(**
A single Nelder-Mead solver is highly subject to local minima. To reduce
the prevalence of local minima, Bristlecone includes a *swarm* implementation
of the Nelder-Mead simplex, which, creates an ensemble of 20 amoeba, all of which
crawl the likelihood surface. After n iterations, all amoeba that have values 
above the 80th percentile of the negative log likelihood values are
discarded. The procedure continues for five levels. The swarm mode
can be used from:
*)

let levels = 5
let individuals = 20

Amoeba.swarm levels individuals settingsNM

(**
### Monte Carlo methods

Monte Carlo Markov Chain (MCMC) methods are most often applied in Bayesian
analyses, but can also be used for optimisation to identify the -log likelihood.
Within Bristlecone, a number of variations of Monte Carlo methods are included.

Often, it is appropriate to use multiple methods in combination; initially, tuning
algorithm(s) are used to enhance the performance of the search. Once the tuning phase
is complete, a search is conducted to identify the minimum; finally, a sampling algorithm
may be run to explore the distribution around the minimum.

#### Random Walk Metropolis Hastings

The random walk algorithm may optionally be passed a list of `TuningStep`s,
which will be run before the final random walk.

*)

open Bristlecone.Optimisation.MonteCarlo

// Random walk with no tuning steps
MonteCarlo.randomWalk []

// Random walk with 50,000 iterations of tuning, during
// which the individual parameter jump sizes are scaled
// every 500 iterations.
[ { Method = MonteCarlo.TuneMethod.Scale
    Frequency = 500<iteration>
    EndCondition = EndConditions.atIteration 50000<iteration> } ]
|> MonteCarlo.randomWalk


(**
#### Adaptive Metropolis (AM)

The AM algorithm continuously tunes the covariance structure of the
jump distribution based on the size of jumps in the recently-explored
parameter space. 
*)

let weighting = 0.250 // Weight to give to recent history versus existing covariance structure
let frequency = 200<iteration> // Tune the covariance structure every 200 iterations

MonteCarlo.adaptiveMetropolis weighting frequency

(**

#### Filzbach-style Monte Carlo

The Filzbach algorithm was introduced by Drew Purves, Microsoft Research
Cambridge. The Filzbach algorithm was designed for problems in ecological 
research. It contains a burn-in phase and a sampling phase. 
Four settings are required: the length of the burn-in phase (which can be
any Bristlecone `EndCondition`); the minimum and maximum scale changes that can
occur within the tuning phase, based on the ranges given for each parameter in
the model definition; and the number of changes after which to conduct parameter
tuning.
*)

open Bristlecone.Optimisation.MonteCarlo.Filzbach

let settingsFB =
    { TuneAfterChanges = 20
      MaxScaleChange = 100.
      MinScaleChange = 0.01
      BurnLength = EndConditions.atIteration 100000<iteration> }

filzbach settingsFB

(**
#### 'Automatic' optimisation

Implementation similar to that proposed by Yang and Rosenthal: "Automatically Tuned
General-Purpose MCMC via New Adaptive Diagnostics"
[Reference](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.70.7198&rep=rep1&type=pdf)
*)

MonteCarlo.``Automatic (Adaptive Diagnostics)``

(**
#### Adaptive-Metropolis-within Gibbs

An adaptive Metropolis-within-Gibbs sampler that tunes the variance of
each parameter according to the per-parameter acceptance rate.
Reference: Bai Y (2009). “An Adaptive Directional Metropolis-within-Gibbs Algorithm.”
Technical Report in Department of Statistics at the University of Toronto.
*)

MonteCarlo.``Adaptive-Metropolis-within Gibbs``

(**
#### Metropolis-within Gibbs

A non-adaptive Metropolis-within-gibbs Sampler. Each parameter 
is updated individually, unlike the random walk algorithm.
*)

MonteCarlo.``Metropolis-within Gibbs``
