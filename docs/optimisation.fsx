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

### A note on end conditions

Many end conditions are included in the `Bristlecone.Optimisation.EndConditions` module.
The appropriate end conditions are often algorithm-dependent. However, Bristlecone
includes some sensible defaults for certain classes of algorithms in the
`Bristlecone.Optimisation.EndConditions.Profiles` module. For example, there are
profiles for tuning stages in MCMC algorithms, and for homogeneous MCMC chains.

In the following outlines of each included algorithm, appropriate end coditions
will be suggested.

### Included optimisation methods

The library itself includes Monte Carlo-based methods, and Amoeba (Nelder Mead Simplex) methods.
All methods are included in this namespace:
*)

open Bristlecone
open Bristlecone.Optimisation

(**

#### Simulated Annealing (SA)

Simulated annealing models a minimisation problem in terms of particle 
entropy, where the function value is analogous to energy. At high 
temperatures, particles are in a high-energy state, thus can move 
readily. As temperature gradually decreases, particles are less able 
to move to high energy states, with particles eventually arranging 
into the ‘ground state’ of a solid material.

The SA implementation in Bristlecone includes classical SA that uses
a Boltzmann machine for jumps, and fast simulated annealing (FSA) that 
uses a Cauchy distribution for jumps.

##### Classical Simulated Annealing

Classical simulated annealing may be used as follows:
*)

let settings = MonteCarlo.SimulatedAnnealing.AnnealSettings.Classical
let csa = MonteCarlo.SimulatedAnnealing.classicalSimulatedAnnealing 0.01<``optim-space``> false settings

(**
Classical SA relies on normal distribution for proposals. The default settings
(as above) run each heating and annealing stage from the end of the previous one,
with relatively strict end conditions. Classical SA is therefore a relatively
inefficient global optimisation meta-heuristic.

References:

- Corana, A., M. Marchesi, C. Martini, and S. Ridella. ‘Minimizing Multimodal Functions of Continuous Variables with the “Simulated Annealing” Algorithm—Corrigenda for This Article Is Available Here’. ACM Transactions on Mathematical Software (TOMS) 13, no. 3 (1987): 262–80. https://doi.org/10.1145/29380.29864.
- Locatelli, M. ‘Simulated Annealing Algorithms for Continuous Global Optimization, Handbook of Global Optimization’. In Simulated Annealing Algorithms For Continuous Global Optimization, edited by Panos M. Pardalos and H. Edwin Romeijn. Boston, MA, 2002. https://doi.org/10.1007/978-1-4757-5362-2.
- Tsallis, C., Daniel A. Stariolo, and 1996. ‘Generalized Simulated Annealing’. Physics A 233, nos 1–2 (1996): 395–406. https://doi.org/10.1016/S0378-4371(96)00271-3.

##### Fast Simulated Annealing

Fast simulated annealing may be used as follows:
*)

let fsaSettings = MonteCarlo.SimulatedAnnealing.AnnealSettings.Fast
let fsa = MonteCarlo.SimulatedAnnealing.fastSimulatedAnnealing 0.01<``optim-space``> false fsaSettings

(**
The FSA approach enables greater exploration of more distant portions of
parameter space more quickly and is therefore less prone to local minima
problems. 

For the SA implementation in Bristlecone, you must specify whether you
wish to use a temperature-dependent or temperature-independent proposal.
When temperature-dependent, the jump size increases at higher temperatures,
allowing coarser exploration of the parameter space initially, progressively
becoming more refined as the system cools.

References:

- Szu, Harold, and Ralph Hartley. ‘Fast Simulated Annealing’. Physics Letters A 122, nos 3–4 (1987): 157–62. https://doi.org/10.1016/0375-9601(87)90796-1.
- Sharman, K. C. ‘Maximum Likelihood Parameter Estimation by Simulated Annealing’. ICASSP-88., International Conference on Acoustics, Speech, and Signal Processing, 1988, 2741–44. https://doi.org/10.1109/ICASSP.1988.197217.

##### 'Bristlecone-style' optimisation

The default optimiser in Bristlecone, which is:
*)

let bco = Optimisation.MonteCarlo.bristleconeSampler

(**
The Bristlecone-style routine conducts the following process:

1. Pre-tuning ('clean' MCMC)
2. Step size tuning
3. Fast simulated annealing (ending at T=1)
4. Homogeneous MCMC chain

The final chain stops using the default MCMC stopping profile, which
stops when a chain is 'well behaved' and stationary. It will also stop
if the maximum specified iteration count is reached if specified by the
user in the end condition passed to the Bristlecone fit function.
*)

(**
#### Nelder-Mead Methods

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
#### Monte Carlo methods

Monte Carlo Markov Chain (MCMC) methods are most often applied in Bayesian
analyses, but can also be used for optimisation to identify the -log likelihood.
Within Bristlecone, a number of variations of Monte Carlo methods are included.

Often, it is appropriate to use multiple methods in combination; initially, tuning
algorithm(s) are used to enhance the performance of the search. Once the tuning phase
is complete, a search is conducted to identify the minimum; finally, a sampling algorithm
may be run to explore the distribution around the minimum.

##### Random Walk Metropolis Hastings

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
##### Adaptive Metropolis (AM)

The AM algorithm continuously tunes the covariance structure of the
jump distribution based on the size of jumps in the recently-explored
parameter space. 
*)

let weighting = 0.250 // Weight to give to recent history versus existing covariance structure
let frequency = 200<iteration> // Tune the covariance structure every 200 iterations

MonteCarlo.adaptiveMetropolis weighting frequency

(**

##### Filzbach-style Monte Carlo

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

References:

* Microsoft Corporation. Filzbach User Gude. 2011.

##### 'Automatic' optimisation

Implementation similar to that proposed by Yang and Rosenthal.
*)

MonteCarlo.``Automatic (Adaptive Diagnostics)``

(**

References:

* Yang, Jinyoung, and Jeffrey S. Rosenthal. ‘Automatically Tuned General-Purpose MCMC via New Adaptive Diagnostics’. Computational Statistics 32, no. 1 (2016): 315–48. https://doi.org/10.1007/s00180-016-0682-2.


##### Adaptive-Metropolis-within Gibbs

An adaptive Metropolis-within-Gibbs sampler that tunes the variance of
each parameter according to the per-parameter acceptance rate.
*)

MonteCarlo.``Adaptive-Metropolis-within Gibbs``

(**
Reference:

* Bai, Yan. ‘An Adaptive Directional Metropolis-within-Gibbs Algorithm’. Technical Report in Department of Statistics at the University of Toronto, 2009.

##### Metropolis-within Gibbs

A non-adaptive Metropolis-within-gibbs Sampler. Each parameter 
is updated individually, unlike the random walk algorithm.
*)

MonteCarlo.``Metropolis-within Gibbs``

(**
### Defining a custom optimisation method

A custom optimisation 
procedure can be used with Bristlecone by plugging in as follows:
*)

let myCustomOptimiser: EstimationEngine.Optimisation.Optimiser =
    EstimationEngine.Optimisation.InDetachedSpace
    <| fun writeOut n domain f -> invalidOp "Doesn't actually do anything!"

Bristlecone.mkContinuous ()
|> Bristlecone.withCustomOptimisation myCustomOptimiser

