(**
---
title: Optimisation
category: Documentation
categoryindex: 2
index: 1
---
*)

(*** condition: prepare ***)
#nowarn "211"
#r "../src/Bristlecone/bin/Release/netstandard2.0/Bristlecone.dll"
(*** condition: fsx ***)
#if FSX
#r "nuget: Bristlecone,{{package-version}}"
#endif // FSX
(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Bristlecone,{{package-version}}"
#endif // IPYNB

(**
Optimisation Methods
========================

The library includes a suite of in-built local and global optimisation methods. In addition, any custom optimisation procedure can be used with Bristlecone by plugging in as follows:
*)

open Bristlecone

let myCustomOptimiser : EstimationEngine.Optimise<float> =
    fun writeOut n domain f -> invalidOp "Doesn't actually do anything!"

let engine =
    Bristlecone.mkContinuous
    |> Bristlecone.withCustomOptimisation myCustomOptimiser

(**
The library itself includes Monte Carlo-based methods, and Amoeba (Nelder Mead Simplex) methods.

Standard Monte Carlo
--------------------

1. Random Walk Metopolis Hastings
2. Adaptive Metropolis
3. Filzbach-style
*)

open Bristlecone.Optimisation

let randomWalk = MonteCarlo.randomWalk
let am = MonteCarlo.adaptiveMetropolis 0.250 200
let randomWalkWithPreTuning =
    [ MonteCarlo.TuneMethod.Scale, 500, EndConditions.afterIteration 50000 ]
    |> MonteCarlo.randomWalk

(**

Tuning steps run in sequence before a 'clean' MCMC chain. In the above case, a the parameter space is scaled every 500 iterations for 50,000 iterations.


Simulated Annealing
-------------------

Simulated annealing models a minimisation problem in terms of particle entropy, where the function value is analogous to energy. At high temperatures, particles are in a high-energy state, thus can move readily. As temperature gradually decreases, particles are less able to move to high energy states, with particles eventually arranging into the ‘ground state’ of a solid material.
*)

let settings = MonteCarlo.SimulatedAnnealing.AnnealSettings<float>.Default

let classicalSA : EstimationEngine.Optimise<float> = 
    MonteCarlo.SimulatedAnnealing.classicalSimulatedAnnealing 0.01 settings

let fastSA : EstimationEngine.Optimise<float> = 
    MonteCarlo.SimulatedAnnealing.fastSimulatedAnnealing 0.01 settings

(**
Amoeba Methods
-------------------

The Amoeba (Nelder-Mead simplex) expands and contracts an n-dimensional geometry within an unconstrained parameter space, following a ‘downhill’ gradient. Our implementation draws a random point from a normal distribution, informed by the starting bounds for each parameter. 
*)

let single = Amoeba.Solver.solve Amoeba.Solver.Default

let swarmMode = Amoeba.swarm

(**
To address concerns of local minima, we included two additional variants. First, a ‘swarm’ mode creates an ensemble of 20 amoeba, which all crawl the likelihood surface. After n iterations, we discard all amoeba that have values above the 80th percentile of the negative log likelihood values returned. We run this procedure for five levels. 
*)