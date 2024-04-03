#### 2.0.0 - unreleased
* FEATURE: Benchmark suite of common test functions for optimisation routines
* FEATURE: Performance tuning of optimisers. For example, Filzbach 94% faster.

#### 2.0.0-alpha2 - 2022-04-27
* FEATURE: Add univariate gaussian -log likelihood function

#### 2.0.0-alpha1 - 2021-09-28
* FEATURE: Domain Specific Language (DSL) for scaffolding models in the `Bristlecone.Language` namespace.
* FEATURE: Pass user-defined Random instances to the EstimationEngine to enable working with seeds for reproducing previous results.
* FEATURE: Automatically generate named hypotheses from nested model systems.
* BREAKING: We recommend using the DSL to construct models. Underlying F# record type signatures have changed and will break version 1.x scripts.

#### 1.0.0 - 2019-09-10
* FEATURE: Basic component logging of internal model processes.
* FEATURE: Calculate and save convergence statistics between MCMC chains.
* FEATURE: Sunlight cache to precompute day length calculations for faster analyses.
#### 1.0.0-alpha6 - 2019-06-17
* FEATURE: Configuration options for Filzbach and SA optimisation.
* FEATURE: Calculations for day length (sunrise and sunset) built-in to Bristlecone.Dendro.
* FEATURE: Calculate confidence intervals using likelihood interval technique.
* FEATURE: Standardised formats for loading and saving model-selection results. 
#### 1.0.0-alpha5 - 2019-03-23
* Release net47 and netstandard2.0 versions
* Automatic documentation (based on ProjectScaffold)
#### 1.0.0-alpha4 - 2019-03-18
* BUGFIX: Fixed divide by zero error when running `testModel`
#### 1.0.0-alpha3 - 2019-03-18
* FEATURE: Simulated Annealing optimisation
* FEATURE: Filzbach-style optimisation
* FEATURE: Run models using higher-resolution forcing data with lower-resolution observations
* FEATURE: Measurement variables
#### 1.0.0-alpha2 - 2019-01-10
* FEATURE: More optimisation algorithms, including: automatic generalised MCMC; Metropolis-within-Gibbs; and Adaptive Metropolis-within-Gibbs
* FEATURE: Support for 'EndConditions' on optimisation algorithms. The library includes a) number of iterations, and b) linear trends in square jumping distances, as two possibilities.
#### 1.0.0-alpha1 - 2018-12-20
* FEATURE: Initial release