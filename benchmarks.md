
Bristlecone Benchmarks
===

## Optimisation Methods

| Model name | Optimisation method | n runs | Target minimum | Best estimated minimum | Median estimated minimum | Sum distance from true parameters | Parameter estimates (individual) | True solutions | Milliseconds taken (median) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Ackley (2D) | random walk MCMC | 10 | 0.000 | 1.762 (1.762 -> min) | 3.786 | 1.244 | best -0.249 / median 0.370 (±0.901)<br>best 0.015 / median 0.095 (±0.865) | 0.000 / 0.000 | 491 |

## Full Model Systems

### Time-series models

| Model | Optimisation method | Success % | n | Likelihood gap (med, tol=1.0) | Param RMSE (rel, median) | Time (ms, median) |
| --- | --- | --- | --- | --- | --- | --- |
| predator-prey (with gaussian noise) | random walk MCMC | 0.000% | 10 | -22.800 | 2.221 | 89.500 |
| predator-prey | random walk MCMC | 10.000% | 10 | -0.078 | 0.454 | 910.500 |