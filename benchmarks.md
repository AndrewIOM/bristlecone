
Bristlecone Benchmarks
===

## Optimisation Methods

| Model name | Optimisation method | n runs | Target minimum | Best estimated minimum | Median estimated minimum | Sum distance from true parameters | Parameter estimates (individual) | True solutions | Milliseconds taken (median) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Bukin Sixth | random walk MCMC | 10 | 0.000 | 0.261 (0.261 -> min) | 0.459 | 3.748 | best -2.840 / median -8.403 (±6.225)<br>best 0.081 / median 0.706 (±0.656) | -10.000 / 1.000 | 955 |
| Holder Table | random walk MCMC | 10 | -19.209 | -19.208 (0.000 -> min) | -19.206 | 0.021 | best -8.058 / median -8.036 (±8.317)<br>best 9.659 / median 0.006 (±10.188) | 8.055 / 9.665<br>8.055 / -9.665<br>-8.055 / 9.665<br>-8.055 / -9.665 | 631.5 |
| Cross in tray | random walk MCMC | 10 | -2.063 | -2.063 (0.000 -> min) | -2.063 | 0.023 | best 1.346 / median 1.333 (±1.301)<br>best -1.351 / median 1.340 (±1.296) | 1.349 / -1.349<br>1.349 / 1.349<br>-1.349 / 1.349<br>-1.349 / -1.349 | 593 |

## Full Model Systems

### Time-series models

| Model name | Optimisation method | success % | n runs | Distance from minimum likelihood (median) | Distance from true parameter values | Milliseconds taken (median) |
| --- | --- | --- | --- | --- | --- | --- |
| predator-prey (with gaussian noise) | random walk MCMC | 100.000% | 10 | 425.467 | [Δ] 0.027 best / median 0.092 (±0.083)<br>[α] 0.002 best / median 0.099 (±0.075)<br>[β] 0.006 best / median 0.097 (±0.076)<br>[γ] 0.024 best / median 0.105 (±0.075)<br>[ρ] 0.072 best / median 0.296 (±0.357)<br>[σ[x]] 0.057 best / median 0.251 (±0.176)<br>[σ[y]] 0.036 best / median 0.170 (±0.370) | 716 |
| predator-prey | random walk MCMC | 100.000% | 10 | 0.521 | [Δ] 0.017 best / median 0.125 (±0.254)<br>[α] 0.000 best / median 0.071 (±1.785)<br>[β] 0.002 best / median 0.102 (±1.822)<br>[γ] 0.008 best / median 0.096 (±0.218) | 240.5 |