
Bristlecone Benchmarks
===

## Optimisation Methods

| Model name | Optimisation method | n runs | Target minimum | Best estimated minimum | Median estimated minimum | Sum distance from true parameters | Parameter estimates (individual) | True solutions | Milliseconds taken (median) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Ackley (2D) | amoeba single | 10 | 0.000 | 0.000 (0.000 -> min) | 4.731 | 1.958 | best 0.000 / median 0.000 (±7.136)<br>best -0.000 / median 0.000 (±9.188) | 0.000 / 0.000 | 288.5 |
| Bukin Sixth | amoeba single | 10 | 0.000 | 0.002 (0.002 -> min) | 0.006 | 0.696 | best -10.214 / median -9.597 (±2.037)<br>best 1.043 / median 0.921 (±0.369) | -10.000 / 1.000 | 172 |
| Holder Table | amoeba single | 10 | -19.209 | -19.209 (0.000 -> min) | -7.619 | 6.492 | best -8.055 / median -4.913 (±6.507)<br>best -9.665 / median 1.630 (±8.518) | 8.055 / 9.665<br>8.055 / -9.665<br>-8.055 / 9.665<br>-8.055 / -9.665 | 176.5 |
| Cross in tray | amoeba single | 10 | -2.063 | -2.063 (0.000 -> min) | -2.063 | 0.001 | best 1.349 / median 1.349 (±3.721)<br>best -1.349 / median -1.349 (±2.326) | 1.349 / -1.349<br>1.349 / 1.349<br>-1.349 / 1.349<br>-1.349 / -1.349 | 192 |

## Full Model Systems

### Time-series models

| Model name | Optimisation method | success % | n runs | Distance from minimum likelihood (median) | Distance from true parameter values | Milliseconds taken (median) |
| --- | --- | --- | --- | --- | --- | --- |
| predator-prey (with gaussian noise) | amoeba single | 100.000% | 10 | 2718.028 | [Δ] 0.053 best / median 0.265 (±1.094)<br>[α] 0.001 best / median 0.170 (±1.380)<br>[β] 0.088 best / median 0.314 (±0.479)<br>[γ] 0.026 best / median 0.271 (±0.227)<br>[ρ] 0.136 best / median 0.662 (±0.342)<br>[σ[x]] 0.007 best / median 0.209 (±0.249)<br>[σ[y]] 0.014 best / median 1.647 (±3755011282.332) | 311 |
| predator-prey | amoeba single | 100.000% | 10 | 6.543 | [Δ] 0.013 best / median 0.114 (±0.167)<br>[α] 0.005 best / median 0.071 (±0.171)<br>[β] 0.001 best / median 0.120 (±0.097)<br>[γ] 0.027 best / median 0.171 (±0.103) | 557 |