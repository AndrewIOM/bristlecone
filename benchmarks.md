
Bristlecone Benchmarks
===

## Optimisation Methods

| Model name | Optimisation method | n runs | Target minimum | Best estimated minimum | Median estimated minimum | Sum distance from true parameters | Parameter estimates (individual) | True solutions | Milliseconds taken (median) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Ackley (2D) | filzbach | 10 | 0.000 | 0.004 (0.004 -> min) | 0.016 | 0.007 | best 0.001 / median -0.002 (±0.004)<br>best 0.001 / median -0.003 (±0.004) | 0.000 / 0.000 | 615 |
| Bukin Sixth | filzbach | 10 | 0.000 | 0.082 (0.082 -> min) | 0.133 | 4.751 | best -12.835 / median -9.312 (±4.882)<br>best 1.647 / median 0.871 (±0.819) | -10.000 / 1.000 | 77.5 |
| Griewank | filzbach | 10 | 0.000 | 0.003 (0.003 -> min) | 0.011 | 7.527 | best 0.079 / median -1.495 (±4.627)<br>best -0.017 / median -4.353 (±3.744) | 0.000 / 0.000 | 797 |
| Holder Table | filzbach | 10 | -19.209 | -19.208 (0.000 -> min) | -19.208 | 0.004 | best -8.056 / median -8.055 (±7.781)<br>best -9.664 / median -9.664 (±9.982) | 8.055 / 9.665<br>8.055 / -9.665<br>-8.055 / 9.665<br>-8.055 / -9.665 | 555 |
| Cross in tray | filzbach | 10 | -2.063 | -2.063 (0.000 -> min) | -2.063 | 0.014 | best 1.349 / median 1.342 (±1.396)<br>best 1.349 / median 1.343 (±1.134) | 1.349 / -1.349<br>1.349 / 1.349<br>-1.349 / 1.349<br>-1.349 / -1.349 | 821.5 |
| Dropwave | filzbach | 10 | -1.000 | -0.999 (0.001 -> min) | -0.997 | 0.011 | best 0.003 / median -0.004 (±0.010)<br>best -0.005 / median -0.002 (±0.007) | 0.000 / 0.000 | 793.5 |
| Eggholder | filzbach | 10 | -959.641 | -959.629 (0.012 -> min) | -786.526 | 1120.229 | best 511.997 / median 283.079 (±432.869)<br>best 404.252 / median -382.603 (±439.438) | 512.000 / 404.232 | 612.5 |
| Gramarcy-Lee | filzbach | 10 | -0.869 | -0.869 (0.000 -> min) | -0.869 | 0.000 | best 0.549 / median 0.549 (±0.000) | 0.549 | 296 |
| Langermann | filzbach | 10 | -5.162 | -5.162 (0.000 -> min) | -5.162 | 0.003 | best 2.003 / median 2.004 (±0.003)<br>best 1.007 / median 1.005 (±0.002) | 2.003 / 1.006 | 666 |

## Full Model Systems

### Time-series models

| Model | Optimisation method | Success % | n | Likelihood gap (med, tol=1.0) | Param RMSE (rel, median) | n iterations (median) | Time to solution (ms, median) |
| --- | --- | --- | --- | --- | --- | --- | --- |
| predator-prey (noisy) | filzbach | 0.000% | 10 | 308.479 | 21.707 | 101002 | 726 |