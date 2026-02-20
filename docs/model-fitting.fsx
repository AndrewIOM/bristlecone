(**
---
title: Model Fitting
category: Techniques
categoryindex: 1
index: 2
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
Model Fitting
========================


### The Estimation Result

Model-fitting returns an `EstimationResult`.

#### Accessing estimated parameter values

To access parameter values from an `EstimationResult`, you may use the
`.Get` or `.TryGet` members to retrieve parameter estimates in their
original units of measure. You must pass the original parameter as
declared in the model to access individual values in this way.
*)
