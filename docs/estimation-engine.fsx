(**
---
title: Defining a Model-Fitting Method (Estimation Engine)
category: The Bristlecone Language
categoryindex: 1
index: 3
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
Defining an Estimation Engine
========================

The *estimation engine* is the definition of the method
through which model-fitting will take place, as defined
by Bristlecone's `EstimationEngine` type. If you are
competing multiple hypotheses, a single `EstimationEngine`
will suffice for all model hypotheses. 

Forthcoming

*)