(**
---
title: Dendroecology
category: Bristlecone.Dendro
categoryindex: 3
index: 1
---
*)
(*** condition: prepare ***)
#nowarn "211"
#r "../src/Bristlecone/bin/Debug/netstandard2.0/Bristlecone.dll"
(*** condition: fsx ***)
#if FSX
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // FSX
(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // IPYNB

(**
Bristlecone
======================

Full documentation is forthcoming.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The Bristlecone.Dendro library can be <a href="https://nuget.org/packages/Bristlecone">installed from NuGet</a>:
      <pre>$ dotnet add package Bristlecone.Dendro</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

*)