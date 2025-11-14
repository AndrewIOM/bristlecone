<img style="width:27em" src="docs/img/logo-wide.png" alt="Bristlecone logo" />

---

![NuGet Downloads](https://img.shields.io/nuget/dt/Bristlecone?labelColor=%23E8E8E8&color=%238D6F64)
[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/AndrewIOM/Bristlecone/push.yml?logo=github&label=FAKE%20build%20and%20test)](https://github.com/AndrewIOM/bristlecone/actions/workflows/push.yml)

An F# library for model-fitting model-selection (MFMS) of time-series models to observational data. Included is expressive modelling system for authoring models, which includes full F# units of measure support.

Bristlecone contains specialist functionality for fitting (long-term) ecological models. The library was developed for use in dendroecology and palaeoecology, but can be used for other time-series modelling problems.

---

## NuGet 

Package | Stable | Prerelease
--- | --- | ---
Bristlecone | [![NuGet Version](https://img.shields.io/nuget/v/Bristlecone)](https://www.nuget.org/packages/bristlecone/) | [![NuGet Version](https://img.shields.io/nuget/vpre/Bristlecone)](https://www.nuget.org/packages/bristlecone/)
Bristlecone.Dendro | [![NuGet Version](https://img.shields.io/nuget/v/Bristlecone.Dendro)](https://www.nuget.org/packages/bristlecone.dendro/) | [![NuGet Version](https://img.shields.io/nuget/vpre/Bristlecone.Dendro)](https://www.nuget.org/packages/bristlecone.dendro/absoluteLatest)

---

Getting Started
----------------

<img align="right" style="padding:0 0 1em 1em" src="docs/img/logo.png" alt="Bristlecone logo" />

To get started with Bristlecone, see [the documentation website with tutorials, examples and API specification](https://andrewiom.github.io/bristlecone).

Some examples are provided on the docs page, with optional downloadable worked notebooks.

#### Benchmarks

A suite of benchmarks - covering optimisation methods and ecological time-series models - are included in the `test/Bristlecone.Benchmark` project. The most recent results of the benchmark tests [are here](benchmarks.md).


Developers: Build Instructions
----------------

Bristlecone requires the .NET 10 SDK, and uses [FAKE](https://fake.build) for building, testing, documentation, and publishing. 

Simple building and testing can be done using ``dotnet build`` or ``dotnet run test/Bristlecone.Tests/Bristlecone.Tests.fsproj`` respectively in the root directory. Docs can be tested by first calling ``dotnet tool restore`` then
``dotnet fsdocs watch --eval``, which serves them on a Suave server.

The full build process can be run using either ``dotnet fsi build.fsx``, which uses FAKE to run all build steps,
tests, and compile docs.

Contribute
----------------

Contributions, issues, and pull-requests are welcome.


*Logo based on [Free Vector Art by www.vecteezy.com](https://vecteezy.com)*
