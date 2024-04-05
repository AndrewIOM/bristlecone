<img style="width:27em" src="docs/img/logo-wide.png" alt="Bristlecone logo" />

An F# library for model-fitting model-selection (MFMS) of ecological models to observational data. The library was developed for tree ring analysis, but can be used for other time-series modelling problems.

---

## Builds

GitHub Actions |
:---: |
[![Github Actions](https://github.com/AndrewIOM/bristlecone/actions/workflows/push.yml/badge.svg?branch=master)](https://github.com/AndrewIOM/bristlecone/actions/workflows/push.yml) |

## NuGet 

Package | Stable | Prerelease
--- | --- | ---
Bristlecone | [![NuGet Badge](https://buildstats.info/nuget/bristlecone)](https://www.nuget.org/packages/bristlecone/) | [![NuGet Badge](https://buildstats.info/nuget/bristlecone?includePreReleases=true)](https://www.nuget.org/packages/bristlecone/)
Bristlecone.Dendro | [![NuGet Badge](https://buildstats.info/nuget/bristlecone.dendro)](https://www.nuget.org/packages/bristlecone.dendro/) | [![NuGet Badge](https://buildstats.info/nuget/bristlecone.dendro?includePreReleases=true)](https://www.nuget.org/packages/bristlecone.dendro/)

---

Getting Started
----------------

<img align="right" style="padding:0 0 1em 1em" src="docs/img/logo.png" alt="Bristlecone logo" />

To get started with Bristlecone, see [the documentation website with tutorials, examples and API specification](https://andrewiom.github.io/bristlecone).

In addition, in the `samples` folder there are example scripts for simple to more complex models.

#### Benchmarks

A suite of benchmarks - covering optimisation methods and ecological time-series models - are included in the `test/Bristlecone.Benchmark` project. The most recent results of the benchmark tests [are here](benchmarks.md).


Developers: Build Instructions
----------------

Bristlecone uses [FAKE](https://fake.build) for building, testing, documentation, and publishing. After cloning the repo, run ``dotnet tool restore`` then either ``dotnet fake build -t Build`` to compile only, or ``dotnet fake build -t All`` to build, test, and compile documentation.

Contribute
----------------

Contributions, issues, and pull-requests are most welcome.


*Logo based on [Free Vector Art by www.vecteezy.com](https://vecteezy.com)*
