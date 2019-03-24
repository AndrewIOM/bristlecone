<img style="max-width:27.5em;width:100%" src="docs/files/img/logo-wide.png" alt="RProvider logo" />

An F# library for model-fitting model-selection (MFMS) of ecological models to observational data. The library was developed for tree ring analysis, but can be used for other time-series modelling problems.

[![Build status](https://ci.appveyor.com/api/projects/status/0rh5alxrdct225st?svg=true)](https://ci.appveyor.com/project/AndrewIOM/dendro-fit) [![NugetStatus](https://img.shields.io/nuget/v/Bristlecone.svg)](https://www.nuget.org/packages/Bristlecone) [![NugetStatus](https://img.shields.io/nuget/v/Bristlecone.Dendro.svg)](https://www.nuget.org/packages/Bristlecone.Dendro)

Getting Started
----------------

<img align="right" style="padding:0 0 1em 1em" src="docs/files/img/logo.png" alt="RProvider logo" />

To get started with Bristlecone, see [the documentation website with tutorials, examples and API specification](https://andrewiom.github.io/bristlecone).

In addition, in the `samples` folder there are example solutions for predator-prey population dynamics, and nutrient limitation.

A template is provided in the `template` folder. This provides a ready-made project structure to run Bristlecone analyses from F# scripts. 

**Note:** on mac/linux, .NET Core does not currently support `fsharpi`, so `mono` is required. 

Developers: Build Instructions
----------------

Bristlecone uses [FAKE](https://fake.build) for building, testing, documentation, and publishing. After cloning the repo, run ``./build.sh`` on macOS / linux, or ``build.cmd`` on windows. 

Contribute
----------------

Contributions, issues, and pull-requests are most welcome.


*Logo based on [Free Vector Art by www.vecteezy.com](https://vecteezy.com)*