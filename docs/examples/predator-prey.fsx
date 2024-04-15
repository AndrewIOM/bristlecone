(**
---
title: Predator-Prey Dynamics
category: Examples
categoryindex: 1
index: 1
---

[![Script](img/badge-script.svg)]({{root}}/{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook](img/badge-notebook.svg)]({{root}}/{{fsdocs-source-basename}}.ipynb)
*)

(*** condition: prepare ***)
#nowarn "211"
#r "nuget:MathNet.Numerics.FSharp,4.15"
#r "../../src/Bristlecone/bin/Release/netstandard2.0/Bristlecone.dll"
(*** condition: fsx ***)
#if FSX
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // FSX
(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // IPYNB

#r "nuget:RProvider"

(**
Predator-Prey Dynamics: Snowshoe Hare and Lynx
---

Here we use the classic example of snowshoe hare and lynx predator-prey dynamics, 
to demonstrate the basic functions of Bristlecone. The dataset is a 90-year 
time-series of snowshoe hare and lynx pelts purchased by the 
Hudson's Bay Company of Canada. Data is in 1000s. 

To get started, we first load and open the Bristlecone library in
an F# script file (.fsx):
*)

open Bristlecone            // Opens Bristlecone core library and estimation engine
open Bristlecone.Language   // Open the language for writing Bristlecone models

(**### Defining the ecological model

In Bristlecone, a single ecological model (representing a single hypothesis)
is defined through the `ModelSystem` type. A `ModelSystem` needs to include three key
components:

* **Model equations.** When working in continuous time, these are a system of Ordinary Differential Equations (ODEs).
* **Parameters to be estimated.** You must specify the starting bounds and constraints for every parameter included in the model equations.
* **Likelihood function**. The (negative log) likelihood function *-logL* represents the probability of observing the data given the parameter set. We use a negative log likelihood function, which is then minimised during optimisation.

In this example, we demonstrate using the *Lotka–Volterra* predator–prey model as the
model system. For the -logL function we use a bivariate normal negative log likelihood function.
This -logL function assumes normally-distributed observation error around each observation
at each time-point, for both the lynx and hare data. The -logL function contains
three parameters that are to be estimated alongside the deterministic model: the variability
in lynx data, the variability in hare data, and their covariance.
*)

let ``predator-prey`` =

    let ``dh/dt`` = Parameter "α" * This - Parameter "β" * This * Environment "lynx"
    let ``dl/dt`` = - Parameter "γ" * This + Parameter "Δ" * Environment "hare" * This

    Model.empty
    |> Model.addEquation       "hare"   ``dh/dt``
    |> Model.addEquation       "lynx"   ``dl/dt``

    |> Model.estimateParameter "α"      noConstraints 0.01 1.00    // Natural growth rate of hares in absence of predation
    |> Model.estimateParameter "β"      noConstraints 0.01 1.00    // Death rate per encounter of hares due to predation
    |> Model.estimateParameter "Δ"      noConstraints 0.01 0.20    // Efficiency of turning predated hares into lynx
    |> Model.estimateParameter "γ"      noConstraints 0.01 0.20    // Natural death rate of lynx in the absence of food

    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares ["hare"; "lynx"])
    |> Model.estimateParameter  "ρ"     noConstraints -0.500 0.500
    |> Model.estimateParameter  "σ[x]"  notNegative 0.001 0.100
    |> Model.estimateParameter  "σ[y]"  notNegative 0.001 0.100

    |> Model.compile

    // TODO Error when setting constraints. Optim allows them to go negative!

(**
### Setting up the *Bristlecone Engine*

A bristlecone engine provides a fixed setup for estimating parameters from data.
Use the same engine for all model fits within a single study.
This engine uses a gradident descent method (Nelder Mead simplex), and a basic
Runge-Kutta 4 integration method provided by MathNet Numerics.
*)

let engine = 
    Bristlecone.mkContinuous
    |> Bristlecone.withTunedMCMC []
    |> Bristlecone.withContinuousTime Integration.MathNet.integrate
    |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint

(**
### Does it all work? Testing the engine and model

Before being confident in the ability of our estimation engine to
be able to arrive at the correct solution, we must run a full test
of the model and estimation engine.

Bristlecone includes the ``Bristlecone.testModel`` function, which
we use here. Given a model system and estimation engine, the function
generates a random parameter set (*θ*) many times; for each *θ*, the
'true' time-series are generated. The test result indicates the effectiveness
of the configuration at estimating *θ* given the auto-generated
time-series. If there is divergence, there is likely an
issue with your model or the Bristlecone configuration.

Bristlecone includes many settings to configure the test
procedure. A simple test configuration is set as `Test.defaultSettings`,
but here we will configure some additional settings:
*)

let testSettings =
    Test.create
    |> Test.addStartValues [
        "hare", 50.
        "lynx", 75. ]
    // |> Test.addNoise (Test.Noise.tryAddNormal "σ[y]" "lynx")
    // |> Test.addNoise (Test.Noise.tryAddNormal "σ[x]" "hare")
    |> Test.addGenerationRules [
        Test.GenerationRules.alwaysLessThan 10000. "lynx"
        Test.GenerationRules.alwaysLessThan 10000. "hare" ]
    |> Test.withTimeSeriesLength 30
    |> Test.endWhen (Optimisation.EndConditions.afterIteration 1)

(**
In our `TestSettings`, we have specified the initial time point (t = 0)
for both modelled time-series. We have also added noise around
each generated time-series, and specified that each time-series
should be 30 years in length.

With these test settings, we can now run the test.
*)

let testResult =
    ``predator-prey`` 
    |> Bristlecone.testModel engine testSettings
(*** include-fsi-merged-output ***)

(**
We can check the test settings by...
*)

module Graphing =

    // Testing out ggplot for graphics
    open RProvider
    open RProvider.Operators
    open RProvider.ggplot2
    open RProvider.svglite
    open RDotNet

    let (++) (a:SymbolicExpression) b = R.``+``([ a; b ])

    let xyPlot (testResult:Result<Bristlecone.Test.TestResult,string>) =
        
        let data =
            match testResult with
            | Ok r ->
                r.Series
                |> Seq.collect(fun kv ->
                    kv.Value
                    |> Bristlecone.Time.TimeSeries.toObservations
                    |> Seq.map(fun (d,v) -> kv.Key, v, d.Fit, d.Obs))
            | Error _ -> []

        let df = R.data_frame([
            "variable" => (data |> Seq.map(fun (a,_,_,_) -> a))
            "time" => (data |> Seq.map(fun (_,b,_,_) -> b))
            "modelled" => (data |> Seq.map(fun (_,_,c,_) -> c))
            "observed" => (data |> Seq.map(fun (_,_,_,d) -> d))
        ])

        let s = R.svgstring()

        R.plot([ "x" => df?time; "y" => df?observed ])

        // R.ggplot()
        //     ++ R.geom__line(R.aes(["x" => df?time, "y" => df?modelled, "group" => df?variable]))
        //     ++ R.geom__line(R.aes(["x" => df?time, "y" => df?observed, "group" => df?variable]))
        //     |> ignore

        let html = s.AsFunction().Invoke().GetValue<string>()
        RProvider.grDevices.R.dev_off() |> ignore
        html

let html = Graphing.xyPlot testResult
(*** include-it-raw ***)

printfn "%s" html

// // 3. Load in Real Data
// // ----------------------------
// // Here, we are using the FSharp.Data type provider to read in a CSV file.

// type PopulationData = FSharp.Data.CsvProvider<"/Users/andrewmartin/Documents/GitHub Projects/bristlecone/docs/examples/data/lynx-hare.csv">
// let data = 
//     let csv = PopulationData.Load "/Users/andrewmartin/Documents/GitHub Projects/bristlecone/docs/examples/data/lynx-hare.csv"
//     [ (code "hare").Value, Time.TimeSeries.fromObservations (csv.Rows |> Seq.map(fun r -> float r.Hare, r.Year))
//       (code "lynx").Value, Time.TimeSeries.fromObservations (csv.Rows |> Seq.map(fun r -> float r.Lynx, r.Year)) ] |> Map.ofList


// // 0. Configure Options
// // ----------------------------

// module Options =
//     let iterations = 100000


// // 4. Fit Model to Real Data
// // -----------------------------------
// let result = ``predator-prey`` |> Bristlecone.fit engine (Optimisation.EndConditions.afterIteration Options.iterations) data

