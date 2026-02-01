(**
---
title: Simple plant growth models
category: Examples
categoryindex: 3
index: 1
---

[![Script]({{root}}/img/badge-script.svg)]({{root}}/{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}/img/badge-notebook.svg)]({{root}}/{{fsdocs-source-basename}}.ipynb)
*)

(**
In this example, we use Bristlecone to fit non-linear models of plant growth
to some data from a plant growth experiment.

In the paper *How to fit nonlinear plant growth models and calculate  growth rates: an update for ecologists*,
[Paine et al (2012)](https://doi.org/10.1111%2Fj.2041-210X.2011.00155.x) explain that there are
seven key functional forms that can be used to represent plant biomass growth over time.

Here, we write the *dM/dt* forms of the seven equations in the Bristlecone model expression
system, then fit these using Nelder-Mead (amoeba) optimisation to some sample plant growth
data. The data is the Holcus unshaded growth experiment data as shown in the figures
of Paine et al (2012).

First, we open Bristlecone core libraries.
*)

(*** condition: prepare ***)
#r "nuget: DiffSharp-cpu, v=1.0.7"
#r "nuget: MathNet.Numerics.FSharp,5.0.0"
#r "nuget: FSharp.Data,6.6"
#r "../../src/Bristlecone/bin/Debug/net10.0/Bristlecone.dll"
#r "../../src/Bristlecone.Dendro/bin/Debug/net10.0/Bristlecone.Dendro.dll"
#r "nuget: Plotly.NET, 4.2.0"

open Bristlecone
open Bristlecone.Language
open Bristlecone.Time

(**
### Defining the models

For units of measure, we can choose to use built-in F# units from
the FSharp.Data.UnitSystems.SI.UnitSymbols namespace, or declare our own.
Similarly, time-based units (days, months, years) are available in the
Bristlecone.Time module. Here, we use the time units from Bristlecone,
and declare grams as a new unit type.
*)

[<Measure>] type gram

let mass = state<gram> "mass"

(**
For the growth functions, there are five different parameters
across the seven models, which we declare. To declare parameters,
a reasonable starting bound and a constraint are required. Often,
parameters will be declared as `notNegative`, but can also be
`noConstraints` for unconstrained. Parameter constraints are handled
automatically by the optimisers, such that the Nelder-Mead algorithm
will run in a transformed (unbounded) parameter space.
*)

module GrowthFunctions =

    // Parameters
    let K = parameter "K" notNegative 20.0<gram> 40.0<gram> // upper asymptote
    let β = parameter "β" notNegative 0.1 2.0
    let L = parameter "L" notNegative 20.0<gram> 40.0<gram> // lower asymptote

    // The meaning of r changes between models, so define it seperately
    let rAbs = parameter "r_abs" notNegative 1.1<gram/day> 1.2<gram/day>
    let r = parameter "r" notNegative 0.01</day> 2.00</day>

    // 7x typical model forms (ODEs)
    let linear = P rAbs
    let exponential = P r * This<gram>
    let powerLaw = P r * This<gram> ** P β
    let monomolecular = P r * (P K - This<gram>)
    let logistic = P r * This<gram> * (Constant 1. - This / P K)
    let logisticFour = P r * (This<gram> - P L) * ((P K - This) / (P K - P L))
    let gompertz = P r * This<gram> * Logarithm (P K / This)

    // Scaffold into a list of plausable hypotheses:        
    let hypotheses =
        let mkModel ode = Model.empty |> Model.addRateEquation mass ode
        [
            "linear", mkModel linear |> Model.estimateParameter rAbs
            "exponential", mkModel exponential |> Model.estimateParameter r
            "power", mkModel powerLaw |> Model.estimateParameter r |> Model.estimateParameter β
            "monomolecular", mkModel monomolecular |> Model.estimateParameter r |> Model.estimateParameter K
            "logistic (3-par)", mkModel logistic |> Model.estimateParameter r |> Model.estimateParameter K
            "logistic (4-par)", mkModel logisticFour |> Model.estimateParameter r |> Model.estimateParameter K |> Model.estimateParameter L
            "gompertz", mkModel gompertz |> Model.estimateParameter r |> Model.estimateParameter K
        ]

(**
### Load the dataset

Here, to keep the example simple we have defined the
data in code, but otherwise you may load data in as you see fit.

Bristlecone expects time-series to be formatted into Bristlecone's
`TimeSeries` type. For neo-ecological data, the `TimeSeries.fromNeoObservations`
function may be used to work in .NET DateTime space. In this case, we do this
by defining a basal date to pin the time-series to, then pass the list of tuples
of data value and date to the Bristlecone function.

Lastly, Bristlecone expects a `Map` of `TimeSeries` for use in the fit function.
*)

let baseDay = System.DateTime(2000, 01, 01)
let data =
    [
        mass.Code, TimeSeries.fromNeoObservations [
        0.07, baseDay.AddDays 14
        0.44, baseDay.AddDays 35
        2.83, baseDay.AddDays 70
        4.47, baseDay.AddDays 98
        8.39, baseDay.AddDays 133
        9.00, baseDay.AddDays 161
        6.13, baseDay.AddDays 189
        ]
    ]
    |> Map.ofList

(**
### Model-fitting

We need to define some basic settings to run the model-fitting. The
estimation engine represents a common fixed method used to fit a model.
The engine specified below uses the default continuous-time engine (`Bristlecone.mkContinuous`) with
some minor changes.

The engine needs to explicitly know about the temporal resolution of the models that
will be applied. Here, we call `Bristlecone.forDailyModel` to make the engine suitable
for use against daily-resolution models.

We specify to use the swarm-based Nelder-Mead algorithm for optimisation. This is more
robust than a single simplex, as it runs ten simplex at five levels, dropping the worst
results and resetting the starting bounds at each level until no improvements are made.

We specify no conditioning. Here, we know that plant biomass was effectively zero at time
zero, so we do not need to condition the time-zero value; it is known. The model fitting will
therefore run from the first time-point in the data onwards.

For the -log likelihood function, we are applying a standard gaussian function
from the built-in selection in the `Bristlecone.ModelLibrary`. However, for
plant growth models we likely should be using a gaussian function with heteroscedacity,
because the associated error may increase in magnitude as biomass increases.
We also therefore need to declare the sigma parameter required by the gaussian function.

Then, we can just run all of the hypotheses.
*)

module Settings =
    let engine =
        Bristlecone.mkContinuous ()
        |> Bristlecone.forDailyModel
        |> Bristlecone.withCustomOptimisation (Optimisation.Amoeba.swarm 5 10 Optimisation.Amoeba.Solver.Default)
        |> Bristlecone.withConditioning Conditioning.NoConditioning

    let endCond = Optimisation.EndConditions.noImprovementRecently 100<iteration>

let sigma = parameter "σ[x]" notNegative 0.01 0.5

let results =
    GrowthFunctions.hypotheses
    |> List.map (fun h ->
            let hy =
                snd h
                |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.gaussian (Require.state mass))
                |> Model.estimateParameter sigma
                |> Model.compile
            fst h, Bristlecone.fit Settings.engine Settings.endCond data hy
    )

(*** hide ***)
module Graphing =

    open Plotly.NET

    let fitPlot (r: (string * ModelSystem.EstimationResult<System.DateTime,int<year>,System.TimeSpan>) seq) =        
        
        // One plot per variable with the data and each model fit on top, by name.
        let variables =
            r
            |> Seq.collect(fun (name, res) -> res.Series |> Seq.map(fun kv -> kv.Key, (name, kv.Value)))
            |> Seq.groupBy fst            

        let charts =
            variables
            |> Seq.map(fun (varCode, modelFits) ->

                let obsLine =
                    modelFits
                    |> Seq.head |> snd |> snd
                    |> Bristlecone.Time.TimeSeries.toObservations
                    |> Seq.map (fun (d, v) -> v, d.Obs)
                    |> Seq.toList
                    |> fun l -> Chart.Point(xy = l, Name = "Observed")

                let lines =
                    modelFits
                    |> Seq.map snd
                    |> Seq.map(fun (modelName, fit) ->
                        fit
                        |> Bristlecone.Time.TimeSeries.toObservations
                        |> Seq.collect (fun (d, v) -> [ v, "Modelled", d.Fit; v, "Observed", d.Obs ])
                        |> Seq.groupBy (fun (_, x, _) -> x)
                        |> Seq.map (fun (_, s) -> s |> Seq.map (fun (x, _, y) -> x, y))
                        |> Seq.toList
                        |> fun l -> Chart.Line(xy = l.Head, Name = modelName)
                    )
                lines
                |> Seq.append [ obsLine ]
                |> Chart.combine
                |> Chart.withTitle varCode.Value
                )

        charts |> Chart.Grid(1, 1)

(**
The resultant model fits are shown in the below graph.
*)

Graphing.fitPlot results
|>Plotly.NET.Chart.show
|> Plotly.NET.GenericChart.toChartHTML
(*** include-it-raw ***)

(**
#### Model selection

As we are applying a non-bayesian model fitting approach in this example, we
can apply Akaike weights to discren which is the most appropriate model representation
to explain this data.

A weights function exists for simple lists of competing hypotheses for a single subject,
which in this case is the single plant biomass time-series.

For more complex sets of results, use the results set functionality described
in the model selection documentation; this takes into account the subjects, hypotheses,
and one or more replicates for each.
*)

let weights = ModelSelection.Akaike.akaikeWeights (results |> Seq.map snd)

(**
The resultant weights are:

| Model | MLE | AICc | Weight |
| ---   | --- | ---  | ---    |
*)
weights 
|> Seq.zip (results |> Seq.map fst)
|> Seq.map(fun (m,(r,w)) ->
    sprintf "| %s | %f | %f | %f |" m r.Likelihood w.AICc w.Weight
    )
|> String.concat "\n"
(*** include-it-raw ***)

(**
The output indicates that the best model is linear, with approximately 72% support
(this may vary as the above table is auto-generated).

#### Model fit quality / uncertainty

...
*)

let likelihoodProfile =
    [results.[0]]
    |> Seq.map(fun (name,r) ->

        // Lookup the original hypothesis model using its name:
        let h =
            GrowthFunctions.hypotheses |> Seq.find (fun s -> fst s = name)
            |> snd
            |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.gaussian (Require.state mass))
            |> Model.estimateParameter sigma
            |> Model.compile

        // Run a profile likelihood around the Maximum Likelihood Estimate:
        let l = 
            Bristlecone.Confidence.ProfileLikelihood.profile
                Bristlecone.fit
                Settings.engine
                data
                h
                1000<iteration>
                r
        name, l )
    |> Seq.toList


let sigmaBase = parameter "σ0[x]" notNegative 0.01 0.5
let sigmaGrowth = parameter "σ1[x]" notNegative 0.01 0.5

let results =
    GrowthFunctions.hypotheses
    |> List.map (fun h ->
            let hy =
                snd h
                |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.gaussianWithExponentialVariance (Require.state mass))
                |> Model.estimateParameter sigmaBase
                |> Model.estimateParameter sigmaGrowth
                |> Model.compile
            fst h, Bristlecone.fit Settings.engine Settings.endCond data hy
    )
