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
In this example, we use Bristlecone to fit a suite of classical non‑linear
plant‑growth models to data from a controlled growth experiment.

[Paine et al. (2012)](https://doi.org/10.1111%2Fj.2041-210X.2011.00155.x)
identify seven widely used functional forms for modelling plant biomass
through time. These models differ in their assumptions about asymptotes,
growth rates, and curvature, and are a natural testbed for demonstrating
Bristlecone’s model‑expression system and optimisation workflow.

We implement the dM/dt forms of all seven models using Bristlecone’s
declarative syntax, then fit them to the Holcus unshaded growth dataset
from Paine et al. (2012) using Nelder–Mead optimisation.

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
the `FSharp.Data.UnitSystems.SI.UnitSymbols` namespace, or declare our own.
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
parameters will be declared as `Positive`, but can also be
`NoConstraints` for unconstrained. Parameter constraints are handled
automatically by the optimisers, such that the Nelder-Mead algorithm
will run in a transformed (unbounded) parameter space.

Each model is expressed in its differential form (dM/dt),
which Bristlecone integrates automatically when fitting
continuous‑time systems.

The parameter r represents different biological processes across
the seven models (absolute growth rate, proportional growth rate,
curvature parameter, etc.), so we define rAbs and r separately
to avoid conflating meanings.
*)

module GrowthFunctions =

    // Parameters
    let K = parameter "K" Positive 20.0<gram> 40.0<gram> // upper asymptote
    let β = parameter "β" Positive 0.1 2.0
    let L = parameter "L" Positive 20.0<gram> 40.0<gram> // lower asymptote

    // The meaning of r changes between models, so define it seperately
    let rAbs = parameter "r_abs" Positive 1.1<gram/day> 1.2<gram/day>
    let r = parameter "r" Positive 0.01</day> 2.00</day>

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

Bristlecone expects time‑series to be provided in Bristlecone's `TimeSeries` type.
For ecological datasets recorded in modern calendar time, the helper
`TimeSeries.fromNeoObservations` converts (`value`, `DateTime`) pairs into a
typed time‑series that Bristlecone can align with the model’s time units.

Lastly, Bristlecone expects a `Map` of `TimeSeries` for use in the fit function.
*)

let baseDay = System.DateTime(2000, 01, 01)
let data =
    [
        mass.Code, TimeSeries.fromNeoObservations [
        0.05, baseDay.AddDays 14
        0.33, baseDay.AddDays 35
        2.43, baseDay.AddDays 70
        3.63, baseDay.AddDays 98
        5.81, baseDay.AddDays 133
        7.28, baseDay.AddDays 161
        5.87, baseDay.AddDays 189
        ]
    ]
    |> Map.ofList

(**
### Model-fitting

All seven growth models are naturally expressed as ODEs, so we use a
continuous‑time estimation engine. The call to `Bristlecone.forDailyModel`
ensures that the engine interprets the model in daily units,
matching the temporal resolution of the dataset.

We use a simple Nelder–Mead optimiser, as the models are simple with only
between one and three parameters. If we wanted increased robustness to local
optima, we could choose to run a swarm-based Nelder-Mead instead.

Plant‑growth data typically exhibit heteroscedasticity: measurement error
increases with biomass. We therefore use a Gaussian likelihood with an
exponential variance function, parameterised by σ0 and σ1.

We specify no conditioning. Here, we know that plant biomass was effectively zero at time
zero, so we do not need to condition the time-zero value; it is known. The model fitting will
therefore run from the first time-point in the data onwards.

Then, we can just run all of the hypotheses.

Each model is compiled before fitting. Compilation resolves the symbolic
model expression into an executable system of equations and prepares
the likelihood function for evaluation.
*)

module Settings =
    let engine =
        Bristlecone.mkContinuous ()
        |> Bristlecone.forDailyModel
        |> Bristlecone.withCustomOptimisation (Optimisation.Amoeba.single Optimisation.Amoeba.Solver.Default)
        |> Bristlecone.withConditioning Conditioning.NoConditioning
        |> Bristlecone.withSeed 200000

    let endCond = Optimisation.EndConditions.whenNoBestValueImprovement 100<iteration>

let sigmaBase = parameter "σ0[x]" Positive 0.01<gram> 0.5<gram>
let sigmaGrowth = parameter "σ1[x]" Positive 0.01</gram> 0.5</gram>
let variance = ModelLibrary.NegLogLikelihood.Variance.exponential sigmaBase sigmaGrowth

let results =
    GrowthFunctions.hypotheses
    |> List.map (fun h ->
            let hy =
                snd h
                |> Model.useLikelihoodFunction (ModelLibrary.NegLogLikelihood.NormalWithVariance (Require.state mass) variance)
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
The plot below overlays the observed biomass with the fitted trajectories
from each model, allowing visual comparison of model performance.
*)

Graphing.fitPlot results
|> Plotly.NET.GenericChart.toChartHTML
(*** include-it-raw ***)

(**
#### Model selection

Because this example uses a non‑Bayesian fitting approach
(maximum likelihood with Nelder–Mead optimisation), we can compare
competing growth models using constrained Akaike’s Information Criterion (AICc).  

AIC provides a balance between goodness‑of‑fit and model complexity,
and the corresponding Akaike weights give the relative support for
each model in the candidate set.
*)

let weights = ModelSelection.Akaike.akaikeWeights (results |> Seq.map snd)

(**
For more complex analyses involving multiple subjects, multiple hypotheses, or replicate fits, see the Model Selection documentation for the ResultSet‑based workflow.

The resultant weights are:
*)
weights 
|> Seq.zip (results |> Seq.map fst)
|> Seq.map(fun (m,(r,w)) ->
    sprintf "| %s | -logL: %f | AICc: %f | Weight: %f |" m r.Likelihood w.AICc w.Weight
    )
|> String.concat "\n"
(*** include-it ***)

(**
The output indicates that the best‑supported model is the gompertz growth model,
with approximately 99% support.

The reason that the gompertz is identified as the best model, despite the fit not
looking as good by eye as the three-parameter logistic form, is because the likelihood
function with exponential variance penalises errors at larger biomasses less heavily.

We can estimate the implied sigma for each observation, for example for the
gompertz fit:
*)

let sigmaAt (expected: float<gram>) =
    let s0 = (snd results.[6]).Parameters.Get sigmaBase
    let s1 = (snd results.[6]).Parameters.Get sigmaGrowth
    s0 * exp (s1 * expected)

let diagnostics =
    (snd results.[6]).Series
    |> Map.find mass.Code
    |> fun ts -> ts.Values
    |> Seq.map (fun d ->
        let fitted = d.Fit
        let observed = d.Obs
        let sigma = sigmaAt (fitted * 1.<gram/ModelSystem.state>)
        sprintf "Fit = %f, Obs = %f, sigma = %f" fitted observed sigma
    )
    |> Seq.toList

diagnostics
(*** include-it ***)

(**
The sigma values are very tight for the first three time points for the gompertz
fit, as shown above. Other models fail to predict the earlier biomasses with such
closeness, so are heavily penalised as a result.

#### Model fit quality / uncertainty

For maximum‑likelihood fits, Bristlecone supports profile‑likelihood confidence intervals for individual parameters.
Profile likelihoods are robust to non‑linearity and asymmetry in the likelihood surface, making them preferable to approximate Hessian‑based intervals for ecological models.

Below is an example of how to compute a profile likelihood for the parameters of a fitted model.
*)

let likelihoodProfile =
    results
    |> Seq.take 1 // just run the first only in this example
    |> Seq.map(fun (name,r) ->

        // Lookup the original hypothesis model using its name:
        let h =
            GrowthFunctions.hypotheses |> Seq.find (fun s -> fst s = name)
            |> snd
            |> Model.useLikelihoodFunction (ModelLibrary.NegLogLikelihood.NormalWithVariance (Require.state mass) variance)
            |> Model.compile

        // Run a profile likelihood around the Maximum Likelihood Estimate:
        let l = 
            Bristlecone.Confidence.ProfileLikelihood.profile
                Bristlecone.fit
                Settings.engine
                data
                h
                100<iteration>
                r
        name, l )
    |> Seq.toList

(**
The likelihood profile returns a 68% and 95% interval for each
parameter. We can inspect them like so:
*)

likelihoodProfile.Head
|> snd
|> Seq.map(fun kv -> sprintf "[%s] 95 CI %f - %f (estimate = %f)" kv.Key.Value kv.Value.``95%``.Lower kv.Value.``95%``.Upper kv.Value.Estimate )
