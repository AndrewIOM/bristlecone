(**
---
title: The long-term ecological detective: Holocene ecosystem functioning
category: Examples
categoryindex: 3
index: 1
---

[![Script]({{root}}/img/badge-script.svg)]({{root}}/{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}/img/badge-notebook.svg)]({{root}}/{{fsdocs-source-basename}}.ipynb)
*)

(*** condition: prepare ***)
#nowarn "211"
#r "nuget: DiffSharp-cpu, v=1.0.7"
#r "nuget: MathNet.Numerics.FSharp,5.0.0"
#r "nuget: FSharp.Data,6.6"
#r "../../src/Bristlecone/bin/Debug/net10.0/Bristlecone.dll"

#r "nuget: Plotly.NET, 4.2.0"

(**
The long-term ecological detective: Holocene ecosystem functioning
---

Analysis of microfossils and inorganic compounds from lake sediment
cores and peat cores can provide rich information on past biodiversity
and environmental conditions.

[Jeffers et al (2011)](https://nph.onlinelibrary.wiley.com/doi/full/10.1111/j.1469-8137.2011.03907.x)
identified ...

To get started, we first load and open the Bristlecone library in
an F# script file (.fsx):
*)

open Bristlecone
open Bristlecone.Language
open Bristlecone.Time

(**
Then, we may define units of measure that are not SI units (which are included with F#)
and are not provided in the Time module of Bristlecone.
*)

[<Measure>] type area
[<Measure>] type indiv // pollen accumulation proxy units -> individuals per area
[<Measure>] type conc  // nitrogen concentration proxy units (e.g., δ15N or %TN, scaled)
[<Measure>] type d15N // proxy measure of nitrogen
[<Measure>] type grain
[<Measure>] type cm

(**
We can then use these units of measure when defining the model system.
Before we can write model equations, we need to define the states, parameters,
and measures that we need to apply within them.
*)

// States
let N = state<conc> "available_N"         // Reconstructed available N proxy (δ15N or %TN scaled)
let X = state<indiv / area> "population"  // Pollen accumulation proxy for population density
let obsN = measure "observed_N" // After conversion with an alpha conversion factor
let obsPAR = measure<(grain / cm^2) / year> "observed_PAR"

// Core ecological parameters
let λ = parameter "λ" notNegative 0.01<conc/year> 1.0<conc/year> // External N input
let γN = parameter "γ[N]" notNegative 0.001<1/year> 0.1<1/year> // N loss rate
let r   = parameter "r" notNegative 0.001<(indiv/area)/conc> 20.<(indiv/area)/conc> // Intrinsic growth rate
let γX = parameter "γ[X]" notNegative 0.01<1/year> 0.2<1/year> // mortality

// Measurement model parameters
let αδ15N = parameter<d15N> "αδ15N" noConstraints -2.0<d15N> 2.0<d15N>   // intercept for δ15N proxy
let βδ15N = parameter<d15N/conc> "βδ15N" noConstraints 0.1<d15N/conc> 2.0<d15N/conc>    // slope linking latent N to δ15N

// Likelihood function parameters
let ρ = parameter "ρ" noConstraints -0.500 0.500
let σx = parameter "σ[x]" notNegative 10. 50.
let σy = parameter "σ[y]" notNegative 0.001 0.100

(**
In this scenario, we will assess eight hypotheses that relate to the form of
dependency between an individual plant taxon and nitrogen availability. To
scaffold the hypotheses, we first define a base model that contains the two
ordinary differential equations for the two state variables; it takes three
pluggable model components as arguments using a standard F# function definition.
*)

let baseModel
    (uptake  : ModelExpression<conc> -> ModelExpression<indiv/area> -> ModelExpression<conc/year> * ModelExpression<1>)
    (feedback: ModelExpression<indiv/area> -> ModelExpression<conc/year>)
    (density : ModelExpression<indiv/area> -> ModelExpression<1>) =

    // ODEs
    let ``dN/dt``: ModelExpression<conc/year> =
        let uptake, uptakeMult = uptake This (State X)
        P λ
        - uptake * uptakeMult
        - P γN * This<conc>
        + feedback(State X)

    let ``dX/dt``: ModelExpression<(indiv / area)/year> =
        let uptake, _ = uptake (State N) This
        P r * uptake * density This - P γX * This

    // Measure: convert modeled N to proxy comparison scale
    let nToProxy : ModelExpression<d15N> = P αδ15N + P βδ15N * State N
    let nFromProxy : ModelExpression<conc> = (Measure obsN - P αδ15N) / P βδ15N

    Model.empty
    // Add the core ecological ODEs:
    |> Model.addRateEquation X ``dX/dt``
    |> Model.addRateEquation N ``dN/dt``
    |> Model.estimateParameter λ
    |> Model.estimateParameter γN
    |> Model.estimateParameter r
    |> Model.estimateParameter γX
    // Add the conversion from d15N to N availability:
    |> Model.addMeasure obsN nToProxy
    |> Model.initialiseHiddenStateWith N nFromProxy
    |> Model.estimateParameter αδ15N
    |> Model.estimateParameter βδ15N
    // Add the likelihood function:
    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.bivariateGaussian (Require.state X) (Require.measure obsN))
    |> Model.estimateParameter ρ
    |> Model.estimateParameter σx
    |> Model.estimateParameter σy
    

(**
In this model, we naively assume that the pollen accumulation rate approximates the individuals of the
plant taxon per unit area. However, for nitrogen the use of a measurement model is desirable, at a minimum
because raw d15N (permil) does not 

Next, we define the three interchangable components that we will plug in. The models as stated in
Jeffers (2011) are not truly nested, as some combinations are not ecologically plausable. In this
instance, we define two sets of model components; one for N-dependent systems, and one for
N-independent systems. The two sets are encoded in a record for each of the three modes below.

#### Feedback component.

A plant-soil feedback may be enabled or disabled, and is only defined in one
mathematical form.
*)

let feedbackMode =

    // Conversion factor (individuals into N)
    let σ  = parameter "α" notNegative 0.01<conc/(indiv/area)> 10.<conc/(indiv/area)>

    let none _ = Constant 0.<conc/year>
    let positive (X: ModelExpression<indiv/area>) = P σ * P γX * X

    {|
        NDependent =
            Components.modelComponent "Feedback" [
                Components.subComponent "None" none
                Components.subComponent "Positive feedback" positive
                |> Components.estimateParameter σ
            ]
        NIndependent =
            Components.modelComponent "Feedback" [
                Components.subComponent "Positive feedback" positive
                |> Components.estimateParameter σ
            ]
    |}


(**
#### N-dependency

We define the N-dependency ("uptake") as a tuple, where the first element
is the form of uptake, and the second is a multiplier used to turn on or
off the uptake term within the ``dN/dt`` equation. This is required because
when growth is N-independent, the growth term in ``dX/dt`` must be multipled
by 1, whereas uptake must be turned off. Three forms of N-dependency are
specified: N-independent, linear N-dependency, and saturating N-dependency.
*)

let uptakeMode =

    // Parameters:
    let a = parameter "a" notNegative 1e-6<area/(indiv year)> 1e-3<area/(indiv year)> // Uptake rate constant
    let b = parameter "b" notNegative 0.5</conc> 2.0</conc> // Half-saturation (MM)

    // If independent, need to substitute 1 into the growth equation instead of 0.
    let independent _ _ =
        Constant 1.<conc/year>, Constant 0.

    // tuple of (uptake rate (0 if none), uptake multiplier)
    let linear (N: ModelExpression<conc>) (X: ModelExpression<indiv/area>) = P a * N * X, Constant 1.
    let michaelisMenten (N: ModelExpression<conc>) (X: ModelExpression<indiv/area>) : ModelExpression<conc/year> * ModelExpression<1> =
        (P a * N * X) / (Constant 1. + (P b * N)), Constant 1.

    {|
        NDependent =
            Components.modelComponent "Uptake" [
                Components.subComponent "Linear" linear
                |> Components.estimateParameter a
                Components.subComponent "Saturating (Michaelis-Menten)" michaelisMenten
                |> Components.estimateParameter a
                |> Components.estimateParameter b
            ]
        NIndependent =
            Components.modelComponent "Uptake" [
                Components.subComponent "Independent" independent
            ]
    |}

(**
#### Density-dependency

...
*)

let densityMode =

    let K   = parameter "K_growth"   notNegative 1.<indiv/area> 1e6<indiv/area>
    let c  = parameter "c_density"  notNegative 0.01<area/indiv> 1e-3<area/indiv>

    let logisticDD (X: ModelExpression<indiv/area>) : ModelExpression<1> =
        Constant 1.<1> - X / P K

    let expDD (X: ModelExpression<indiv/area>) : ModelExpression<1> =
        Exponential (-(P c) * X)

    let constant (_: ModelExpression<indiv/area>) : ModelExpression<1> =
        Constant 1.<1>

    {|
        NDepenentGrowth =
            Components.modelComponent "Density" [
                Components.subComponent "Constant" constant
                Components.subComponent "Logistic" logisticDD
                |> Components.estimateParameter K
            ]
        NIndependentGrowth =
            Components.modelComponent "Density" [
                Components.subComponent "Logistic" logisticDD
                |> Components.estimateParameter K
                Components.subComponent "ExpDecay" expDD
                |> Components.estimateParameter c
            ]
    |}


(**
Finally, we can scaffold all of the possible model-component combinations
into a set of hypotheses that represents the product of all of the
components.

The system is not truly nested, so we create two nested sets
and concatenate them together.
*)

let hypothesesDependent =
    Hypotheses.createFromModel baseModel
    |> Hypotheses.apply uptakeMode.NDependent
    |> Hypotheses.apply feedbackMode.NDependent
    |> Hypotheses.apply densityMode.NDepenentGrowth
    |> Hypotheses.compile

let hypothesesIndependent =
    Hypotheses.createFromModel baseModel
    |> Hypotheses.apply uptakeMode.NIndependent
    |> Hypotheses.apply feedbackMode.NIndependent
    |> Hypotheses.apply densityMode.NIndependentGrowth
    |> Hypotheses.compile

let hypotheses =
    List.append hypothesesDependent hypothesesIndependent

(**
The resulting list of hypotheses mirrors the 10 nitrogen-based models
presented in Table 1 of Jeffers et al (2011). Bristlecone assigns each
model a code based on the components within it. For our 10 models that
vary uptake mechanism (UP), feedback (FE), and density-dependence (DE), they are:
*)

for h in hypotheses do
    printfn "%s" h.ReferenceCode
(*** include-output ***)

(**

### Fitting the models

We can fit the ecological models to data by defining an estimation engine
that contains the method that will be applied for model fitting:
*)

let engine =
    Bristlecone.mkContinuous ()
    |> Bristlecone.withCustomOptimisation ( Optimisation.MonteCarlo.Filzbach.filzbach
           { Optimisation.MonteCarlo.Filzbach.FilzbachSettings.Default with BurnLength = Optimisation.EndConditions.atIteration 200000<iteration> })
    |> Bristlecone.withConditioning Conditioning.NoConditioning
    |> Bristlecone.withSeed 1500 // We are setting a seed for this example - see below
    |> Bristlecone.withTimeConversion DateMode.Conversion.RadiocarbonDates.toYears

let endCond = Optimisation.EndConditions.atIteration 100000<iteration>


(**
Next, we must load in some real data. We leverage FSharp.Data here to read in
a csv file containing the raw data.
*)

open FSharp.Data

type PalaeoData = CsvProvider<"data/loch-dubhan/ld.tsv">

let data = PalaeoData.Load "data/loch-dubhan/ld.tsv"

let ts =
    [
        X.Code, data.Rows |> Seq.map(fun r -> float r.Par_betula, DatingMethods.Radiocarbon (float r.``Scaled_age_cumulative (cal yr bp)`` * 1.<Time.``cal yr BP``>)) |> TimeSeries.fromCalibratedRadiocarbonObservations
        obsN.Code, data.Rows |> Seq.map(fun r -> float r.D15N, DatingMethods.Radiocarbon (float r.``Scaled_age_cumulative (cal yr bp)`` * 1.<Time.``cal yr BP``>)) |> TimeSeries.fromCalibratedRadiocarbonObservations
    ] |> Map.ofList


(**
We can run an individual model fit like so:
*)

let result =
    Bristlecone.tryFit engine endCond ts hypotheses.[5].Model
