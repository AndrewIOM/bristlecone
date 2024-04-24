(**

*)
#r "nuget: Bristlecone,2.0.0"
(**
## A fully-worked example: soil nutrients and plant growth

The following code demonstrates the key functionality of
Bristlecone, using a real-world example. Specifically, it
demonstrates model-fitting and model-selection for the
mechanisms controlling nutrient limitation to shrub individuals
in the Arctic tundra.

The hypotheses are tested on a per-shrub basis. For each shrub,
there are four components that may vary in the model, which
when multiplied together results in 12 possible combinations
of mechanisms for each shrub.

First, we must load Bristlecone.

*)
open Bristlecone // Opens Bristlecone core library and estimation engine
open Bristlecone.Language // Open the language for writing Bristlecone models
open Bristlecone.Time
(**
### Defining a 'base model'

Then, we define a `base model` system, into which we can nest
the components that vary (which represent different hypotheses).

Our base model consists of the following parts:

* **An empirical transform.** As this model uses long-term ecological data, 
there is a function that translates from an ecological proxy (stable nitrogen isotope)
into the desired variable - soil nitrogen availability.

* **Two ordinary differential equations (ODEs).** These represent the soil N availability
and plant biomass through time.

* **A **measurement** variable.** Bristlecone 'measurements' are effectively
derived time-series that are calulated from the integrated ODE output. Here,
we use a measurement variable to transform biomass into wood ring production,
where rings are only produced where plant biomass has increased during the year.

The code for the parts of the base model is shown below.

*)
// Empirical transform from δ15N to N availability.
let ``δ15N -> N availability`` =
    (Constant 100. * Environment "N" + Constant 309.) / Constant 359.

// ODE1. Cumulative stem biomass
let ``db/dt`` geom nLimitation =
    Parameter "r" * (geom This) * This * (nLimitation ``δ15N -> N availability``)
    - Parameter "γ[b]" * This

// ODE2. Soil nitrogen availability
let ``dN/dt`` geom feedback limitationName nLimitation =
    if limitationName = "None" then
        Parameter "λ" - Parameter "γ[N]" * ``δ15N -> N availability`` + feedback This
    else
        Parameter "λ" - Parameter "γ[N]" * ``δ15N -> N availability`` + feedback This
        - (geom This) * This * (nLimitation ``δ15N -> N availability``)

// Measurement variable: stem radius
let stemRadius lastRadius lastEnv env =
    let oldCumulativeMass = lastEnv |> lookup "bs"
    let newCumulativeMass = env |> lookup "bs"

    if (newCumulativeMass - oldCumulativeMass) > 0. then
        newCumulativeMass |> Allometric.Proxies.toRadiusMM // NB Allometrics is given in full in the downloadable script.
    else
        lastRadius
(**
Once we have defined the components, we can scaffold them into a model system.
We can plug in the nestable hypotheses (defined further below) by defining
the base model as a function that takes parameters representing the
alternative hypotheses.

*)
let ``base model`` geometricConstraint plantSoilFeedback (nLimitMode, nLimitation) =
    Model.empty
    |> Model.addEquation "bs" (``db/dt`` geometricConstraint nLimitation)
    |> Model.addEquation "N" (``dN/dt`` geometricConstraint plantSoilFeedback nLimitMode nLimitation)
    |> Model.includeMeasure "x" stemRadius
    |> Model.estimateParameter "λ" notNegative 0.001 0.500
    |> Model.estimateParameter "γ[N]" notNegative 0.001 0.200
    |> Model.estimateParameter "γ[b]" notNegative 0.001 0.200
    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.bivariateGaussian "x" "N")
    |> Model.estimateParameter "ρ" noConstraints -0.500 0.500
    |> Model.estimateParameter "σ[x]" notNegative 0.001 0.100
    |> Model.estimateParameter "σ[y]" notNegative 0.001 0.100
(**
### Defining the competing hypotheses

Here, we define 12 alternative hypotheses by defining three interchangeable
components:

* Asymptotic plant size (2 types);

* Plant-soil feedback presence / absence (2 types); and

* Nitrogen limitation form (3 types).

Once we have defined each of the three components, we can take the
product of them with the base model which forms 12
alternative hypotheses, each represented as a `ModelSystem`.

#### Geometric constraint

Plants do not grow indefinitely, but eventually reach an asymptotic mass
owing either to geometric or resource constraints. Here, we define two
competing hypotheses: that a shrub does not show evidence of nearing its
asymptote, or that it does (based on a Chapman-Richards growth function).

In Bristlecone, we use `modelComponent` to construct a pluggable component
into a model system. We pass `modelComponent` a list of `subComponent`s, which
each have a name and an equation. In its equation, a model component can
take one or many parameters, but these must match the signiture required
by the hole in the base model. For example, the geometric constraint here
takes the current `mass` only.

In addition, a `subComponent` may require additional estimatable parameters
over the base model. In this case, the Chapman-Richards model requires an
extra parameter **K**, which represents the asymptotic biomass. These may be
added to a `subComponent` by using `|> estimateParameter` afterwards, as below.

*)
let ``geometric constraint`` =
    modelComponent
        "Geometric constraint"
        [ subComponent "None" (Constant 1. |> (*))
          subComponent "Chapman-Richards" (fun mass -> Constant 1. - (mass / (Parameter "k" * Constant 1000.)))
          |> estimateParameter "k" notNegative 3.00 5.00 ] // Asymptotic biomass (in kilograms)
(**
#### Plant-soil feedback

The plant-soil feedback is the flow of nutrients from plant biomass into the
soil available nitrogen pool. Here, we effectively turn on or off N input into
the soil pool on biomass loss. In the base model, density-dependent biomass
loss occurs. Turning on the feedback here creates an N input into soil based
on the biomass lost multiplied by a conversion factor `ɑ`.

*)
let ``plant-soil feedback`` =

    let biomassLoss biomass =
        (Parameter "ɑ" / Constant 100.) * biomass * Parameter "γ[b]"

    modelComponent
        "Plant-Soil Feedback"
        [ subComponent "None" (Constant 1. |> (*))
          subComponent "Biomass Loss" biomassLoss
          |> estimateParameter "ɑ" notNegative 0.01 1.00 ] // N-recycling efficiency
(**
#### Nitrogen limitation

We specify three plausable mechanisms for nutrient limitation to shrub
growth: (1) that growth is independent on soil N availability; (2) that
growth is dependent on soil N availability in a linear way; or (3) that
a mechanistic model of root-foraging (saturating) best represents
N-limitation of shrub growth.

A new concept here is the `Conditional` element in an equation. This
term exposes a `ModelExpression -> float` (compute), allowing a calculation
to be conditional on the state of parameters or values. In this example,
we use it to restrict the models such that the N-limiting effect cannot
be zero.

*)
let ``N-limitation to growth`` =

    let saturating minimumNutrient nutrient =
        let hollingModel n =
            (Parameter "a" * n)
            / (Constant 1. + (Parameter "a" (** Parameter "b"*) * Parameter "h" * n))

        Conditional
        <| fun compute ->
            if compute (hollingModel minimumNutrient) < 1e-12 then
                Invalid
            else
                hollingModel nutrient

    let linear min resource =
        Conditional
        <| fun compute ->
            if compute (Parameter "a" * min) < 1e-12 then
                Invalid
            else
                Parameter "a" * resource

    modelComponent
        "N-limitation"
        [ subComponent "Saturating" (saturating (Constant 5.))
          |> estimateParameter "a" notNegative 0.100 0.400
          |> estimateParameter "h" notNegative 0.100 0.400
          |> estimateParameter "r" notNegative 0.500 1.000
          subComponent "Linear" (linear (Constant 5.))
          |> estimateParameter "a" notNegative 0.100 0.400
          |> estimateParameter "r" notNegative 0.500 1.000
          subComponent "None" (Constant 1. |> (*))
          |> estimateParameter "r" notNegative 0.500 1.000 ]
(**
#### Putting the hypotheses together

*)
let hypotheses =
    ``base model``
    |> Hypotheses.createFromComponent ``geometric constraint``
    |> Hypotheses.useAnother ``plant-soil feedback``
    |> Hypotheses.useAnotherWithName ``N-limitation to growth``
    |> Hypotheses.compile
(**
The resultant hypotheses (each of which is a `Hypotheses.Hypothesis`) are:

```
Hypothesis 1 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: None + None + Saturating
Hypothesis 2 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: Chapman-Richards + None + Saturating
Hypothesis 3 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: None + Biomass Loss + Saturating
Hypothesis 4 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: Chapman-Richards + Biomass Loss + Saturating
Hypothesis 5 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: None + None + Linear
Hypothesis 6 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: Chapman-Richards + None + Linear
Hypothesis 7 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: None + Biomass Loss + Linear
Hypothesis 8 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: Chapman-Richards + Biomass Loss + Linear
Hypothesis 9 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: None + None + None
Hypothesis 10 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: Chapman-Richards + None + None
Hypothesis 11 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: None + Biomass Loss + None
Hypothesis 12 [MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]_MICROSOFT.FSHARP.COLLECTIONS.IENUMERATOR+MKSEQ@177[SYSTEM.COLLECTIONS.GENERIC.IENUMERABLE`1[SYSTEM.CHAR]]]: Chapman-Richards + Biomass Loss + None
```

### Setting up a **Bristlecone engine**

A bristlecone engine provides a fixed setup for estimating parameters from data.
We use the same engine for all model fits within a single study.

Here, we scaffold an engine from `Bristlecone.mkContinuous`, as we are working
with continuous-time models.

**Note.** In a non-docs environment, we may choose to use a logger for this
example that outputs real-time traces in graphical format using `ggplot` in `R`.
To do this, we can call `withOutput` with `Logging.RealTimeTrace.graphWithConsole 60. 10000`
to use the graphics functions from the `Bristlecone.Charts.R` package.

*)
let output = Logging.Console.logger 1000

let engine =
    Bristlecone.mkContinuous
    |> Bristlecone.withContinuousTime Integration.MathNet.integrate
    |> Bristlecone.withOutput output
    |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint
    |> Bristlecone.withCustomOptimisation (
        Optimisation.MonteCarlo.SimulatedAnnealing.fastSimulatedAnnealing
            0.01
            true
            { Optimisation.MonteCarlo.SimulatedAnnealing.AnnealSettings<float>.Default with
                InitialTemperature = 100.
                TemperatureCeiling = Some 100.
                HeatRamp = (fun t -> t + 5.00)
                BoilingAcceptanceRate = 0.85
                HeatStepLength = Optimisation.EndConditions.afterIteration 1000
                TuneLength = 1000
                AnnealStepLength =
                    (fun x n ->
                        Optimisation.MonteCarlo.SimulatedAnnealing.EndConditions.improvementCount 5000 250 x n
                        || Optimisation.EndConditions.afterIteration 10000 x n) }
    )
(**
### Testing the engine and model

Running a full test is strongly recommended. The test will demonstrate if the current
configuration can find known parameters for a model. If this step fails, there is an
issue with either your model, or the Bristlecone configuration.

*)
let testSettings =
    Test.create
    |> Test.addNoise (Test.Noise.tryAddNormal "σ[y]" "N")
    |> Test.addNoise (Test.Noise.tryAddNormal "σ[x]" "bs")
    |> Test.addGenerationRules
        [ Test.GenerationRules.alwaysMoreThan -3. "N"
          Test.GenerationRules.alwaysLessThan 20. "N"
          Test.GenerationRules.alwaysMoreThan 0. "bs"
          Test.GenerationRules.monotonicallyIncreasing "x" ] // There must be at least 10mm of wood production
    |> Test.addStartValues [ "x", 5.0; "bs", 5.0<Dendro.mm> |> Allometric.Proxies.toBiomassMM; "N", 3.64 ]
    |> Test.withTimeSeriesLength 30
    |> Test.endWhen (Optimisation.EndConditions.afterIteration 1000)
let testResult =
    hypotheses
    |> List.map ((fun h -> h.Model) >> Bristlecone.testModel engine testSettings)
(**
### Load in the real dataset

Here, we are using the Bristlecone.Dendro package to
read in dendroecological data. For other problems, any
method to wrangle the data into a `TimeSeries` is acceptable.

*)
open Bristlecone.Dendro

let dataset =
    let plants =
        Data.PlantIndividual.loadRingWidths (__SOURCE_DIRECTORY__ + "/../data/yamal-rw.csv")

    let isotopeData =
        Data.PlantIndividual.loadLocalEnvironmentVariable (__SOURCE_DIRECTORY__ + "/../data/yuribei-d15N-imputed.csv")

    plants |> PlantIndividual.zipEnvMany "N" isotopeData
(**
### Model-fitting to real data

In this step, we will apply our `EstimationEngine` to the real
shrub data and model hypotheses.

Because there are so many hypotheses (x12) and individuals (x10),
and we want to run replicate analyses (x3), it makes sense to
run these 360 workloads in parallel. To do this, we can make use of
the workflow tools in the `Bristlecone.Workflow` namespace.

An orchestration agent is an agent that queues and runs work items
in parallel. Below, we demonstrate setting up an agent:

*)
open Bristlecone.Workflow

let orchestrator =
    Orchestration.OrchestrationAgent(
        writeOut = output,
        maxSimultaneous = System.Environment.ProcessorCount,
        retainResults = false
    )
(**
Before we can run the models, there are some specific considerations
required for this problem.

#### Setting the start values

Given time-series data, a common modelling problem is the question of
what to set t = 0 as. One strategy is to repeat the first data point (t1)
as t0. In this instance, out isotope time-series are shorter than the
ring-width time-series, so we generally know the real value of one
time-series but not the other. Because of this, we use a custom start
point for each shrub individual.

*)
let startValues (startDate: System.DateTime) (plant: PlantIndividual.PlantIndividual) =
    let removeUnit (x: float<_>) = float x

    let initialRadius =
        match plant.Growth with
        | PlantIndividual.PlantGrowth.RingWidth s ->
            match s with
            | GrowthSeries.Cumulative c ->
                let trimmed = c |> TimeSeries.trimStart (startDate - System.TimeSpan.FromDays(366.))

                match trimmed with
                | Some t -> t.Values |> Seq.head
                | None -> failwith "Could not get t0 from ring-width series"
            | _ -> invalidOp "Not applicable"
        | _ -> invalidOp "Not applicable"

    let initialMass = initialRadius |> Allometric.Proxies.toBiomassMM
    let initialNitrogen = plant.Environment.[(code "N").Value].Head |> fst

    [ ((code "x").Value, initialRadius |> removeUnit)
      ((code "N").Value, initialNitrogen)
      ((code "bs").Value, initialMass) ]
    |> Map.ofList
(**
Next, we set up some configuration settings, then wrap up our hypotheses and shrubs
as work packages for the orchestrator, where a work package is a `Async<ModelSystem.EstimationResult>`.

The function to make the work packages first sets up the common time period and custom
start values for each shrub, then creates per-hypothesis work packages for that shrub.

*)
module Config =

    let numberOfReplicates = 3
    let resultsDirectory = "results/test/"
    let thinTrace = Some 100
    let endWhen = Optimisation.EndConditions.afterIteration 100000


// Function to scaffold work packages
let workPackages shrubs (hypotheses: Hypotheses.Hypothesis list) engine saveDirectory =
    seq {
        for s in shrubs do

            // 1. Arrange the subject and settings
            let shrub = s |> PlantIndividual.toCumulativeGrowth
            let common = shrub |> PlantIndividual.keepCommonYears
            let startDate = (common.Environment.[(code "N").Value]).StartDate |> snd
            let startConditions = startValues startDate shrub
            let e = engine |> Bristlecone.withConditioning (Conditioning.Custom startConditions)

            // 2. Setup batches of dependent analyses
            for h in hypotheses do
                for _ in [ 1 .. Config.numberOfReplicates ] do
                    yield
                        async {
                            // A. Compute result
                            let result =
                                Bristlecone.tryFit e Config.endWhen (Bristlecone.fromDendro common) h.Model
                            // B. Save to file
                            match result with
                            | Ok r ->
                                Bristlecone.Data.EstimationResult.saveAll
                                    saveDirectory
                                    s.Identifier.Value
                                    h.ReferenceCode
                                    Config.thinTrace
                                    r

                                return r
                            | Error e -> return failwithf "Error in package: %s" e
                        }
    }

let work = workPackages dataset hypotheses engine Config.resultsDirectory

// Calling this function will run all of the analyses, but for this
// example script we won't run them here.
let run () =
    work
    |> Seq.iter (Orchestration.OrchestrationMessage.StartWorkPackage >> orchestrator.Post)
(**
### Model selection

After calling `run ()`, we should have a folder containing csv files, three
for each `EstimationResult`. We can load all of these in at once to
calculate model comparison statistics.

Functions for loading data are in the `Bristlecone.Data` namespace. You
may notice that in the work package definition above we used functions from
this namespace to save the results.

*)
let saveDiagnostics () =

    // 1. Get all results sliced by plant and hypothesis
    let results =
        let get (subject: PlantIndividual.PlantIndividual) (hypothesis: Hypotheses.Hypothesis) =
            Bristlecone.Data.EstimationResult.loadAll
                Config.resultsDirectory
                subject.Identifier.Value
                hypothesis.Model
                hypothesis.ReferenceCode

        Bristlecone.ModelSelection.ResultSet.arrangeResultSets dataset hypotheses get

    // 2. Save convergence statistics to file
    results
    |> Diagnostics.Convergence.gelmanRubinAll
        10000
        (fun (s: PlantIndividual.PlantIndividual) -> s.Identifier.Value)
        (fun (h: Hypotheses.Hypothesis) -> h.ReferenceCode)
    |> Data.Convergence.save Config.resultsDirectory

    // 3. Save Akaike weights to file
    results
    |> ModelSelection.Akaike.akaikeWeightsForSet (fun (h: Hypotheses.Hypothesis) -> h.ReferenceCode)
    |> Seq.map (fun (x, a, b, c) -> x.Identifier.Value, a, b, c)
    |> Data.ModelSelection.save Config.resultsDirectory


    // // 4. Save out logged components
    // results
    // |> Seq.map(fun r ->
    //    Diagnostics.ModelComponents.calculateComponents fit engine r)

    // 5. One-step ahead predictions

    let bestFits =
        Seq.allPairs dataset hypotheses
        |> Seq.map (fun (s, h) ->
            s, h, Bristlecone.Data.MLE.loadBest Config.resultsDirectory s.Identifier.Value h.Model h.ReferenceCode)

    let oneStepPredictions =
        bestFits
        |> Seq.map (fun (s, h, mle) ->

            // 0. Convert x into biomass
            let preTransform (data: CodedMap<TimeSeries<float>>) =
                data
                |> Map.toList
                |> List.collect (fun (k, v) ->
                    if k.Value = "x" then
                        [ (k, v)
                          ((code "bs").Value,
                           v |> TimeSeries.map (fun (x, _) -> x * 1.<mm> |> Allometric.Proxies.toBiomassMM)) ]
                    else
                        [ (k, v) ])
                |> Map.ofList

            // 1. Arrange the subject and settings (same as in model-fitting)
            let shrub = s |> PlantIndividual.toCumulativeGrowth
            let common = shrub |> PlantIndividual.keepCommonYears
            let startDate = (common.Environment.[(code "N").Value]).StartDate |> snd
            let startConditions = startValues startDate shrub

            let e =
                engine
                |> Bristlecone.withConditioning (Bristlecone.Conditioning.Custom startConditions)

            let result =
                Bristlecone.oneStepAhead e h.Model preTransform (Bristlecone.fromDendro common) (mle |> snd |> snd)

            // Save each n-step ahead result to a csv file
            Bristlecone.Data.NStepAhead.save
                Config.resultsDirectory
                s.Identifier.Value
                h.ReferenceCode
                (mle |> fst)
                1
                result

            s.Identifier.Value, h.ReferenceCode, result)

    Bristlecone.Data.NStepAhead.saveAllRMSE Config.resultsDirectory oneStepPredictions

    ()

