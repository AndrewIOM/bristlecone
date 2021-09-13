#load "bristlecone.fsx"
#load "components/allometric.fsx"
// #nuget: Bristlecone

////////////////////////////////////////////////////
/// Plant nitrogen limitation using wood rings
/// and nitrogen isotopes
////////////////////////////////////////////////////

(* An example Bristlecone script for working with
   wood ring datasets. *)

open Bristlecone            // Opens Bristlecone core library and estimation engine
open Bristlecone.Language   // Open the language for writing Bristlecone models
open Bristlecone.Time

// 1. Define basic model system
// ----------------------------
// First, we define a 'base model' into which we can insert
// components that represent different hypotheses.

let baseModel =
    
    /// Transform δ15N to N availability.
    let ``δ15N -> N availability`` =
        (Constant 100. * Environment "N" + Constant 309.) / Constant 359.
        
    /// Plant uptake of N from soil, which may be turned on or off
    let uptake f geom =
        (geom This) * This * (f ``δ15N -> N availability``)

    /// 1. Cumulative stem biomass
    let ``db/dt`` geom nLimitation =
        Parameter "r" * (uptake nLimitation geom) - Parameter "γ[b]" * This

    /// 2. Soil nitrogen availability
    let ``dN/dt`` geom feedback limitationName nLimitation =          
        if limitationName = "None"
        then Parameter "λ" - Parameter "γ[N]" * ``δ15N -> N availability`` + feedback This
        else
            Parameter "λ" - Parameter "γ[N]" * ``δ15N -> N availability``
            + feedback This
            - (geom This) * This * (nLimitation ``δ15N -> N availability``)

    /// 3. Stem radius (a 'Measurement' variable)
    let stemRadius lastRadius lastEnv env =
        let oldCumulativeMass = lastEnv |> lookup "bs"
        let newCumulativeMass = env |> lookup "bs"
        if (newCumulativeMass - oldCumulativeMass) > 0.
        then newCumulativeMass |> Allometric.Proxies.toRadiusMM
        else lastRadius

    fun geom feedback (nLimitMode,nLimitation) ->
        Model.empty
        |> Model.addEquation        "bs"    (``db/dt`` geom nLimitation)
        |> Model.addEquation        "N"     (``dN/dt`` geom feedback nLimitMode nLimitation)
        |> Model.includeMeasure     "x"     stemRadius
        |> Model.estimateParameter  "λ"     notNegative 0.001 0.500
        |> Model.estimateParameter  "γ[N]"  notNegative 0.001 0.200
        |> Model.estimateParameter  "γ[b]"  notNegative 0.001 0.200
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.bivariateGaussian "x" "N")
        |> Model.estimateParameter  "ρ"     noConstraints -0.500 0.500
        |> Model.estimateParameter  "σ[x]"  notNegative 0.001 0.100
        |> Model.estimateParameter  "σ[y]"  notNegative 0.001 0.100


// 2. Define competing hypotheses
// ----------------------------
/// Define 12 alternative hypotheses by defining three interchangeable components:
/// - Asymptotic plant size (2 types);
/// - Plant-soil feedback presence / absence (2 types); and
/// - Nitrogen limitation form (3 types).
/// The product of each of the three components with the base model forms 12
/// alternative hypotheses, each represented as a `ModelSystem`.
let hypotheses =
    
    // 1. Setup two alternatives for geometric mode.
    let chapmanRichards mass = Constant 1. - (mass / (Parameter "k" * Constant 1000.))
    let geometricModes = modelComponent "Geometric constraint" [
        subComponent "None" (Constant 1. |> (*))
        subComponent "Chapman-Richards" chapmanRichards
        |> estimateParameter "k" notNegative 3.00 5.00  // Asymptotic biomass (in kilograms)
    ]

    // 2. Setup two alternatives for plant-soil feedbacks.
    let biomassLoss biomass = (Parameter "ɑ" / Constant 100.) * biomass * Parameter "γ[b]"
    let feedbackModes = modelComponent "Plant-Soil Feedback" [
        subComponent "None" (Constant 1. |> (*))
        subComponent "Biomass Loss" biomassLoss
        |> estimateParameter "ɑ" notNegative 0.01 1.00  // N-recycling efficiency
    ]

    // 3. Setup three alternatives for the form of plant N limitation.
    
    let saturating minimumNutrient nutrient =
        let hollingModel n = (Parameter "a" * n) / (Constant 1. + (Parameter "a" * Parameter "b" * Parameter "h" * n))
        Conditional(fun compute ->
            if compute (hollingModel minimumNutrient) < 1e-12
            then Constant nan
            else hollingModel nutrient )
        
    let linear min resource =
        Conditional(fun compute ->
            if compute (Parameter "a" * min) < 1e-12 then Constant nan else Parameter "a" * resource)

    let limitationModes = modelComponent "N-limitation" [
        subComponent "Saturating" (saturating (Constant 5.))
        |> estimateParameter "a" notNegative 0.100 0.400
        |> estimateParameter "h" notNegative 0.100 0.400
        |> estimateParameter "r" notNegative 0.500 1.000
        subComponent "Linear" (linear (Constant 5.))
        |> estimateParameter "a" notNegative 0.100 0.400
        |> estimateParameter "r" notNegative 0.500 1.000
        subComponent "None" (Constant 1. |> (*))
        |> estimateParameter "r" notNegative 0.500 1.000
    ]

    baseModel
    |> Hypotheses.createFromComponent geometricModes
    |> Hypotheses.useAnother feedbackModes
    |> Hypotheses.useAnotherWithName limitationModes
    |> Hypotheses.compile


// 3. Setup Bristlecone Engine
// ----------------------------
// A bristlecone engine provides a fixed setup for estimating parameters from data.
// Use the same engine for all model fits within a single study.

let engine = 
    Bristlecone.mkContinuous
    |> Bristlecone.withContinuousTime Integration.MathNet.integrate
    |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint
    |> Bristlecone.withTunedMCMC [ Optimisation.MonteCarlo.TuneMethod.CovarianceWithScale 0.200, 250, Optimisation.EndConditions.afterIteration 20000 ]


// 4. Test Engine and Model
// ----------------------------
// Running a full test is strongly recommended. The test will demonstrate if the current
// configuration can find known parameters for a model. If this step fails, there is an
// issue with either your model, or the Bristlecone configuration.


// 5. Load Real Data
// ----------------------------
// Here, we are using the FSharp.Data type provider to read in .csv datasets.


// 6. Fit Models to Real Data
// -----------------------------------


// 7. Calculate model comparison statistics
// -----------------------------------


