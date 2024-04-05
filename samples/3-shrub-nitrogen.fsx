#load "bristlecone.fsx"
#load "components/allometric.fsx"
// #nuget: Bristlecone

////////////////////////////////////////////////////
// Plant nitrogen limitation using wood rings
// and nitrogen isotopes
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
            then Invalid
            else hollingModel nutrient )
        
    let linear min resource =
        Conditional(fun compute ->
            if compute (Parameter "a" * min) < 1e-12 then Invalid else Parameter "a" * resource)

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

let testSettings =
    Test.create
    |> Test.addNoise (Test.Noise.tryAddNormal "sigma[y]" "N")
    |> Test.addNoise (Test.Noise.tryAddNormal "sigma[x]" "bs")
    |> Test.addGenerationRules [ 
        Test.GenerationRules.alwaysMoreThan -3. "N"
        Test.GenerationRules.alwaysLessThan 20. "N"
        Test.GenerationRules.alwaysMoreThan 0. "bs"
        Test.GenerationRules.monotonicallyIncreasing "x" ] // There must be at least 10mm of wood production
    |> Test.addStartValues [
        "x", 5.0
        "bs", 5.0 |> Allometric.Proxies.toBiomassMM
        "N", 3.64 ]
    |> Test.withTimeSeriesLength 30
    |> Test.endWhen (Optimisation.EndConditions.afterIteration 1000)

let testResult =
    hypotheses 
    |> List.map(fst >> Bristlecone.testModel engine testSettings)


// 5. Load Real Data
// ----------------------------
// Here, we are using the Bristlecone.Dendro package to 
// read in dendroecological data.

open Bristlecone.Dendro

let shrubData =
    let plants = Data.PlantIndividual.loadRingWidths (__SOURCE_DIRECTORY__ + "/../data/yamal-rw.csv")
    let isotopeData = Data.PlantIndividual.loadLocalEnvironmentVariable (__SOURCE_DIRECTORY__ + "/../data/yuribei-d15N-imputed.csv")
    plants |> PlantIndividual.zipEnvMany "N" isotopeData


// 6. Fit Models to Real Data
// -----------------------------------



// 7. Calculate model comparison statistics
// -----------------------------------
 
   
// 3. Load Real Data and Estimate
// ----------------------------

// Define the start values for a model system, as follows:
// initial radius = 
let startValues (startDate:System.DateTime) (plant:PlantIndividual.PlantIndividual) =
    let initialRadius =
        match plant.Growth with
        | PlantIndividual.PlantGrowth.RingWidth s -> 
            match s with
            | GrowthSeries.Absolute c -> c.Head |> fst
            | _ -> invalidOp "Not applicable"
        | _ -> invalidOp "Not applicable"
    let initialMass = initialRadius |> ModelComponents.Proxies.toBiomassMM
    let initialNitrogen = plant.Environment.[code "N"].Head |> fst
    [ ("x", initialRadius)
      ("N", initialNitrogen)
      ("bs", initialMass) ] |> Map.ofList

let workPackages shrubs hypotheses engine saveDirectory =
    seq {
        for s in shrubs do

            // 1. Arrange the subject and settings
            let shrub = s |> PlantIndividual.toCumulativeGrowth
            let common = shrub |> PlantIndividual.keepCommonYears
            let startDate = (common.Environment.["N"]).StartDate |> snd
            let startConditions = getStartValues startDate shrub
            let e = engine |> Bristlecone.withConditioning (Custom startConditions)

            // 2. Setup batches of dependent analyses
            for h in [ 1 .. hypotheses |> List.length ] do
                for _ in [ 1 .. Options.chains ] do
                    if h = 1 || h = 2 || h = 7 || h = 8 || h = 9 || h = 10 then yield async {
                            // A. Compute result
                            let result = Bristlecone.PlantIndividual.fit e Options.endWhen hypotheses.[h-1] common |> fst
                            // B. Save to file
                            Bristlecone.Data.EstimationResult.saveAll saveDirectory s.Identifier.Value h 1 result
                            return result }
    }

// Orchestrate the analyses
let work = workPackages shrubs hypotheses Options.engine Options.resultsDirectory
let run() = work |> Seq.iter (OrchestrationMessage.StartWorkPackage >> Options.orchestrator.Post)


let saveDiagnostics () =

    // 1. Get all results sliced by plant and hypothesis
    let results = 
        let get subject model modelId = Bristlecone.Data.EstimationResult.loadAll Options.resultsDirectory subject.Identifier.Value model modelId
        Bristlecone.ModelSelection.ResultSet.arrangeResultSets shrubs hypotheses get

    // 2. Save convergence statistics to file
    // NB include only results within 5 likelihood of the minimum (to remove outliers)
    results 
    // |> Seq.map(fun (x,a,b,c) -> x.Identifier.Value,a,b,c)
    // |> Seq.toList
    |> Diagnostics.Convergence.gelmanRubinAll 10000
    |> Data.Convergence.save Options.resultsDirectory

    // 3. Save Akaike weights to file
    results
    |> ModelSelection.Select.weights
    |> Seq.map(fun (x,a,b,c) -> x.Identifier.Value,a,b,c)
    |> Data.ModelSelection.save Options.resultsDirectory

    // 4. Save out logged components
    // results
    // |> Seq.map(fun r -> calculateComponents fit Options.engine r)





// What about one-step-ahead predictions?

open Bristlecone.Diagnostics.ModelComponents
open FSharp.Data

// 1. Load in all MLE results and pick the best for each shrub x hypothesis.
// - Must convert from old bristlecone format to new one.

module LoadOldTrace =

    open Bristlecone.Data.Trace
    open Bristlecone.Data.Config

    type OldTrace = CsvProvider<"/Users/andrewmartin/Desktop/Bristlecone Results/Paper1-Yuribei-Annual/dphil-shrub-output-YUSL03A-1-92aadb09-f035-4373-aacd-a16ed7dec822.csv">

    let fileMatch directory subject modelId =
        let path = System.IO.DirectoryInfo(directory)
        if path.Exists then
            let files = path.GetFiles(sprintf "dphil-shrub-output-%s-%i-*.csv" subject modelId)
            let regex = sprintf "dphil-shrub-output-%s-%i-(%s).csv" subject modelId regexGuid
            files |> Seq.choose(fun f -> 
                let m = System.Text.RegularExpressions.Regex.Match(f.Name, regex)
                if m.Success
                then 
                    let guid = m.Groups.[1].Value |> System.Guid.Parse
                    (guid, f) |> Some
                else None )
        else invalidArg "directory" "The specified directory does not exist"

    let toTrace (data:OldTrace) : (float * float []) list =
        data.Rows
        |> Seq.groupBy(fun r -> r.Iteration)
        |> Seq.map(fun (i,r) -> 
            i,
            (r |> Seq.head).NegativeLogLikelihood, 
            r |> Seq.map(fun r -> r.ParameterValue) |> Seq.toArray)
        |> Seq.sortByDescending(fun (i,_,_) -> i)
        |> Seq.map(fun (_,x,y) -> x,y)
        |> Seq.toList

    let loadOldTrace directory subject modelId =
        let traceFiles = fileMatch directory subject modelId
        traceFiles
        |> Seq.choose(fun (i,f) ->
            printfn "File loading is %s" f.FullName
            let data = OldTrace.Load f.FullName
            match data.Rows |> Seq.length with
            | 0 -> None
            | _ -> data |> toTrace |> Some )

let oldShrubDir = "/Users/andrewmartin/Documents/DPhil Zoology/Bristlecone Results/YuribeiAnnual/"

let oldMleResults = 
    seq {
        for s in shrubs do
            for hi in [ 1 .. 12 ] do
                let oldResult = LoadOldTrace.loadOldTrace oldShrubDir s.Identifier.Value hi |> Seq.concat |> Seq.toList
                let mle = oldResult |> Seq.minBy(fun (p,l) -> p)
                yield s, hi, mle
    } 
    |> Seq.toList


// 2. Setup each so that 

module Cool =

    open Bristlecone.Bristlecone
    open Bristlecone.Optimisation

    /// "How good am I at predicting the next data point"?
    /// 
    let oneStepAhead engine hypothesis (preTransform:CodedMap<TimeSeries<float>>->CodedMap<TimeSeries<float>>) (timeSeries) (estimatedTheta:ParameterPool) =
        let mleToBounds mlePool = mlePool |> Map.map(fun k v -> Parameter.create (Parameter.detatchConstraint v |> snd) (v |> Parameter.getEstimate) (v |> Parameter.getEstimate))
        let hypothesisMle : ModelSystem =  { hypothesis with Parameters = mleToBounds estimatedTheta }
        let pairedDataFrames =
            timeSeries
            |> Map.map(fun _ fitSeries -> 
                fitSeries 
                |> TimeSeries.toObservations 
                |> Seq.pairwise 
                |> Seq.map (fun (t1,t2) -> TimeSeries.fromObservations [t1; t2] |> TimeSeries.map(fun (x,y) -> x )))
        printfn "Paired data frames = %A" pairedDataFrames
        let timeParcelCount = (pairedDataFrames |> Seq.head).Value |> Seq.length
        printfn "Time parcels: %i" timeParcelCount
        let data =
            seq { 1 .. timeParcelCount }
            |> Seq.map(fun i -> pairedDataFrames |> Map.map(fun _ v -> v |> Seq.item (i-1)) |> preTransform)
        printfn "Data: %A" data

        // TODO Remove this hack:
        // It is predicting with a repeated first point... so...
        // The next point estimate is at t1
        // The next point observation is at t2
        data
        |> Seq.map (fun d -> 
            let est = Bristlecone.fit (engine |> withCustomOptimisation Optimisation.None.passThrough |> withConditioning RepeatFirstDataPoint) (EndConditions.afterIteration 0) d hypothesisMle
            printfn "Estimated time series is %A" (est |> fst).Series

            let nextObservation = d |> Map.map(fun c ts -> ts |> TimeSeries.toObservations |> Seq.skip 1 |> Seq.head)
            let paired =
                nextObservation
                |> Map.map(fun code obs ->
                    let nextEstimate = ((est |> fst).Series.[code].Values |> Seq.head).Fit
                    obs |> snd, obs |> fst, nextEstimate
                    )

            paired
        )
        |> Seq.toList

    /// Perform n-step-ahead computation on the hypothesis and plant.
    let predictAhead (engine:EstimationEngine.EstimationEngine<float,float>) system (plant:PlantIndividual) preTransform estimate =
        let g =
            match plant.Growth |> growthSeries with
            | Absolute g -> g
            | Cumulative g -> g
            | Relative g -> g
        let predictors = plant.Environment |> Map.add (ShortCode.create "x") g
        oneStepAhead engine system preTransform predictors estimate


let oneStepPredictions =
    oldMleResults
    |> List.map(fun (s,hi,mle) ->

            // 0. Convert x into biomass
            let preTransform (data:CodedMap<TimeSeries<float>>) =
                data
                |> Map.toList
                |> List.collect(fun (k,v) ->
                    if k = code "x"
                    then [ (k, v); (code "bs", v |> TimeSeries.map(fun (x,_) -> x |> ModelComponents.Proxies.toBiomassMM)) ]
                    else [ (k, v)] )
                |> Map.ofList

            // 1. Arrange the subject and settings
            let shrub = s |> PlantIndividual.toCumulativeGrowth
            let common = shrub |> PlantIndividual.keepCommonYears
            let startDate = (common.Environment.[code "N"]).StartDate |> snd
            let startConditions = getStartValues startDate shrub
            let e = Options.engine |> Bristlecone.withConditioning (Custom startConditions)

            let mlePool = 
                hypotheses.[hi-1].Parameters
                |> Map.toList
                |> List.mapi(fun i (sc,p) ->
                    let est = (mle |> snd).[i]
                    sc, Parameter.setEstimate p est)
                |> ParameterPool

            Cool.predictAhead e hypotheses.[hi-1] common preTransform mlePool
            |> List.map(fun r -> s.Identifier.Value, hi, r)
        )


type SOSRow = {
    Year: int
    Variable: string
    Shrub: string
    Hypothesis: int
    Observation: float
    OneStepPrediction: float
}

let predictions = 
    oneStepPredictions
    |> List.collect(fun m ->
        m |> Seq.collect(fun (s,h,m) -> 
            m |> Seq.map(fun kv ->
                let y,a,b = kv.Value
                let x = 
                    {
                    Year = y.Year
                    Variable = kv.Key.Value
                    Observation = a
                    Shrub = s
                    Hypothesis = h
                    OneStepPrediction = b }
                x
            )) |> Seq.toList )

predictions
|> List.map(fun x -> sprintf "%s,%i,%i,%s,%f,%f" x.Shrub x.Hypothesis x.Year x.Variable x.Observation x.OneStepPrediction)
|> List.append ["Shrub,Hypothesis,Year,Variable,Observed,OneStepPrediction"]
|> String.concat "\n"
|> fun x -> System.IO.File.WriteAllText("/Users/andrewmartin/Desktop/onestepahead.csv", x)

// Root mean squared error is the average squared differences, then square rooted.
let rmse =
    predictions
    |> List.groupBy(fun x -> x.Shrub, x.Hypothesis, x.Variable)
    |> List.map(fun ((s,h,v),x) ->
        let sos = x |> Seq.averageBy(fun x -> (x.Observation - x.OneStepPrediction) ** 2.)
        s, h, v, sqrt sos)

rmse
|> List.map(fun (s,h,v,sos) -> sprintf "%s,%i,%s,%f" s h v sos)
|> List.append ["Shrub,Hypothesis,Variable,RMSE"]
|> String.concat "\n"
|> fun x -> System.IO.File.WriteAllText("/Users/andrewmartin/Desktop/onestepahead-2.csv", x)

