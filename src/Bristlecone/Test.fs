namespace Bristlecone

module Test =

    open System
    open Time
    open ModelSystem
    open EstimationEngine

    /// Functions for adding background variability into
    /// test problems.
    module Noise =

        /// Adds noise to a time-series 'seriesName', based on the given distribution function.
        let tryAddNoise seriesName noiseDistributionFn (data: CodedMap<TimeSeries.TimeSeries<'a>>) =
            data 
            |> Map.tryFindKey (fun c _ -> c.Value = seriesName)
            |> Option.map(fun k ->
                data
                |> Map.find k
                |> TimeSeries.map (fun (x,_) -> x + noiseDistributionFn())
                |> fun ts -> Map.add k ts data)

        /// Adds normally-distributed noise around each data point in the selected
        /// time-series.
        /// Returns `None` if the series or parameter does not exist.
        let tryAddNormal sdParamCode seriesName rnd pool data =
            pool 
            |> Parameter.Pool.tryGetRealValue sdParamCode
            |> Option.bind(fun sd ->
                tryAddNoise seriesName (Bristlecone.Statistics.Distributions.Normal.draw rnd 0. sd) data)
            |> Result.ofOption (sprintf "Could not add noise. Either the parameter %s or the series '%s' is missing." sdParamCode seriesName)


    type GenerationRule = (string * (seq<float> -> bool))

    module GenerationRules =

        /// Ensures that all generated values are less than i
        let alwaysLessThan i variable : GenerationRule =
            variable, fun data -> data |> Seq.max < i

        /// Ensures that all generated values are greater than i
        let alwaysMoreThan i variable : GenerationRule =
            variable, fun data -> data |> Seq.min > i

        /// Ensures that there is always a positive change in values of a variable
        let monotonicallyIncreasing variable : GenerationRule =
            variable, fun data -> 
                data |> Seq.pairwise |> Seq.map(fun (x1,x2) -> (x2 - x1) > 0.) |> Seq.contains false 
    
    type TestSettings<'a> = {
        TimeSeriesLength: int
        StartValues: CodedMap<'a>
        EndCondition: EndCondition<'a>
        GenerationRules: GenerationRule list
        NoiseGeneration: System.Random -> Parameter.Pool -> CodedMap<TimeSeries<'a>> -> Result<CodedMap<TimeSeries<'a>>,string>
        EnvironmentalData: CodedMap<TimeSeries<'a>>
        Resolution: FixedTemporalResolution
        Random: System.Random
        StartDate: DateTime
        Attempts: int
    } with
        static member Default = {
            Resolution = FixedTemporalResolution.Years (PositiveInt.create 1).Value
            TimeSeriesLength = 30
            StartValues = Map.empty
            EndCondition = Optimisation.EndConditions.afterIteration 1000
            GenerationRules = []
            NoiseGeneration = fun _ -> fun _ -> id >> Ok
            EnvironmentalData = Map.empty
            Random = MathNet.Numerics.Random.MersenneTwister()
            StartDate = DateTime(1970,01,01)  
            Attempts = 50000
        }
    
    type ParameterTestResult = {
        Identifier: string
        RealValue: float
        EstimatedValue: float
    }

    and TestResult = {
        Parameters: ParameterTestResult list
        Series: Map<string,FitSeries>
        ErrorStructure: Map<string,seq<float>>
        RealLikelihood: float
        EstimatedLikelihood: float
    }

    /// Ensures settings are valid for a test, by ensuring that
    /// start values have been set for each equation.
    let isValidSettings (model:ModelSystem.ModelSystem) testSettings =
        let equationKeys = model.Equations |> Map.toList  |> List.map fst
        if Set.isSubset (Set.ofList equationKeys) (Set.ofList (testSettings.StartValues |> Map.toList |> List.map fst))
        then Ok testSettings
        else Error <| sprintf "You must specify a start point for the following equations: %A" equationKeys

    let create = TestSettings<float>.Default

    /// Add noise to a particular time-series when generating fake time-series.
    /// Built-in noise functions are in the `Noise` module.
    let addNoise noiseFn settings =
        let stackNoise rnd pool ts =
            settings.NoiseGeneration rnd pool ts
            |> Result.bind (noiseFn rnd pool)
        { settings with NoiseGeneration = stackNoise }

    let addGenerationRules rules settings =
        { settings with GenerationRules = List.append settings.GenerationRules rules }

    /// Adds a start value for a time-series.
    let addStartValue seriesName value settings =
        match ShortCode.create seriesName with
        | Some code -> { settings with StartValues = settings.StartValues |> Map.add code value }
        | None -> failwithf "%s is not a valid code for a series" seriesName

    /// Adds start values to the test settings. Overwrites any existing
    /// start values that may already exist.
    let addStartValues values settings =
        Seq.fold(fun set (s,v) ->
            addStartValue s v set) settings values

    let withTimeSeriesLength n settings = { settings with TimeSeriesLength = n }
    let withFixedTemporalResolution res settings = { settings with Resolution = res }
    let endWhen goal settings = { settings with EndCondition = goal }
    let useRandom rnd (settings: TestSettings<'a>) = { settings with Random = rnd }
    let useStartTime time settings = { settings with StartDate = time }

    module Compute =

        /// Draw a random set of parameters
        let drawParameterSet rnd pool =
            pool
            |> Parameter.Pool.map(fun _ v ->
                let lower,upper = 
                    match Parameter.bounds v with
                    | Some b -> b
                    | None -> failwith "Parameters already estimated"
                let trueValue = Statistics.Distributions.ContinuousUniform.draw rnd lower upper ()
                match Parameter.setTransformedValue v trueValue with
                | Ok p -> p
                | Error e -> failwith e )

        /// Generate a fixed-resolution time-series for testing model fits
        let generateFixedSeries writeOut equations timeMode seriesLength startPoint startDate resolution env theta =
            let applyFakeTime s = TimeSeries.fromSeq startDate resolution s
            let eqs = equations |> Map.map (fun _ v -> v theta)
            match timeMode with
            | Discrete -> invalidOp "Not supported at this time"
            | Continuous i -> 
                i writeOut 0. (seriesLength |> float) 1. startPoint env eqs
                |> Map.map (fun _ v -> applyFakeTime v)

        /// A test procedure for computing measures given time series data.
        let generateMeasures measures startValues (expected:CodedMap<TimeSeries<'a>>) : CodedMap<TimeSeries<'a>> =
            let time = (expected |> Seq.head).Value |> TimeSeries.toObservations |> Seq.map snd
            measures
            |> Map.map (fun key measure -> Solver.Discrete.solve startValues key measure (expected |> Map.map(fun _ t -> t.Values |> Seq.toArray)))
            |> Map.fold (fun acc key value -> Map.add key (time |> Seq.zip value |> TimeSeries.fromObservations) acc) expected

        /// Generate data
        let tryGenerateData' engine model testSettings theta =
            let envIndex = testSettings.EnvironmentalData |> Map.map(fun k v -> TimeIndex.TimeIndex(testSettings.StartDate, testSettings.Resolution, TimeIndex.IndexMode.Interpolate Statistics.Interpolate.bilinear, v))
            theta
            |> generateFixedSeries engine.LogTo model.Equations engine.TimeHandling testSettings.TimeSeriesLength testSettings.StartValues testSettings.StartDate testSettings.Resolution envIndex
            |> testSettings.NoiseGeneration testSettings.Random theta
            |> Result.lift (generateMeasures model.Measures testSettings.StartValues)

        /// Generate data and check that it complies with the
        /// given ruleset.
        let rec tryGenerateData (engine:EstimationEngine.EstimationEngine<'a,'b>) settings (model:ModelSystem.ModelSystem) attempts = 
            let theta = drawParameterSet engine.Random model.Parameters
            tryGenerateData' engine model settings theta
            |> Result.bind(fun series ->
                let brokeTheRules = 
                    settings.GenerationRules
                    |> List.choose(fun (key,ruleFn) -> 
                        series 
                        |> Map.tryFindKey (fun k _ -> k.Value = key)
                        |> Option.map(fun k -> 
                            Map.find k series 
                            |> TimeSeries.toObservations |> Seq.map fst |> ruleFn))
                if (brokeTheRules |> List.contains false) || brokeTheRules.Length <> settings.GenerationRules.Length
                then
                    if attempts = 0 
                    then Error "Could not generate data that complies with the given ruleset" 
                    else tryGenerateData engine settings model (attempts - 1)
                else Ok (series, theta)
            )