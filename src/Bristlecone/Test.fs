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
        let tryAddNoise
            seriesName
            noiseDistributionFn
            (data: CodedMap<TimeSeries.TimeSeries<float, 'date, 'timeunit, 'timespan>>)
            =
            data
            |> Map.tryFindKey (fun c _ -> c.Value = seriesName)
            |> Option.map (fun k ->
                data
                |> Map.find k
                |> TimeSeries.map (fun (x, _) -> x + noiseDistributionFn ())
                |> fun ts -> Map.add k ts data)

        /// Adds normally-distributed noise around each data point in the selected
        /// time-series.
        /// Returns `None` if the series or parameter does not exist.
        let tryAddNormal sdParamCode seriesName rnd pool data =
            pool
            |> Parameter.Pool.tryGetRealValue sdParamCode
            |> Option.bind (fun sd ->
                tryAddNoise seriesName (Bristlecone.Statistics.Distributions.Normal.draw rnd 0. sd) data)
            |> Result.ofOption (
                sprintf
                    "Could not add noise. Either the parameter %s or the series '%s' is missing."
                    sdParamCode
                    seriesName
            )


    type GenerationRule<[<Measure>] 'state> = (string * (seq<float<'state>> -> bool))

    module GenerationRules =

        /// Ensures that all generated values are less than i
        let alwaysLessThan i variable : GenerationRule<'state> =
            variable, (fun data -> data |> Seq.max < i)

        /// Ensures that all generated values are greater than i
        let alwaysMoreThan i variable : GenerationRule<'state> =
            variable, (fun data -> data |> Seq.min > i)

        let alwaysFinite variable : GenerationRule<'state> =
            variable, (fun data -> data |> Seq.exists Units.isNotFinite |> not)

        /// Ensures that there is always a positive change in values of a variable
        let monotonicallyIncreasing variable : GenerationRule<'state> =
            variable,
            fun data ->
                data
                |> Seq.pairwise
                |> Seq.map (fun (x1, x2) -> (x2 - x1) > LanguagePrimitives.FloatWithMeasure<'state> 0.)
                |> Seq.contains false


    type TestSettings<[<Measure>] 'stateUnit, 'date, 'yearUnit, 'timespan> =
        { TimeSeriesLength: int
          StartValues: CodedMap<float<'stateUnit>>
          EndCondition: EndCondition
          GenerationRules: GenerationRule<'stateUnit> list
          NoiseGeneration:
              Random
                  -> Parameter.Pool.ParameterPool
                  -> CodedMap<TimeSeries<float<'stateUnit>, 'date, 'yearUnit, 'timespan>>
                  -> Result<CodedMap<TimeSeries<float<'stateUnit>, 'date, 'yearUnit, 'timespan>>, string>
          EnvironmentalData: CodedMap<TimeSeries<float<'stateUnit>, 'date, 'yearUnit, 'timespan>>
          Resolution: Resolution.FixedTemporalResolution<'timespan>
          Random: Random
          StartDate: 'date
          DateMode: DateMode.DateMode<'date, 'yearUnit, 'timespan>
          Attempts: int }

        static member Default: TestSettings<1, DateTime, int<year>, TimeSpan> =
            { Resolution = Resolution.FixedTemporalResolution.Years (PositiveInt.create 1<year>).Value
              TimeSeriesLength = 30
              StartValues = Map.empty
              EndCondition = Optimisation.EndConditions.atIteration 1000<iteration>
              GenerationRules = []
              NoiseGeneration = fun _ -> fun _ -> id >> Ok
              EnvironmentalData = Map.empty
              Random = MathNet.Numerics.Random.MersenneTwister()
              StartDate = DateTime(1970, 01, 01)
              DateMode = DateMode.calendarDateMode
              Attempts = 50000 }

        static member Annual: TestSettings<1, DatingMethods.Annual, int<year>, int<year>> =
            { Resolution = Resolution.FixedTemporalResolution.Years (PositiveInt.create 1<year>).Value
              TimeSeriesLength = 30
              StartValues = Map.empty
              EndCondition = Optimisation.EndConditions.atIteration 1000<iteration>
              GenerationRules = []
              NoiseGeneration = fun _ -> fun _ -> id >> Ok
              EnvironmentalData = Map.empty
              Random = MathNet.Numerics.Random.MersenneTwister()
              StartDate = DatingMethods.Annual 1970<year>
              DateMode = DateMode.annualDateMode
              Attempts = 50000 }

    let defaultSettings = TestSettings<_, _, _, _>.Default
    let annualSettings = TestSettings<_, _, _, _>.Annual


    type ParameterTestResult =
        { Identifier: string
          RealValue: float<parameter>
          EstimatedValue: float<parameter> }

    and TestResult<'date, 'timeunit, 'timespan, [<Measure>] 'u> =
        { Parameters: ParameterTestResult list
          Series: Map<string, FitSeries<'date, 'timeunit, 'timespan>>
          ErrorStructure: Map<string, seq<float<'u^2>>>
          IterationsRun: int<iteration>
          RealLikelihood: float<``-logL``>
          EstimatedLikelihood: float<``-logL``> }

    /// Ensures settings are valid for a test, by ensuring that
    /// start values have been set for each equation.
    let isValidSettings (model: ModelSystem.ModelSystem<'modelTimeUnit>) testSettings =
        let equationKeys =
            match model.Equations with
            | DifferenceEqs eqs -> eqs |> Map.toList |> List.map fst
            | DifferentialEqs eqs -> eqs |> Map.toList |> List.map fst

        if
            Set.isSubset (Set.ofList equationKeys) (Set.ofList (testSettings.StartValues |> Map.toList |> List.map fst))
        then
            Ok testSettings
        else
            Error
            <| sprintf "You must specify a start point for the following equations: %A" equationKeys

    // let create = TestSettings<_, _, _, _>.Default

    /// Add noise to a particular time-series when generating fake time-series.
    /// Built-in noise functions are in the `Noise` module.
    let addNoise noiseFn settings =
        let stackNoise rnd pool ts =
            settings.NoiseGeneration rnd pool ts |> Result.bind (noiseFn rnd pool)

        { settings with
            NoiseGeneration = stackNoise }

    let addGenerationRules rules settings =
        { settings with
            GenerationRules = List.append settings.GenerationRules rules }

    /// Adds a start value for a time-series.
    let addStartValue seriesName value settings =
        match ShortCode.create seriesName with
        | Some code ->
            { settings with
                StartValues = settings.StartValues |> Map.add code value }
        | None -> failwithf "%s is not a valid code for a series" seriesName

    /// Adds start values to the test settings. Overwrites any existing
    /// start values that may already exist.
    let addStartValues values settings =
        Seq.fold (fun set (s, v) -> addStartValue s v set) settings values

    let withTimeSeriesLength n settings = { settings with TimeSeriesLength = n }
    let withFixedTemporalResolution res settings = { settings with Resolution = res }
    let endWhen goal settings = { settings with EndCondition = goal }
    let useRandom rnd (settings: TestSettings<_, _, _, _>) = { settings with Random = rnd }
    let useStartTime time settings = { settings with StartDate = time }

    let useDateMode dateMode startDate settings =
        { settings with
            StartDate = startDate
            DateMode = dateMode }

    module Compute =

        let tryMakeDummySeries<[<Measure>] 'state, 'T, 'yearType, [<Measure>] 'timeunit, 'timespan when 'T: comparison>
            (startDate: 'T)
            (resolution: Resolution.FixedTemporalResolution<'timespan>)
            (length: int)
            (eqs: ModelForm<'timeunit>)
            (dateMode: DateMode.DateMode<'T, 'yearType, 'timespan>)
            =

            let ts =
                TimeSeries.fromSeq
                    dateMode
                    startDate
                    resolution
                    (Seq.init length (fun _ -> nan |> LanguagePrimitives.FloatWithMeasure<'state>))

            let stateNames =
                match eqs with
                | DifferenceEqs e -> Map.keys e
                | DifferentialEqs e -> Map.keys e

            stateNames |> Seq.map (fun s -> s, ts) |> Map.ofSeq |> TimeFrame.tryCreate

        /// Generate time-series for a given engine and model, and
        /// for a particular point in optim-space.
        let tryGenerateData'
            engine
            (model: ModelSystem<'modelTimeUnit>)
            (testSettings: TestSettings<'T, 'date, 'timeunit, 'timespan>)
            thetaPool
            =

            // Build environment index
            let envSeries =
                testSettings.EnvironmentalData
                |> Map.map (fun k v ->
                    v
                    |> TimeSeries.map (fun (v, _) -> v |> Units.removeUnitFromFloat |> (*) 1.<environment>))
                |> TimeFrame.tryCreate

            // Setup dummy timeline for solver (all values = nan)
            let dynSeries =
                tryMakeDummySeries
                    testSettings.StartDate
                    testSettings.Resolution
                    testSettings.TimeSeriesLength
                    model.Equations
                    testSettings.DateMode
                |> Option.get

            let t0 =
                testSettings.StartValues
                |> Map.map (fun _ v -> v |> Units.removeUnitFromFloat |> (*) 1.<state> |> Tensors.Typed.ofScalar)

            let dynamicKeys = dynSeries.Keys

            let conditioned =
                Solver.Conditioning.resolve engine.Conditioning dynSeries envSeries dynamicKeys

            let obsTimes = conditioned.ObservedForPairing |> TimeFrame.dates

            // Configure solver
            let solver =
                Solver.SolverCompiler.compile
                    ignore
                    engine.ToModelTime
                    model.Equations
                    engine.TimeHandling
                    (Solver.StepType.External obsTimes)
                    conditioned.StatesObservedForSolver
                    conditioned.StatesHiddenForSolver
                    conditioned.ExogenousForSolver
                    (fun _ -> Solver.Exact)

            // Get real-space vector from pool
            let _, thetaReal = Parameter.Pool.toTensorWithKeysReal thetaPool

            // Predict series
            let timeline =
                dynSeries.Series
                |> Seq.head
                |> fun kv -> kv.Value |> TimeSeries.toObservations |> Seq.map snd |> Seq.toArray

            let predicted =
                Objective.predict solver model.Measures thetaReal
                |> Map.map (fun _ v ->
                    Tensors.Typed.toFloatArray v
                    |> Array.map (Units.removeUnitFromFloat >> LanguagePrimitives.FloatWithMeasure<'T>)
                    |> Array.zip timeline
                    |> Array.map (fun (a, b) -> b, a)
                    |> TimeSeries.fromObservations testSettings.DateMode)

            // Add noise if needed
            let noisy = testSettings.NoiseGeneration testSettings.Random thetaPool predicted
            noisy


        /// Generate data and check that it complies with the
        /// given ruleset.
        let rec tryGenerateData
            (engine: EstimationEngine.EstimationEngine<'timespan, 'modelTimeUnit, 'state>)
            (settings: TestSettings<'state, 'date, 'timeunit, 'timespan>)
            (model: ModelSystem.ModelSystem<'modelTimeUnit>)
            attempts
            =
            let randomPool = Parameter.Pool.drawRandom engine.Random model.Parameters

            match tryGenerateData' engine model settings randomPool with
            | Ok series ->

                engine.LogTo <| Logging.GeneralEvent(sprintf "Series = %A" series)

                let extraFiniteRules =
                    series
                    |> Map.keys
                    |> Seq.map (fun s -> s.Value |> GenerationRules.alwaysFinite)
                    |> Seq.toList

                let rules = extraFiniteRules @ settings.GenerationRules

                let rulesPassed =
                    rules
                    |> List.choose (fun (key, ruleFn) ->
                        series
                        |> Map.tryFindKey (fun k _ -> k.Value = key)
                        |> Option.map (fun k ->
                            Map.find k series |> TimeSeries.toObservations |> Seq.map fst |> ruleFn))

                if (rulesPassed |> List.contains false) || rulesPassed.Length <> rules.Length then
                    if attempts = 0 then
                        Error "Could not generate data that complies with the given ruleset"
                    else
                        tryGenerateData engine settings model (attempts - 1)
                else
                    Ok(series, randomPool)
            | Error e -> Error e
