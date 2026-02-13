namespace Bristlecone

module Test =

    open System
    open Time
    open ModelSystem
    open EstimationEngine

    /// Generate synthetic time-series using mathematical
    /// functions, for example for environmental data inputs.
    module Synthetic =

        let sinusoid<[<Measure>] 'time, [<Measure>] 'u>
            (mean: float<'u>)
            (amplitude: float<'u>)
            (period: float<'time>)
            phase
            =
            fun (t: float<'time>) ->
                let x = t / period + phase
                mean + amplitude * sin (2.0 * Math.PI * x)

        let ar1 phi sigma (rnd: Random) =
            let rec loop prev =
                seq {
                    let eps = sigma * (rnd.NextDouble() - 0.5)
                    let next = phi * prev + eps
                    yield next
                    yield! loop next
                }

            loop 0.


    /// Functions for adding background variability into
    /// test problems.
    module ObservationError =

        /// Adds noise to a time-series 'seriesName', based on the given distribution function.
        let internal tryAddNoise
            seriesName
            (addNoiseFn: float<'s> -> float<'s>)
            (data: CodedMap<TimeSeries.TimeSeries<float, 'date, 'timeunit, 'timespan>>)
            =
            data
            |> Map.tryFindKey (fun c _ -> c = seriesName)
            |> Option.map (fun k ->
                data
                |> Map.find k
                |> TimeSeries.map (fun (x, _) -> x |> Units.tagUnit<'s> |> addNoiseFn |> Units.removeUnitFromFloat)
                |> fun ts -> Map.add k ts data)

        let internal addNoise series fn data =
            match tryAddNoise series fn data with
            | Some ts -> ts
            | None -> failwithf "Could not add noise. Time series %s missing" series.Value

        let normal rnd (sd: float<'s>) =
            let draw = Statistics.Distributions.Normal.draw rnd (Units.tagUnit<'s> 0.) sd
            fun x -> x + draw ()

        let logNormal rnd sd =
            let draw = Statistics.Distributions.Normal.draw rnd 0. sd
            fun x -> x * exp (draw ())

    type GenerationRule<[<Measure>] 's> = (seq<float<'s>> -> bool)

    module GenerationRules =

        /// Ensures that all generated values are less than i
        let alwaysLessThan (i: float<'u>) : GenerationRule<'s> = fun data -> data |> Seq.max < i

        /// Ensures that all generated values are greater than i
        let alwaysMoreThan i : GenerationRule<'s> = fun data -> data |> Seq.min > i

        let between iLow iHi : GenerationRule<'s> =
            fun data -> Seq.min data > iLow && Seq.max data < iHi

        let alwaysFinite: GenerationRule<'s> =
            fun data -> data |> Seq.exists Units.isNotFinite |> not

        /// Ensures that there is always a positive change in values of a variable
        let monotonicallyIncreasing: GenerationRule<'s> =
            fun data ->
                data
                |> Seq.pairwise
                |> Seq.map (fun (x1, x2) -> (x2 - x1) > LanguagePrimitives.FloatWithMeasure<'state> 0.)
                |> Seq.contains false


    type TestSettings<[<Measure>] 'stateUnit, 'date, 'yearUnit, 'timespan> =
        { TimeSeriesLength: int
          StartValues: CodedMap<float<'stateUnit>>
          GenerationRules: (ShortCode.ShortCode * GenerationRule<'stateUnit>) list
          ObservationErrorFn:
              Random
                  -> Parameter.Pool.ParameterPool
                  -> CodedMap<TimeSeries<float<'stateUnit>, 'date, 'yearUnit, 'timespan>>
                  -> CodedMap<TimeSeries<float<'stateUnit>, 'date, 'yearUnit, 'timespan>>
          EnvironmentalData: CodedMap<TimeSeries<float<'stateUnit>, 'date, 'yearUnit, 'timespan>>
          Resolution: Resolution.FixedTemporalResolution<'timespan>
          StartDate: 'date
          RetryDataGen: int
          DateMode: DateMode.DateMode<'date, 'yearUnit, 'timespan> }

        static member mkSettings (dateMode: DateMode.DateMode<'date, 'yearType, 'timespan>) startDate =
            { Resolution = Resolution.FixedTemporalResolution.Years (PositiveInt.create 1<year>).Value
              TimeSeriesLength = 30
              StartValues = Map.empty
              GenerationRules = []
              ObservationErrorFn = fun _ _ s -> s
              EnvironmentalData = Map.empty
              StartDate = startDate
              RetryDataGen = 100
              DateMode = dateMode }

        static member Default: TestSettings<1, DateTime, int<year>, TimeSpan> =
            TestSettings<_, _, _, _>.mkSettings DateMode.calendarDateMode (DateTime(1970, 01, 01))

        static member Annual: TestSettings<1, DatingMethods.Annual, int<year>, int<year>> =
            TestSettings<_, _, _, _>.mkSettings DateMode.annualDateMode (DatingMethods.Annual 1970<year>)

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
          EstimatedLikelihood: float<``-logL``>
          Trace: Trace list }

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
            rnd
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
                Solver.Conditioning.resolve engine.Conditioning dynSeries envSeries dynamicKeys model.Measures.Keys

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
                    conditioned.MeasuresForSolver
                    conditioned.StatesHiddenForSolver
                    model.Initialisers
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
            let noisy = testSettings.ObservationErrorFn rnd thetaPool predicted
            noisy


        /// Generate data and check that it complies with the
        /// given ruleset.
        let rec tryGenerateData
            (engine: EstimationEngine.EstimationEngine<'date, 'timespan, 'modelTimeUnit, 'state>)
            (settings: TestSettings<'state, 'date, 'timeunit, 'timespan>)
            (model: ModelSystem.ModelSystem<'modelTimeUnit>)
            attempts
            =
            let randomPool = Parameter.Pool.drawRandom engine.Random model.Parameters

            let series = tryGenerateData' engine model settings engine.Random randomPool

            engine.LogTo <| Logging.GeneralEvent(sprintf "Series = %A" series)

            let extraFiniteRules =
                series
                |> Map.keys
                |> Seq.map (fun s -> s, GenerationRules.alwaysFinite)
                |> Seq.toList

            let rules = extraFiniteRules @ settings.GenerationRules

            let rulesPassed =
                rules
                |> List.choose (fun (key, ruleFn) ->
                    series
                    |> Map.tryFindKey (fun k _ -> k = key)
                    |> Option.map (fun k -> Map.find k series |> TimeSeries.toObservations |> Seq.map fst |> ruleFn))

            if rulesPassed |> List.contains false || rulesPassed.Length <> rules.Length then
                if attempts = 0 then
                    Error "Could not generate data that complies with the given ruleset"
                else
                    tryGenerateData engine settings model (attempts - 1)
            else
                Ok(series, randomPool)
