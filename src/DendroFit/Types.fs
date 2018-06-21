module Types

open Time.TimeSeries
open Time

let removeUnit (x:float<_>) =
    float x

// Year
[<Measure>] type year

// Millimetre
[<Measure>] type mm

[<AutoOpen>]
module GrowthSeries =

    type GrowthSeries<[<Measure>] 'u> =
    | Cumulative of TimeSeries<float<'u>>
    | Absolute of TimeSeries<float<'u>>
    | Relative of TimeSeries<float<'u>>

    let growthToTime growth =
        match growth with
        | Absolute g -> g
        | Cumulative g -> g
        | Relative g -> g


[<AutoOpen>]
module ShortCode =

    type ShortCode = private ShortCode of string
    let unwrap (ShortCode n) = n
    let create str = str |> ShortCode

    type ShortCode with
        member this.Value = unwrap this

type CodedMap<'T> = Map<ShortCode,'T>


module EnvironmentalVariables =

    type RegionalEnvironment = CodedMap<TimeSeries<float>>
    type LocalEnvironment = CodedMap<TimeSeries<float>>


module PlantIndividual =

    open EnvironmentalVariables

    type PlantGrowth =
    | RingWidth of GrowthSeries<mm>
    | BasalArea of GrowthSeries<mm^2>
    | StemVolume of GrowthSeries<mm^3>

    type Trait =
    | Static of float
    | Variable of TimeSeries<float>

    type PlantIndividual = {
        Identifier: ShortCode
        Growth: PlantGrowth
        InternalControls: Map<ShortCode,Trait>
        Environment: LocalEnvironment
    }

    let zipEnv envName envData (plant:PlantIndividual) =
        { plant with Environment = plant.Environment.Add (envName, envData) }

    let growthSeries plant =
        match plant with
        | RingWidth rw ->
            match rw with
            | Absolute rws -> TimeSeries.map rws (fun (x,t) -> removeUnit x, t) |> Absolute
            | Cumulative m -> TimeSeries.map m (fun (x,t) -> removeUnit x, t) |> Cumulative
            | Relative rws -> TimeSeries.map rws (fun (x,t) -> removeUnit x, t) |> Relative
        | _ -> invalidOp "Not implemented"

    let private toCumulativeGrowth' (growth:GrowthSeries<_>) =
        match growth with
        | Absolute g ->
            let time = g |> toSeq |> Seq.map snd |> Seq.scan (+) (g |> TimeSeries.start)
            let agr = g |> toSeq |> Seq.map fst
            let biomass = agr |> Seq.scan (+) 0.<_> |> Seq.tail |> Seq.toList
            TimeSeries.createVarying (Seq.zip time biomass) |> Cumulative
        | Relative _ -> failwith "Not implemented"
        | Cumulative _ -> growth

    let private toRelativeGrowth' (growth:GrowthSeries<_>) =
        match growth with
        | Absolute g -> 
            let time = g |> toSeq |> Seq.map snd |> Seq.scan (+) (g |> TimeSeries.start)
            let agr = g |> toSeq |> Seq.map fst
            let biomass = agr |> Seq.scan (+) 0.<_> |> Seq.tail |> Seq.toList
            let rgr = agr |> Seq.mapi (fun i a -> a / biomass.[i] * 1.<_> )
            TimeSeries.createVarying (Seq.zip time rgr) |> Relative
        | Relative _ -> growth
        | Cumulative g -> 
            // Calculate agr and divide agr by biomass
            failwith "Not implemented"

    let toRelativeGrowth (plant:PlantIndividual) =
        match plant.Growth with
        | RingWidth s -> { plant with Growth = s |> toRelativeGrowth' |> RingWidth }
        | _ -> invalidOp "Not implemented"

    let toCumulativeGrowth (plant:PlantIndividual) =
        match plant.Growth with
        | RingWidth s -> { plant with Growth = s |> toCumulativeGrowth' |> RingWidth }
        | _ -> invalidOp "Not implemented"

    let keepCommonYears (plant:PlantIndividual) =
        let allSeries = 
            let response = plant.Growth |> growthSeries |> growthToTime
            let envSeries = plant.Environment |> Map.toList |> List.map snd
            response :: envSeries
        let startDates, endDates =
            allSeries
            |> List.map (fun s -> TimeSeries.start s, TimeSeries.endDate s)
            |> List.unzip
        let startDate = startDates |> List.max
        let endDate = endDates |> List.min
        let mapts f s = TimeSeries.map s f
        printfn "Start = %A End = %A" startDate endDate
        { plant with Environment = plant.Environment |> Map.toList |> List.map (fun (x,y) -> x,y |> TimeSeries.bound startDate endDate) |> Map.ofList
                     Growth = plant.Growth |> growthSeries |> growthToTime |> TimeSeries.bound startDate endDate |> mapts (fun (x,t) -> x * 1.<mm>, t) |> Absolute |> RingWidth }


[<AutoOpen>]
module Parameter =

    type EstimationStartingBounds = float * float

    type Constraint =
    | Unconstrained
    | PositiveOnly

    type Estimation =
    | NotEstimated of EstimationStartingBounds
    | Estimated of float

    type Parameter = private Parameter of Constraint * Estimation

    let private unwrap (Parameter (c,e)) = c,e

    let create con bound1 bound2 =
        Parameter (con, (NotEstimated ([bound1; bound2] |> Seq.min, [bound1; bound2] |> Seq.max)))

    let setEstimate parameter value =
        let c,_ = parameter |> unwrap
        match c with
        | Unconstrained -> Parameter (Unconstrained, Estimated value)
        | PositiveOnly -> Parameter (PositiveOnly, Estimated (exp value))

    let bounds (p:Parameter) : float * float =
        let c,estimate = p |> unwrap
        match estimate with
        | NotEstimated (s,e) ->
            match c with
            | Unconstrained -> s,e
            | PositiveOnly -> log s, log e
        | Estimated _ -> invalidOp "Already estimated"

    let getEstimate (p:Parameter) : float =
        let c,estimate = p |> unwrap
        match estimate with
        | NotEstimated _ -> invalidOp (sprintf "Oops: Parameter not available")
        | Estimated v ->
            match c with
            | Unconstrained -> v
            | PositiveOnly -> v

    [<AutoOpen>]
    module Pool =

        type ParameterPool = CodedMap<Parameter>

        let getEstimate key (pool:ParameterPool) : float =
            let c,estimate = pool |> Map.find (ShortCode.create key) |> unwrap
            match estimate with
            | NotEstimated _ -> invalidOp (sprintf "Oops: Parameter %s not available" key)
            | Estimated v ->
                match c with
                | Unconstrained -> v
                | PositiveOnly -> v

        let getBoundsForEstimation (pool:ParameterPool) key : float * float =
            let c,estimate = pool |> Map.find key |> unwrap
            match estimate with
            | NotEstimated (s,e) ->
                match c with
                | Unconstrained -> s,e
                | PositiveOnly -> log s, log e
            | Estimated _ -> invalidOp "Already estimated"


module ParameterEstimation =

    // Models 
    type Response = float
    type Environment = CodedMap<float>
    type Time = float
    type ModelEquation = ParameterPool -> Time -> Response -> Environment -> float

    // Likelihood
    type PredictedSeries = {
        Expected: float[]
        Observed: float[] }
    type Likelihood = ParameterPool -> CodedMap<PredictedSeries> -> float

    type ModelSystem = {
        Parameters: ParameterPool
        Equations: CodedMap<ModelEquation>
        Likelihood: Likelihood }

    type EstimationResult = {
        Likelihood: float
        Parameters: ParameterPool
        Series: CodedMap<PredictedSeries>
        Trace: float list * float [] list }

type StartingValues =
| FirstDataItem
| Custom of CodedMap<float>


module Seq =

    ///Groups two sequences together by key
    let align a b = 

        let simplifyEntry (key, values) =
            let matches = [for value in values -> snd value]
            key, matches

        a 
        |> Seq.append b
        |> Seq.groupBy fst
        |> Seq.map simplifyEntry
        |> Seq.toList


    ///**Description**
    /// Unifies two sequences into a tuple based on a string key
    let keyMatch (a:(string*'a)seq) (b:(string*'b)seq) =
        a
        |> Seq.choose (fun (s,x) -> 
            b 
            |> Seq.tryFind(fun (s2,_) -> s2 = s)
            |> Option.bind (fun f -> Some (s,x,snd f)))

    let private sqr x = x * x

    let stddev nums =
        let mean = nums |> List.average
        let variance = nums |> List.averageBy (fun x -> sqr(x - mean))
        sqrt(variance)


module Map =

    let merge group1 group2 appender = 
        group1 |> Seq.fold(fun (acc:Map<'a,'b>) (KeyValue(key, values)) -> 
                          match acc.TryFind key with
                                            | Some items -> Map.add key (appender values items) acc
                                            | None -> Map.add key values acc) group2
