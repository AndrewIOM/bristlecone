module Types

open Time.TimeSeries

// Year
[<Measure>] type year

// Millimetre
[<Measure>] type mm

type GrowthSeries<[<Measure>] 'u> =
| Cumulative of TimeSeries<float<'u>>
| Absolute of TimeSeries<float<'u>>

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


[<AutoOpen>]
module ParameterPool =

    type EstimationStartingBounds = float * float

    type Constraint =
    | Unconstrained
    | PositiveOnly

    type Estimation =
    | NotEstimated of EstimationStartingBounds
    | Estimated of float

    type Parameter = Parameter of Constraint * Estimation
    let private unwrapP (Parameter (c,e)) = c,e

    type ParameterPool = CodedMap<Parameter>

    let getEstimate key (pool:ParameterPool) : float =
        let c,estimate = pool |> Map.find (ShortCode.create key) |> unwrapP
        match estimate with
        | NotEstimated _ -> invalidOp (sprintf "Oops: Parameter %s not available" key)
        | Estimated v ->
            match c with
            | Unconstrained -> v
            | PositiveOnly -> exp v

    let getBoundsForEstimation (pool:ParameterPool) key : float * float =
        let c,estimate = pool |> Map.find key |> unwrapP
        match estimate with
        | NotEstimated (s,e) ->
            match c with
            | Unconstrained -> s,e
            | PositiveOnly -> log s, log e
        | Estimated _ -> invalidOp "Already estimated"

    let setEstimate parameter value =
        let c,_ = parameter |> unwrapP
        match c with
        | Unconstrained -> Parameter (Unconstrained, Estimated value)
        | PositiveOnly -> Parameter (PositiveOnly, Estimated value)


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

let removeUnit (x:float<_>) =
    float x


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
