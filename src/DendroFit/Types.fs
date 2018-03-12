module Types

open Time.TimeSeries
open System.Collections

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

    type Parameter =
    | NotEstimated of EstimationStartingBounds
    | Estimated of float

    type ParameterPool = CodedMap<Parameter>

    let value key pool : float =
        let p = pool |> Map.find (ShortCode.create key)
        match p with
        | NotEstimated _ -> invalidOp (sprintf "Oops: Parameter %s not available" key)
        | Estimated v -> v


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
