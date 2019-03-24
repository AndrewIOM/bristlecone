namespace Bristlecone.Optimisation

open System
open Bristlecone

type OptimisationError =
    | OutOfBounds
    | ModelError
    | LikelihoodError

module Bootstrap =

    let removeSingle (random:System.Random) (data:CodedMap<TimeSeries<float<'v>>>) =
        let commonTimeSeries = TimeSeries.commonTimeline (data |> Map.toList |> List.map snd)
        match commonTimeSeries with
        | None -> invalidOp "The time series are not on common time"
        | Some ts ->
            match ts.Length with
            | 0 -> invalidOp "The time series was empty"
            | _ ->
                let selection = random.Next(0, ts.Length - 1)
                data |> Map.map (fun _ v -> v |> TimeSeries.bootstrapFixedStep (TimeSpan.FromDays(366.)) selection )


module ParameterPool =

    let toParamList (domainplist:ParameterPool) (p:Point<float>) : ParameterPool =
        let plist = Map.toList domainplist
        [0 .. Array.length p - 1]
        |> List.map (fun i -> (fst plist.[i]), setEstimate (snd plist.[i]) p.[i])
        |> Map.ofList

    let toDomain (optimisationConstraints:Constraint list) (pool:ParameterPool) : Domain =
        let x,y = 
            pool
            |> Map.toList
            |> List.map (snd >> Parameter.bounds)
            |> List.unzip
        List.zip3 x y optimisationConstraints |> List.toArray

    let fromPoint (pool:ParameterPool) (point:Point<float>) : ParameterPool =
        if pool.Count = point.Length
        then
            pool 
            |> Map.toList
            |> List.mapi (fun i (sc,p) -> sc, Parameter.setEstimate p point.[i] )
            |> Map.ofList
        else
            invalidOp "The number of parameters estimated differs from those in the parameter pool"