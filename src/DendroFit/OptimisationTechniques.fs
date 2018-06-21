module OptimisationTechniques

open Types
open System
open Optimisation.Amoeba
open Solver
open Time

type OptimisationError =
| OutOfBounds
| ModelError
| LikelihoodError

// Helper Functions
let containsNan s : bool = s |> Seq.exists (Double.IsNaN)
let private rnd = Random()


module Bootstrap =

    let removeSingle (data:CodedMap<TimeSeries<float<'v>>>) =
        let commonTimeSeries = TimeSeries.commonTimeline (data |> Map.toList |> List.map snd)
        match commonTimeSeries with
        | None -> invalidOp "The time series are not on common time"
        | Some ts ->
            match ts.Length with
            | 0 -> invalidOp "The time series was empty"
            | _ ->
                let selection = rnd.Next(0, ts.Length - 1)
                data |> Map.map (fun _ v -> v |> TimeSeries.bootstrapFixedStep (TimeSpan.FromDays(366.)) selection )


module HeuristicOptimisation =
    let rec heuristicOptimisation numberOfLevels iterationsPerLevel numberOfAomeba (paramBounds:Domain) (f:Point->float) =
            
        // let aomebaResults = 
        //     [|1 .. numberOfAomeba|]
        //     |> Array.collect (fun _ -> 
        //         try [|solve Default paramBounds f iterationsPerLevel|]
        //         with | _ -> [||] )
        let aomebaResults =
            [| 1 .. numberOfAomeba |]
            |> Array.map (fun _ -> solve Default paramBounds f iterationsPerLevel)

        let mostLikely = aomebaResults |> Array.minBy fst

        // Drop worst 20% of likelihoods
        let percentile80thRank = int (Math.Floor (float (80. / 100. * (float aomebaResults.Length + 1.))))
        let percentile80thValue = fst aomebaResults.[percentile80thRank - 1]
        printfn "80th percentile = %f" percentile80thValue
        let ranked = aomebaResults |> Array.filter (fun x -> (fst x) <= percentile80thValue)

        let dims = Array.length paramBounds

        let boundsList = ranked |> Array.map snd

        let getBounds (dim:int) (points:Point array) =
            let max = points |> Array.maxBy (fun p -> p.[dim])
            let min = points |> Array.minBy (fun p -> p.[dim])
            printfn "Min %f Max %f" min.[dim] max.[dim]
            min.[dim],max.[dim]

        let bounds =
            [|0 .. dims - 1|]
            |> Array.map (fun dim -> (boundsList |> getBounds dim))

        let boundWidth =
            bounds
            |> Array.sumBy (fun dim -> snd dim - fst dim)
        printfn "Bound width: %f" boundWidth 
        
        if (numberOfLevels > 1 && boundWidth > 0.01) 
            then heuristicOptimisation (numberOfLevels-1) iterationsPerLevel numberOfAomeba bounds f
            else mostLikely

    
let toParamList (domainplist:ParameterPool) (p:Point) : ParameterPool =
    let plist = Map.toList domainplist
    [0 .. Array.length p - 1]
    |> List.map (fun i -> (fst plist.[i]), setEstimate (snd plist.[i]) p.[i])
    |> Map.ofList

let toDomain (p:ParameterPool) : Domain =
    p |> Map.toArray |> Array.map (fst >> getBoundsForEstimation p)

let fromPoint (pool:ParameterPool) (point:Point) : ParameterPool =
    if pool.Count = point.Length
    then
        pool 
        |> Map.toList
        |> List.mapi (fun i (sc,p) -> sc, Parameter.setEstimate p point.[i] )
        |> Map.ofList
    else
        invalidOp "The number of parameters estimated differs from those in the parameter pool"