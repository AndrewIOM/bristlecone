module Bristlecone.Diagnostics

open Bristlecone.ModelSystem
open Bristlecone.ModelSelection.ResultSet

/// Convergence diagnostics for monte-carlo markov chain (MCMC) analayses.
module Convergence =

    type ConvergenceStatistic = {
        Subject: string
        HypothesisId: int
        Parameter: ShortCode
        StatisticName: string
        StatisticValue: float 
    }

    let gelmanRubin nBurn n (result:ResultSet<string,ModelSystem>) =
        let subjectId,_,hi,r = result
        printfn "Calculating Rhat for %s %i" subjectId hi
        match r with
        | Some (results, mle) ->
            let chains = 
                results
                |> Seq.sortBy(fun x -> x.Likelihood)
                |> (fun c -> if c |> Seq.length > n then c |> Seq.take n else c)
                |> Seq.map(fun r -> r.Trace |> Seq.map snd)
            let minChainLength = chains |> Seq.minBy(fun x -> x |> Seq.length) |> Seq.length
            if (chains |> Seq.length) < 2 then None
            else
                let chains' = chains |> Seq.map(fun c -> c |> Seq.take minChainLength)
                printfn "Using %i chains (with %i common iterations)" (chains |> Seq.length) minChainLength
                mle.Parameters
                |> Map.toSeq
                |> Seq.mapi(fun i (code,p) -> 
                    { Subject = subjectId
                      HypothesisId = hi
                      Parameter = code
                      StatisticName = "rHat"
                      StatisticValue = 
                        chains'
                        |> Seq.map(fun c -> c |> Seq.map(fun x -> x.[i]))
                        |> Statistics.Convergence.GelmanRubin.rHat
                    }) |> Some
        | None -> None

    let gelmanRubinAll nBurn n (results:ResultSet<string,ModelSystem> list) =
        results |> Seq.choose (gelmanRubin nBurn n)


/// Functions to enable logging of internal dynamics within
/// models. 
module ModelComponents =

    open EstimationEngine

    type IComponentLogger<'data> =
        abstract member StoreValue: string -> float -> 'data -> 'data
        abstract member GetAll: unit -> Map<string,Map<float,'data>>

    type ComponentLogger<'data>() =
        let mutable (data:Map<string,Map<float,'data>>) = [] |> Map.ofList
        interface IComponentLogger<'data> with

            member __.StoreValue componentId t v =
                let existing = data |> Map.tryFind componentId
                match existing with
                | Some e -> data <- data |> Map.add componentId (e |> Map.add t v)
                | None -> data <- data |> Map.add componentId ([t,v] |> Map.ofList)
                v

            member __.GetAll() = data

    type PassThrough<'data>() =
        interface IComponentLogger<'data> with
            member __.StoreValue c t v = v
            member __.GetAll() = [] |> Map.ofList

    /// Log out components specified in a model by disabling optimisation.
    /// The model will only be computed once. 
    let calculateComponents fitFn engine (result:ResultSet<'subject,IComponentLogger<'data>->ModelSystem>) =
        let subject,hypothesis,_,estimate = result
        match estimate with
        | None -> [] |> Map.ofList
        | Some (_,mle) ->
            let eng = { engine with OptimiseWith = Optimisation.None.passThrough }
            let cLog = ComponentLogger<'data>() :> IComponentLogger<'data>
            let p = mle.Parameters |> Map.map(fun k v -> Parameter.create Unconstrained (v |> Parameter.getEstimate) (v |> Parameter.getEstimate))
            let mleHypothesis = { hypothesis cLog with Parameters = p }
            fitFn subject mleHypothesis eng |> ignore
            cLog.GetAll()
