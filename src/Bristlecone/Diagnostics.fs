/// Diagnostic techniques for determining the suitability of 
/// results obtained with Bristlecone.
module Bristlecone.Diagnostics

open Bristlecone.ModelSystem

/// Convergence diagnostics for monte-carlo markov chain (MCMC) analyses.
module Convergence =

    open Bristlecone.ModelSelection

    /// A per-parameter convergence statistic. The statistic used is given in `StatisticName`.
    type ConvergenceStatistic = {
        Subject: string
        HypothesisId: int
        Parameter: ShortCode.ShortCode
        StatisticName: string
        StatisticValue: float
    }

    /// Calculate the Gelman-Rubin statistic for each parameter in the given
    /// `ResultSet`. The statistic tends downwards to one, with one indicating
    /// perfect convergence between all chains.
    let gelmanRubin n (result:ResultSet.ResultSet<string,ModelSystem>) =
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
                mle.Parameters
                |> Parameter.Pool.asList
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

    /// Calculate the Gelman-Rubin statistic for each parameter in all of the
    /// given `ResultSet`. The statistic tends downwards to one, with one indicating
    /// perfect convergence between all chains.
    let gelmanRubinAll n (results:ResultSet.ResultSet<string,ModelSystem> list) =
        results |> Seq.choose (gelmanRubin n)


/// Logging functions to output the internal dynamics of model systems.
module ModelComponents =

    open Bristlecone.ModelSelection
    open Bristlecone.EstimationEngine

    type IComponentLogger<'data> =
        abstract member StoreValue: string -> float -> 'data -> 'data
        abstract member GetAll: unit -> Map<string,Map<float,'data>>

    /// A component logger stores the value at each time t for each
    /// component specified by a `componentId`.
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

    /// A component logger that does not store any values.
    type PassThrough<'data>() =
        interface IComponentLogger<'data> with
            member __.StoreValue c t v = v
            member __.GetAll() = [] |> Map.ofList

    /// Log out components specified in a model by disabling optimisation.
    /// The model will only be computed once. 
    let calculateComponents fitFn engine (result:ResultSet.ResultSet<'subject,IComponentLogger<'data>->ModelSystem>) =
        let subject,hypothesis,_,estimate = result
        match estimate with
        | None -> [] |> Map.ofList
        | Some (_,mle) ->
            let eng = { engine with OptimiseWith = Optimisation.None.passThrough }
            let cLog = ComponentLogger<'data>() :> IComponentLogger<'data>
            let p = 
                mle.Parameters 
                |> Parameter.Pool.asList
                |> List.map(fun (k,v) -> k, Parameter.create Parameter.Constraint.Unconstrained (v |> Parameter.getEstimate) (v |> Parameter.getEstimate))
                |> Parameter.Pool.fromList
            let mleHypothesis = { hypothesis cLog with Parameters = p }
            fitFn subject mleHypothesis eng |> ignore
            cLog.GetAll()
