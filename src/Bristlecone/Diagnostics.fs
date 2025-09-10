/// <summary>Diagnostic techniques for determining the suitability of
/// results obtained with Bristlecone.</summary>
module Bristlecone.Diagnostics

open Bristlecone.ModelSystem

/// <summary>Convergence diagnostics for monte-carlo markov chain (MCMC) analyses.</summary>
module Convergence =

    open Bristlecone.ModelSelection

    /// <summary>A per-parameter convergence statistic. The statistic used is given in `StatisticName`.</summary>
    type ConvergenceStatistic =
        { Subject: string
          HypothesisId: string
          Parameter: ShortCode.ShortCode
          StatisticName: string
          StatisticValue: float }

    /// <summary>Calculate the Gelman-Rubin statistic for each parameter in the given
    /// `ResultSet`. The statistic tends downwards to one, with one indicating
    /// perfect convergence between all chains.</summary>
    /// <param name="nMostRecent">How many recent iterations to use from the trace.</param>
    /// <param name="subjectId">The subject identifier</param>
    /// <param name="hypothesisId">The hypothesis identifier</param>
    /// <param name="result">A result set (of 1 .. many results) for a particular subject and hypothesis</param>
    /// <returns>If more than one replicate, the R-hat convergence statistic across replicates</returns>
    let gelmanRubin
        nMostRecent
        subjectId
        hypothesisId
        (result: ResultSet.ResultSet<'subject, 'hypothesis, 'date, 'timeunit, 'timespan>)
        =
        printfn "Calculating Rhat for %s (H = %s)" subjectId hypothesisId

        if result.AllResults |> Seq.isEmpty || result.BestResult.IsNone then
            None
        else
            let chains =
                result.AllResults
                |> Seq.sortBy (fun x -> x.Likelihood)
                |> (fun c ->
                    if c |> Seq.length > nMostRecent then
                        c |> Seq.take nMostRecent
                    else
                        c)
                |> Seq.map (fun r -> r.Trace |> Seq.map snd)

            let minChainLength = chains |> Seq.minBy (fun x -> x |> Seq.length) |> Seq.length

            if (chains |> Seq.length) < 2 then
                None
            else
                let chains' = chains |> Seq.map (fun c -> c |> Seq.take minChainLength)

                result.BestResult.Value.Parameters
                |> Parameter.Pool.toList
                |> Seq.mapi (fun i (code, p) ->
                    { Subject = subjectId
                      HypothesisId = hypothesisId
                      Parameter = code
                      StatisticName = "R-hat"
                      StatisticValue =
                        chains'
                        |> Seq.map (fun c -> c |> Seq.map (fun x -> x.[i] |> Units.removeUnitFromFloat))
                        |> Statistics.Convergence.GelmanRubin.rHat })
                |> Some

    /// <summary>Calculate the Gelman-Rubin statistic for each parameter in all of the given
    /// `ResultSet`. The statistic tends downwards to one, with one indicating
    /// perfect convergence between all chains.</summary>
    /// <param name="nMostRecent">How many recent iterations to use from the trace.</param>
    /// <param name="subject">A function to retrieve a subject ID from a subject</param>
    /// <param name="hypothesis">A function to retrieve a hypothesis ID from a hypothesis</param>
    /// <param name="result">A result set (of 1 .. many results) for a particular subject and hypothesis</param>
    /// <returns>If more than one replicate, the R-hat convergence statistic across replicates</returns>
    let gelmanRubinAll
        nMostRecent
        subject
        hypothesis
        (results: ResultSet.ResultSet<'subject, 'hypothesis, 'date, 'timeunit, 'timespan> seq)
        =
        results
        |> Seq.choose (fun r -> gelmanRubin nMostRecent (subject r.Subject) (hypothesis r.Hypothesis) r)
        |> Seq.concat


/// <summary>Logging functions to output the internal dynamics of model systems.</summary>
module ModelComponents =

    open Bristlecone.ModelSelection
    open Bristlecone.EstimationEngine

    /// <summary>Contains types that can collect internal
    /// components of a model during computation.</summary>
    module Loggers =

        /// <summary>A component logger can store value for a
        /// component's value at a particular time.</summary>
        type IComponentLogger<'data> =
            abstract member StoreValue: componentName: string -> time: float -> value: 'data -> 'data
            abstract member GetAll: unit -> Map<string, Map<float, 'data>>

        /// <summary>A component logger stores the value at each time t for each
        /// component specified by a `componentId`.</summary>
        type ComponentLogger<'data>() =
            let mutable (data: Map<string, Map<float, 'data>>) = [] |> Map.ofList

            interface IComponentLogger<'data> with

                member __.StoreValue componentId t v =
                    let existing = data |> Map.tryFind componentId

                    match existing with
                    | Some e -> data <- data |> Map.add componentId (e |> Map.add t v)
                    | None -> data <- data |> Map.add componentId ([ t, v ] |> Map.ofList)

                    v

                member __.GetAll() = data

        /// <summary>A component logger that does not store any values.</summary>
        type PassThrough<'data>() =
            interface IComponentLogger<'data> with
                member __.StoreValue _ _ v = v
                member __.GetAll() = [] |> Map.ofList

    // /// <summary>Log out components specified in a model by disabling optimisation. The model will only be computed once.</summary>
    // /// <param name="fitFn">The function used</param>
    // /// <param name="engine"></param>
    // /// <param name="result"></param>
    // /// <typeparam name="'subject"></typeparam>
    // /// <typeparam name="'a"></typeparam>
    // /// <typeparam name="'b"></typeparam>
    // /// <typeparam name="'data"></typeparam>
    // /// <returns></returns>
    // let calculateComponents
    //     fitFn
    //     engine
    //     (result:
    //         ResultSet.ResultSet<
    //             'subject,
    //             Loggers.IComponentLogger<'data> -> ModelSystem<'dataUnit, 'timeIndex>,
    //             'date,
    //             'timeunit,
    //             'timespan
    //          >)
    //     =
    //     match result.BestResult with
    //     | None -> [] |> Map.ofList
    //     | Some mle ->
    //         let eng =
    //             { engine with
    //                 OptimiseWith = Optimisation.None.none }

    //         let cLog = Loggers.ComponentLogger<'data>() :> Loggers.IComponentLogger<'data>

    //         let p =
    //             mle.Parameters
    //             |> Parameter.Pool.toList
    //             |> List.choose (fun (k, v) ->
    //                 Parameter.create
    //                     Parameter.Constraint.Unconstrained
    //                     (v |> Parameter.getTransformedValue)
    //                     (v |> Parameter.getTransformedValue)
    //                 |> Option.map (fun v -> k, v))
    //             |> Parameter.Pool.fromList

    //         let mleHypothesis =
    //             { result.Hypothesis cLog with
    //                 Parameters = p }

    //         fitFn result.Subject mleHypothesis eng |> ignore
    //         cLog.GetAll()
