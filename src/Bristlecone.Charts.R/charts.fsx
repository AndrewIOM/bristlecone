#I "../Bristlecone/bin/Debug/netstandard2.0"
#I "../../packages/RProvider"

#r "Bristlecone.dll"
#load "RProvider.fsx"

namespace Bristlecone.Logging

type Device =
    | X11
    | PNG

module RHelper =

    open RProvider

    let (>!>) (plot1:RDotNet.SymbolicExpression) (plot2:RDotNet.SymbolicExpression) = 
        R.``+``(plot1, plot2)


module RealTimeTrace =

    open System.Threading
    open Bristlecone.Logging
    open RProvider
    open RProvider.ggplot2
    open RProvider.grDevices
    open RHelper

    let decompose state =
        let likelihood = (state.Iteration, state.Likelihood, "-logL", state.Likelihood)
        state.Theta
        |> Seq.mapi(fun i v -> (state.Iteration, state.Likelihood, sprintf "theta_%i "i, v))
        |> Seq.append [likelihood]

    let convertToDataFrame d =
        let data =
            d
            |> List.collect(fun (k,v) ->
                let ks = k.ToString()
                v |> Seq.toList |> List.collect(fun (a,b,c,d) -> [a,ks,b,c,d]))
        [ "Iteration", data |> List.map(fun (a,_,_,_,_) -> a) |> box
          "ChainId", data |> List.map(fun (_,a,_,_,_) -> a) |> box
          "Likelihood", data |> List.map(fun (_,_,a,_,_) -> a) |> box
          "Parameter", data |> List.map(fun (_,_,_,a,_) -> a) |> box
          "Value", data |> List.map(fun (_,_,_,_,a) -> a) |> box ]
        |> namedParams
        |> R.data_frame

    let facetedTrace data =
        let df = data |> Map.toList |> convertToDataFrame
        R.ggplot(
            namedParams[
                "data", box df;
                "mapping", box (
                    R.aes__string(x="Iteration", y="Value"))])
        >!> R.geom__line(R.aes__string(namedParams ["color", "ChainId"]))
        >!> R.facet__grid(namedParams ["facets", "Parameter~."; "scales", "free"])


    type TraceGraph(graphicsDevice:Device, refreshRate, maxTraceItems) = 

        let __ = 
            match graphicsDevice with
            | X11 -> R.x11()
            | PNG -> R.png()

        let appendToTrace e recentTrace =
            recentTrace
            |> Seq.append (e |> decompose)
            |> Seq.truncate maxTraceItems

        let agent = MailboxProcessor.Start(fun inbox -> 
            let rec messageLoop traceMap lastDrawn = async {
                let! msg,chainId = inbox.Receive()
                match msg with
                | OptimisationEvent e ->
                    let trace = 
                        match traceMap |> Map.tryFind chainId with
                        | Some t -> t |> appendToTrace e
                        | None -> [] |> appendToTrace e
                    let map = traceMap |> Map.add chainId trace
                    if (System.DateTime.Now - lastDrawn) > System.TimeSpan.FromSeconds(refreshRate)
                    then 
                        map |> facetedTrace |> R.print |> ignore
                        return! messageLoop map System.DateTime.Now
                    else
                        return! messageLoop map lastDrawn
                | _ -> return! messageLoop traceMap lastDrawn }
            messageLoop Map.empty System.DateTime.Now )

        member __.Log msg = 
            let chain = Thread.CurrentThread.ManagedThreadId
            agent.Post (msg, chain)

    let graphWithConsole refreshRate maxData = 
        let consolePost = Bristlecone.Logging.Console.logger ()
        let graphLog = TraceGraph(Device.X11, refreshRate, maxData)
        (fun event -> 
            consolePost event
            graphLog.Log event )
