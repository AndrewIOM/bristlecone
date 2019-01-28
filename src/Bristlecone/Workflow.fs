namespace Bristlecone.Workflow

/// Orchestration module inspired by Tom Petricek: http://fssnip.net/nX
module Orchestration =

    open Bristlecone.Logging
    open Bristlecone.ModelSystem
    open System.Collections.Generic

    type OrchestrationMessage =
        | StartWorkPackage of Async<EstimationResult>
        | StartDependentWorkPackages of Async<EstimationResult>
        | Finished of EstimationResult
        // | PopResult of EstimationResult option

    let print writeOut s =
        s |> WorkloadEvent |> writeOut

    let printError (e:exn) =
        printfn "Error! %s" e.Message

    type OrchestrationAgent(writeOut, maxSimultaneous) = 

        let queue = Queue<_>()

        let agent = MailboxProcessor.Start(fun inbox ->
            let rec messageLoop inProgress = async {
                
                let! msg = inbox.Receive()

                let preProcess =
                    match msg with
                    | StartWorkPackage work -> 
                        queue.Enqueue(work)
                        print writeOut "Queued a work parcel"
                        inProgress
                    | Finished r -> 
                        print writeOut "A work parcel has finished"
                        inProgress - 1

                let postProcess =
                    if preProcess < maxSimultaneous && queue.Count > 0
                    then
                        let work = queue.Dequeue()
                        Async.Start(async {
                            print writeOut "Started processing a work parcel"
                            let! r = work
                            inbox.Post(Finished r) })
                        preProcess + 1
                    else preProcess

                print writeOut <| sprintf "There are currently %i running work parcels (max = %i)" inProgress maxSimultaneous
                return! messageLoop postProcess }

            messageLoop 0 )

        do agent.Error.Add printError

        member __.Post msg = agent.Post msg
