namespace Bristlecone.Workflow

/// Queue functions to manage many work packages in parallel.
/// [ Inspired by Tom Petricek: http://fssnip.net/nX ]
[<RequireQualifiedAccess>]
module Orchestration =

    open Bristlecone.Logging
    open Bristlecone.ModelSystem
    open System.Collections.Generic

    type OrchestrationMessage =
        | StartWorkPackage of Async<EstimationResult>
        | StartDependentWorkPackages of Async<EstimationResult>
        | Finished of EstimationResult
        | WorkFailed of exn
        | WorkCancelled

    let print writeOut s =
        s |> WorkloadEvent |> writeOut

    /// The `OrchestrationAgent` queues work items of the type `Async<EstimationResult>`, which
    /// are run in parallel up to a total of `maxSimultaneous` at one time.
    type OrchestrationAgent(writeOut, maxSimultaneous, retainResults) = 

        let queue = Queue<_>()
        let results = Queue<_>()

        let agent = MailboxProcessor.Start(fun inbox ->
            let rec messageLoop inProgress = async {
                
                let! msg = inbox.Receive()

                let preProcess =
                    match msg with
                    | StartWorkPackage work -> 
                        queue.Enqueue(work)
                        print writeOut <| sprintf "Work parcel added to queue (%i items are waiting)" queue.Count
                        inProgress
                    | Finished r -> 
                        print writeOut <| sprintf "Work parcel has finished (%i items are waiting)" queue.Count
                        if retainResults then results.Enqueue r
                        inProgress - 1
                    | StartDependentWorkPackages work -> invalidOp "Not implemented"
                    | WorkFailed e ->
                        print writeOut <| sprintf "There was an issue completing one work item: %s" e.Message
                        inProgress - 1
                    | WorkCancelled ->
                        print writeOut "A work parcel was cancelled"
                        inProgress - 1

                let postProcess =
                    if preProcess < maxSimultaneous && queue.Count > 0
                    then
                        let work = queue.Dequeue()
                        Async.Start(
                            async {
                                print writeOut "Started processing a work parcel"
                                try
                                    let! r = work
                                    Finished r |> inbox.Post
                                with error -> inbox.Post <| WorkFailed error })
                        preProcess + 1
                    else preProcess

                print writeOut <| sprintf "Running %i work parcels in parallel (limit = %i)" inProgress maxSimultaneous
                return! messageLoop postProcess }

            messageLoop 0 )

        do agent.Error.Add (fun e -> print writeOut <| sprintf "An error occurred in the orchestrator: %s" e.Message)

        member __.Post msg = agent.Post msg

        member __.TryGetResult () =
            if results.Count > 0
            then results.Dequeue() |> Some
            else None