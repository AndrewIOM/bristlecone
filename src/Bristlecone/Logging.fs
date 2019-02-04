namespace Bristlecone.Logging

type LogEvent =
    | OptimisationEvent of ModelFitState
    | DifferentiationEvent of string
    | GeneralEvent of string
    | WorkloadEvent of string

and ModelFitState = {
    Iteration: int
    Likelihood: float
    Theta: seq<float>
}


module Console =

    open System.Threading

    let print threadId (x:LogEvent) = 
        match x with
        | OptimisationEvent _ -> ()
        | _ -> printfn "##%i## %A" threadId x

    let logger () =

        let agent = MailboxProcessor.Start(fun inbox -> 
            let rec messageLoop () = async {
                let! threadId,msg = inbox.Receive()
                print threadId msg
                return! messageLoop ()
                }
            messageLoop () )

        agent.Error.Add(fun e -> printfn "Error = %A" e)

        fun msg -> 
            let threadId = Thread.CurrentThread.ManagedThreadId
            agent.Post (threadId, msg)
