namespace Bristlecone.Logging

open Bristlecone

type LogEvent =
    | OptimisationEvent of ModelFitState
    | DifferentiationEvent of string
    | GeneralEvent of string
    | WorkloadEvent of string
    | DebugEvent of string

and ModelFitState =
    { Iteration: int<iteration>
      Likelihood: float<``-logL``>
      Theta: seq<float<``optim-space``>> }

/// Simple logger to console that prints line-by-line progress and events.
module Console =

    open System.Threading

    let internal print nIteration threadId (x: LogEvent) =
        match x with
        | OptimisationEvent e ->
            if e.Iteration % nIteration = 0<iteration> then
                printfn "##%i## At iteration %i (-logL = %f) %A" threadId e.Iteration e.Likelihood e.Theta
        | _ -> printfn "##%i## %A" threadId x

    /// A simple console logger.
    /// `nIteration` specifies the number of iterations after which to log
    /// the current likelihood and parameter values.
    let logger (nIteration) =

        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec messageLoop () =
                    async {
                        let! threadId, msg = inbox.Receive()
                        print nIteration threadId msg
                        return! messageLoop ()
                    }

                messageLoop ())

        agent.Error.Add(fun e -> printfn "Error = %A" e)

        fun msg ->
            let threadId = Thread.CurrentThread.ManagedThreadId
            agent.Post(threadId, msg)
