namespace Bristlecone.Logging

module ModelFit =

    type ModelFitState = {
        ThreadId: int
        Subject: string
        Hypothesis: string
        AcceptanceRate: float
        Iteration: int
        TotalIterations: int
        Likelihood: float
    }

    let printModelState state =
        printfn "%i - %i iteration" state.ThreadId state.Iteration


    type ConsoleLogger() = 

        let agent = MailboxProcessor.Start(fun inbox -> 
            let rec messageLoop () = async{
                let! msg = inbox.Receive()
                printModelState msg
                return! messageLoop ()
                }
            messageLoop () )

        member this.Log msg = agent.Post msg
