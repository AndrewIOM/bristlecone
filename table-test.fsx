// Showing output as a console table

// Console output:
// | Model ID | Work packages | Stage           | Status                 |
// | udie-... | #2            | Tuning          | Errored "Cool error"   |

open System

type LogIdentifier = {
    ThreadId: int
    Subject: string
    Hypothesis: string
}

type LogEvent =
    | OptimisationEvent of ModelFitState
    | DifferentiationEvent of string
    | GeneralEvent of string
    | WorkloadEvent of string
    | DebugEvent of string

and ModelFitState =
    { Iteration: int
      Likelihood: float
      Theta: seq<float> }

// Needs to know about optimisation process:
// - Current stage and any associated stage data (arbitrary key value?)

let row consoleWidth (colVals: string seq) =
    let colWidth = consoleWidth / (colVals |> Seq.length)
    printfn "a) %i" colWidth
    colVals
    |> Seq.mapi(fun i c ->
        sprintf "{%i,%i}" i colWidth
    )
    |> String.concat " | "
    |> fun s -> String.Format(sprintf "| %s |" s, colVals |> Seq.toArray)


let write nRow rowData =
    printfn "b) %i " <| (Console.WindowWidth - 4 - (rowData |> Seq.length) * 3)
    Console.SetCursorPosition(0, nRow)
    Console.Write(rowData |> row (Console.WindowWidth - 4 - (rowData |> Seq.length) * 3))

let writeSummary nRow =
    Console.SetCursorPosition(0, nRow)
    Console.Write("Summary: complete [na] / running [na] / waiting [na]")


module ConsoleTable =

    /// Row format is [ "Model ID * Subject"; "Work Packages"; "Stage"; "iteration"; "-logL"; "θ" ]
    let messageToRow msg =
        match msg with
        | OptimisationEvent e -> [ ""; ""; ""; ""; ""; "" ]

    let createConsoleTable (rows:Map<LogIdentifier,string list>) =
        write 0 [ "Model ID * Subject"; "Work Packages"; "Stage"; "iteration"; "-logL"; "θ" ]
        write 1 [ "---"; "---"; "---"; "---"; "---"; "---" ]
        rows |> Map.iter(fun _ v -> write 2 v)
        write ((rows |> Map.count) + 2) [ "---"; "---"; "---"; "---"; "---"; "---" ]
        writeSummary ((rows |> Map.count) + 3)
        rows

    let updateTable (logId:LogIdentifier) message currentRows =
        match currentRows |> Map.tryFind logId with
        | None ->
            let newRowData = messageToRow message
            let newMap = currentRows |> Map.add logId newRowData
            createConsoleTable newMap
        | Some _ ->
            let rowId = currentRows |> Seq.findIndex(fun kv -> kv.Key = logId)
            let rowData = messageToRow message
            write rowId rowData
            currentRows |> Map.add logId rowData


// Run this:
ConsoleTable.createConsoleTable Map.empty
|> ConsoleTable.updateTable { ThreadId = 34; Hypothesis = "Cool"; Subject = "YUSL29" } (OptimisationEvent {
    Iteration = 100
    Likelihood = 32.54
    Theta = [ 1.2; 1.4; 15.2 ]
})
|> ConsoleTable.updateTable { ThreadId = 40; Hypothesis = "Some other H"; Subject = "BV32" } (OptimisationEvent {
    Iteration = 628
    Likelihood = 367.1
    Theta = [ 1.2; 73.2; 15.2 ]
})

// Order work packages


write 1 [ "COSJ-1"; "YUSL29A"; "#31"; "Annealing"; "29.423"; "[|6.06798994; 0.103458061; 5.91590487; 0.065341847; 0.1050545732; 2.325422657; 0.2483225211|]" ]