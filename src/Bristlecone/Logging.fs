namespace Bristlecone.Logging

open Bristlecone

type LogEvent =
    | OptimisationEvent of ModelFitState
    | CompleteEvent
    | GeneralEvent of string
    | WorkloadEvent of string
    | DebugEvent of string

and ModelFitState =
    { Iteration: int<iteration>
      Likelihood: float<``-logL``>
      Theta: seq<float<``optim-space``>> }

type ThreadStatus =
    { ThreadId   : int
      Stage      : string
      Iteration  : int<iteration>
      Likelihood : float<``-logL``>
      Theta      : string
      Status     : string }

module ConsoleTable =

    open System
    open System.Collections.Concurrent
    open System.Threading

    let logger nIteration =
        let active    = ConcurrentDictionary<int, ThreadStatus>()
        let lastLikelihoods = ConcurrentDictionary<int, float<``-logL``>>()
        let drawLock  = obj()
        let mutable completedCount = 0

        Console.Clear()

        // --- Helpers ---
        let oneLine (s: string) =
            if String.IsNullOrEmpty s then ""
            else s.Replace("\r", "").Replace("\n", " ")

        let truncate (text: string) width =
            let t = oneLine text
            if t.Length > width then
                if width > 3 then t.Substring(0, width - 3) + "..."
                else t.Substring(0, width)
            else t.PadRight(width)

        let formatThetaValues (values: seq<float<'u>>) =
            values
            |> Seq.map (fun v -> sprintf "%.10g" (float v))
            |> String.concat "; "
            |> fun s -> "[|" + s + "|]"
            |> oneLine

        let truncateColumn (colVal: string) (fixedColsLen: int) =
            let remaining = Console.WindowWidth - fixedColsLen
            let t = oneLine colVal
            if remaining <= 0 then ""
            elif t.Length > remaining then
                if remaining > 3 then t.Substring(0, remaining - 3) + "..."
                else t.Substring(0, remaining)
            else t.PadRight(remaining)

        // --- Drawing ---
        let redrawActiveTable () =
            lock drawLock (fun () ->
                let reservedHeight = 2 + active.Count // status + header + rows

                // Clear reserved block
                Console.SetCursorPosition(0, 0)
                for _ in 1 .. reservedHeight do
                    Console.Write(new string(' ', Console.WindowWidth))
                Console.SetCursorPosition(0, 0)

                // Status bar
                Console.BackgroundColor <- ConsoleColor.DarkBlue
                Console.ForegroundColor <- ConsoleColor.White
                let statusText = sprintf " Active: %d   Completed: %d " active.Count completedCount
                Console.Write(statusText.PadRight(Console.WindowWidth))
                Console.ResetColor()
                Console.WriteLine()

                // Header
                Console.ForegroundColor <- ConsoleColor.Cyan
                printfn "%-5s %-30s %-12s %-12s %-12s %s"
                        "TID" "Stage" "Iter" "-logL" "Status" "Theta"
                Console.ResetColor()

                // Rows
                for KeyValue(_, st) in active |> Seq.sortBy (fun kv -> kv.Key) do
                    let stageCol = truncate st.Stage 30
                    let fixedCols =
                        sprintf "%-5i %s %-12i %-12.4f %-12s "
                            st.ThreadId stageCol (int st.Iteration) (float st.Likelihood) st.Status
                    let thetaDisplay = truncateColumn st.Theta fixedCols.Length
                    Console.Write(fixedCols)
                    Console.WriteLine(thetaDisplay)

                // Leave cursor just below reserved block
                Console.SetCursorPosition(0, reservedHeight)
            )

        let appendCompletedLog (st: ThreadStatus) =
            Console.ForegroundColor <- ConsoleColor.DarkGray
            let stageCol = truncate st.Stage 30
            let fixedCols =
                sprintf "Completed: %-5i %s Iter=%i -logL=%.4f "
                    st.ThreadId stageCol (int st.Iteration) (float st.Likelihood)
            let thetaDisplay = truncateColumn st.Theta fixedCols.Length
            Console.Write(fixedCols)
            Console.WriteLine(thetaDisplay)
            Console.ResetColor()

        // --- Agent ---
        let agent =
            MailboxProcessor.Start(fun inbox ->
                let rec loop () = async {
                    let! (threadId, msg) = inbox.Receive()
                    match msg with
                    | OptimisationEvent e when e.Iteration % nIteration = 0<iteration> ->
                        let prev =
                            match lastLikelihoods.TryGetValue threadId with
                            | true, v -> v
                            | _ -> e.Likelihood

                        let status =
                            if e.Likelihood < prev then "\u2193" else "-"

                        lastLikelihoods.[threadId] <- e.Likelihood

                        active.[threadId] <-
                            { ThreadId   = threadId
                              Stage      = "Optimising"
                              Iteration  = e.Iteration
                              Likelihood = e.Likelihood
                              Theta      = formatThetaValues e.Theta
                              Status     = status }

                        redrawActiveTable()

                    | GeneralEvent s ->
                        let cleanStage = oneLine s
                        active.AddOrUpdate(
                            threadId,
                            { ThreadId = threadId
                              Stage = cleanStage
                              Iteration = 0<iteration>
                              Status = ""
                              Likelihood = nan * 1.<``-logL``>
                              Theta = "" },
                            fun _ old -> { old with Stage = cleanStage }
                        ) |> ignore
                        redrawActiveTable()

                    | CompleteEvent ->
                        match active.TryRemove threadId with
                        | true, st ->
                            completedCount <- completedCount + 1
                            redrawActiveTable() // redraw table in place
                            appendCompletedLog st // print completion below table
                        | _ -> ()

                    | _ -> ()
                    return! loop ()
                }
                loop ()
            )

        agent.Error.Add(fun e -> printfn "Error = %A" e)

        fun msg ->
            let tid = Thread.CurrentThread.ManagedThreadId
            agent.Post(tid, msg)


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
    let logger nIteration =

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
