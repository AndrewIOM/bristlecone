namespace Bristlecone.Data

open FSharp.Data

type BristleconeResult = CsvProvider<"templates/saved-data.csv">

module Cache =

    type Subject = string
    type AnalysisId = System.Guid
    type Data = {
        Iteration: int
        Parameters: (string * float) list
    }

    type CachingMessage = 
        | Add of Subject * AnalysisId * Data
        | Clear of Subject * AnalysisId

    type BristleconeCache = MailboxProcessor<CachingMessage>

    

        // let create directory cacheName =
            
        //     if not <| Directory.Exists directory then invalidOp (sprintf "Cannot create cache: the directory does not exist (%s)" directory)
        //     if File.Exists then 

    //     invalidOp "Not implemented"

    // AIMS:
    // 1. Gradually output rows into a csv cache of results (mailbox)
    // 2. Save 

    // We need a convertor of raw Bristlecone results into 