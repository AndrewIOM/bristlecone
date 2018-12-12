module ModelSelectionTests

open Bristlecone
open Bristlecone.ModelSelection
open Expecto
open FsCheck

module Akaike =

    // [<Tests>]
    // let ``akaike weights`` =
    //     testList "Akaike Weights" [

    //         testProperty "Weights for a set of models always add to one" <|
    //             fun n models -> 
    //                 models
    //                 |> Seq.map (fun l -> l, 10)
    //                 |> Akaike.akaikeWeights' n
    //                 |> Seq.sum = 1.
    //     ]
    
    let placeholder = true