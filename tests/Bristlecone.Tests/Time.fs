module TimeTests

open System
open Expecto
open Expecto.ExpectoFsCheck
open Bristlecone

[<Tests>]
let timeSeries =
    testList "Time series" [

        testProperty "Reverse of reverse of a list is the original list" <|
          fun (xs:list<int>) -> List.rev (List.rev xs) = xs


        // testProperty "Always ordered in time" <| fun x ->
        //     x

        // testProperty "Fixing and unfixing a time series does not change the data" <| fun x ->
        //     x

        // testProperty "" <| fun x ->
        //     x
    ]
