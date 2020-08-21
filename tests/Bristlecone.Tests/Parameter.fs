module ParameterTests

open System
open Bristlecone
open Expecto

[<Tests>]
let shortCode =
    testList "Short codes" [

        testProperty "Cannot be null or empty" <| fun s ->
            if String.IsNullOrEmpty s 
            then Expect.isNone (ShortCode.create s)
            else Expect.isSome (ShortCode.create s)

        testProperty "Must be between one and ten characters" <| fun s ->
            match ShortCode.create s with
            | Some c -> c.Value.Length > 0 && c.Value.Length < 10
            | None -> true
    ]


[<Tests>]
let test =
    testList "Parameters" [

        testProperty "Only created when bounds are non-infinite and valid numbers" <| fun con bound1 bound2 ->
            match Parameter.create con bound1 bound2 with
            | Some p -> 
                Expect.isNotNaN bound1 ""
                Expect.isNotNaN bound2 ""
                Expect.isNotInfinity bound1 ""
                Expect.isNotInfinity bound2 ""
            | None -> ()

        testProperty "Reverse of reverse of a list is the original list" <|
            fun (xs:list<int>) -> List.rev (List.rev xs) = xs

        // testProperty "Unconstrained parameter is not transformed" <|
        //     fun v ->
        //         let parameter = Parameter.create Unconstrained 

        testProperty "Must be cool" <|
            fun x -> x
]