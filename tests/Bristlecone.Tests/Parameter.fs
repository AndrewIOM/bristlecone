module ParameterTests

open Bristlecone
open Expecto


[<Tests>]
let test =
    testList "Parameters" [

        testProperty "Reverse of reverse of a list is the original list" <|
            fun (xs:list<int>) -> List.rev (List.rev xs) = xs

        testProperty "Unconstrained parameter is not transformed" <|
            fun v ->
                let parameter = Parameter.create Unconstrained 

        testProperty "Must be cool" <|
            fun x -> x
]