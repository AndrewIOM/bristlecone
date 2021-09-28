module LibraryTests

open Bristlecone
open Expecto
open FsCheck

module ``Objective creation`` =
       
    [<Tests>]
    let initialBounds =
        testList "Objective" [

            testProperty "Time-series are paired to correct years" <| fun x ->
                false

            testProperty "Throws when time-series are not the same length" <| fun x ->
                false

        ]


// module ``Fit`` =

//     [<Tests>]
//     let fitTests =
//         testList "Time-invariant models" [

//             testProperty "MLE cannot fall outside given parameter constraints" <| fun x ->
//                 false

//             testProperty "It works" <| fun x ->

//                 let fit = 
//                     Bristlecone.Invariant.fitWithoutTime 
//                         Optimisation.None.passThrough 
//                         NoConditioning
//                         Detached
//                         ignore
//                         (Optimisation.EndConditions.afterIteration 1000)

//                 let f x = 2.

//                 let model = {
//                     Equations = [ shortCode "f"; f ]
//                 }

//                 let r = fit model
//                 printfn "R is %A" r
//                 ()
              

            

//         ]
