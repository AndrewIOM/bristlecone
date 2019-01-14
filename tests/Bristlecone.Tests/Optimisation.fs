module OptimisationTests

open Bristlecone.Optimisation
open Bristlecone
open Expecto

let config = { FsCheck.Config.Default with MaxTest = 10000 }

let random = System.Random()

module Generate =

    let rand = random.NextDouble

    let resultList dimensions n =
        [ for _ in 1..n do yield rand(), Array.init dimensions (fun _ -> rand()) ]


module ``End Conditions`` =

    open Optimisation.EndConditions

    [<Tests>]
    let iterationTests =
        testList "Iteration" [

            testProperty "Ends on iteration" <| fun current ->
                afterIteration current (Generate.resultList 5 current)

            testProperty "Ends when iteration has already occurred" <| fun n results ->
                let atEnd = afterIteration n results
                if results.Length >= n then atEnd else not atEnd
        ]

    [<Tests>]
    let jumpDistanceTests =

        testList "Mean Squared Jump Distance (MSJD)" [

            // testProperty "Always false when not enough data" <| fun nBin nPoints results ->
            //     let msjd = stationarySquaredJumpDistance' nBin nPoints results
            //     if (nBin * nPoints) <= results.Length then msjd else not msjd

            testProperty "MSJD is stationary when theta is fixed through time" <| fun theta ->
                if theta |> snd |> Array.length = 0
                then true
                else
                    List.init 1000 (fun _ -> theta)
                    |> stationarySquaredJumpDistance

            // testProperty "MSJD is not stationary when there is a linear trend in the data" <| fun (theta:Solution<float>) addition ->
            //     List.init 1000 (fun i -> theta |> fst, theta |> snd |> Array.map(fun j -> j + (addition * float i)))
            //     |> stationarySquaredJumpDistance
            //     |> not
        ]


// Module adapted from https://github.com/mathias-brandewinder/Amoeba
module ``Gradient Descent`` = 

    open Optimisation.Amoeba.Solver

    let testAmoeba () =
        let sols = [|
            0., [| 0.; 1. |]; 
            1., [| 2.; 5. |];
            2., [| 4.; 6. |]; |]
        { Dim = 2; Solutions = sols }

    [<Tests>]
    let ``Gradient Descent Tests`` =
        testList "Gradient descent" [

            test "Centroid validation" {
                let amoeba = testAmoeba ()
                Expect.equal (centroid amoeba) [| 1.; 3.; |] "Centroids are not equal"
            }

            test "Replace validation" {
                let amoeba = testAmoeba ()
                let sub = 0.5, [| 42.; 42.; |]
                
                let updated = replace amoeba sub
                
                Expect.equal updated.Size amoeba.Size "Unequal sizes"
                
                Expect.equal updated.Solutions.[0] amoeba.Solutions.[0] "Unequal sizes"
                Expect.equal updated.Solutions.[1] sub "Unequal sizes"
                Expect.equal updated.Solutions.[2] amoeba.Solutions.[1] "Unequal sizes"
            }

            test "Reflected validation" {
                let centroid = [| 0.; 1.; |]
                let x        = [| 4.; 3.; |]
                Expect.equal [| -4.; -1. |] (reflected (centroid, x) Default) "Unequal"
            }

            test "Expanded validation" {
                let centroid = [| 0.; 1.; |]
                let x        = [| 4.; 3.; |]
                Expect.equal (expanded (centroid, x) Default) [| -8.; -3. |] "Unequal"
            }

            test "Contracted validation" {
                let centroid = [| 0.; 1.; |]
                let x        = [| 4.; 3.; |]
                Expect.equal (contracted (centroid, x) Default) [| 2.; 2. |] "Unequal"
            }

            test "Shrink validation" {
                let amoeba = testAmoeba ()
                let f (p:Point<'a>) = 42.

                let updated = shrink amoeba f Default

                Expect.equal (snd updated.Solutions.[0]) (snd amoeba.Solutions.[0]) "Unequal"
                Expect.equal (snd updated.Solutions.[1]) [| 1.; 3.; |] "Unequal"
                Expect.equal (snd updated.Solutions.[2]) [| 2.; 3.5|] "Unequal"
            }
        ]
