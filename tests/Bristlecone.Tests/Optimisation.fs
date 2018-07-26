module OptimisationTests

open Optimisation
open Expecto
open Expecto.ExpectoFsCheck

let config = { FsCheck.Config.Default with MaxTest = 10000 }

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
                let f (p:Point) = 42.

                let updated = shrink amoeba f Default

                Expect.equal (snd updated.Solutions.[0]) (snd amoeba.Solutions.[0]) "Unequal"
                Expect.equal (snd updated.Solutions.[1]) [| 1.; 3.; |] "Unequal"
                Expect.equal (snd updated.Solutions.[2]) [| 2.; 3.5|] "Unequal"
            }
        ]
