module OptimisationTests

open Bristlecone.Optimisation
open Bristlecone
open Expecto
open FsCheck

let config =
    { FsCheck.Config.Default with
        MaxTest = 10000 }

let random = System.Random()

module Generate =

    let rand = random.NextDouble

    let resultList dimensions (n:int<'a>) =
        [ for _ in 1 .. Units.removeUnitFromInt n do
              yield rand () * 1.<``-logL``>, Array.init dimensions (fun _ -> rand () * 1.<``optim-space``>) |> Tensors.Typed.ofVector ]


module ``End Conditions`` =

    open Optimisation.EndConditions

    [<Tests>]
    let iterationTests =
        testList
            "Number of iterations"
            [

              testProperty "Ends on specified iteration"
              <| fun current -> afterIteration current (Generate.resultList 5 current) current

              testProperty "Ends if beyond maximum iteration"
              <| fun current n ->
                  let results = Generate.resultList 1 n
                  let atEnd = afterIteration current results

                  if results.Length * 1<iteration> >= n then
                      atEnd current
                  else
                      not <| atEnd current ]

// [<Tests>]
// let jumpDistanceTests =

//     testList "Mean Squared Jump Distance (MSJD)" [

//         // testProperty "Always false when not enough data" <| fun nBin nPoints results ->
//         //     let msjd = stationarySquaredJumpDistance' nBin nPoints results
//         //     if (nBin * nPoints) <= results.Length then msjd else not msjd

//         // testProperty "MSJD is stationary when theta is fixed through time" <| fun theta ->
//         //     if theta |> snd |> Array.length = 0
//         //     then true
//         //     else
//         //         List.init 1000 (fun _ -> theta)
//         //         |> stationarySquaredJumpDistance

//         // testProperty "MSJD is not stationary when there is a linear trend in the data" <| fun (theta:Solution<float>) addition ->
//         //     List.init 1000 (fun i -> theta |> fst, theta |> snd |> Array.map(fun j -> j + (addition * float i)))
//         //     |> stationarySquaredJumpDistance
//         //     |> not
//     ]


// Module adapted from https://github.com/mathias-brandewinder/Amoeba
module ``Gradient Descent`` =

    open Optimisation.Amoeba.Solver

    let optimVector (arr: float[]) =
        arr |> Array.map ((*) 1.<``optim-space``>) |> Tensors.Typed.ofVector
    let optimSolution (l,v) = l * 1.<``-logL``>, optimVector v
    let toFloats v = Tensors.Typed.toFloatArray v

    let testAmoeba () =
        let sols = [| 0., [| 0.; 1. |]; 1., [| 2.; 5. |]; 2., [| 4.; 6. |] |] |> Array.map optimSolution
        { Dim = 2; Solutions = sols }


    [<Tests>]
    let nelderMeadTests =
        testList "Amoeba geometric properties" [

            test "Centroid validation" {
                let amoeba = testAmoeba ()
                Expect.equal (centroid amoeba) (optimVector [| 1.; 3. |]) "Centroids are not equal"
            }

            test "Replace validation" {
                let amoeba = testAmoeba ()
                let sub = optimSolution (0.5, [| 42.; 42. |])

                let updated = replace amoeba sub

                Expect.equal updated.Size amoeba.Size "Unequal sizes"

                Expect.equal updated.Solutions.[0] amoeba.Solutions.[0] "Unequal sizes"
                Expect.equal updated.Solutions.[1] sub "Unequal sizes"
                Expect.equal updated.Solutions.[2] amoeba.Solutions.[1] "Unequal sizes"
            }

            // Reflection: r - c = α * (c - w)
            testProperty "Reflect is opposite to worst relative to centroid" <| fun (c: NormalFloat[]) (w: NormalFloat[]) ->
                (c.Length = w.Length && c.Length > 0) ==> lazy
                    let cVec = optimVector (c |> Array.map(fun c -> c.Get))
                    let wVec = optimVector (w |> Array.map(fun c -> c.Get))
                    let rVec = reflect cVec wVec Default
                    let lhs = toFloats (rVec - cVec)
                    let rhs = toFloats (cVec - wVec) |> Array.map (fun x -> x * Default.Alpha)
                    Expect.all (Array.zip lhs rhs) (fun (a,b) -> abs (a - b) < 1e-8<``optim-space``>)
                        ""

            // Expansion: e - c = γ * (x - c) from x
            testProperty "Expand moves further in same direction" <| fun (c: NormalFloat[]) (x: NormalFloat[]) ->
                (c.Length = x.Length && c.Length > 0) ==> lazy
                    let cVec = optimVector (c |> Array.map(fun c -> c.Get))
                    let xVec = optimVector (x |> Array.map(fun c -> c.Get))
                    let eVec = expand cVec xVec Default
                    let dirOrig = toFloats (xVec - cVec)
                    let dirExp  = toFloats (eVec - cVec)
                    Expect.all (Array.zip dirExp dirOrig) (fun (de,d) -> abs (de - Default.Gamma * d) < 1e-8<``optim-space``>)
                        ""

            // Contraction: k lies between c and w for 0<rho<1
            testProperty "Contract lies between centroid and worst" <| fun (c: NormalFloat[]) (w: NormalFloat[]) ->
                (c.Length = w.Length && c.Length > 0) ==> lazy
                    let cVec = optimVector (c |> Array.map(fun c -> c.Get))
                    let wVec = optimVector (w |> Array.map(fun c -> c.Get))
                    let kVec = contract cVec wVec Default
                    let cF, wF, kF = toFloats cVec, toFloats wVec, toFloats kVec
                    Expect.all (Array.zip3 cF wF kF) (fun (ci,wi,ki) ->
                        (ci <= ki && ki <= wi) || (wi <= ki && ki <= ci)) ""

            // Shrink: best unchanged, others move towards best by sigma
            testProperty "Shrink moves points towards best" <| fun (best: NormalFloat[]) (other: NormalFloat[]) ->
                (best.Length = other.Length && best.Length > 0) ==> lazy
                    let bestVec = optimVector (best |> Array.map(fun c -> c.Get))
                    let otherVec = optimVector (other |> Array.map(fun c -> c.Get))
                    let shrunk = shrinkTowardsBest bestVec otherVec Default
                    let expected = bestVec + (otherVec - bestVec) * Tensors.Typed.ofScalar Default.Sigma
                    let sF, eF = toFloats shrunk, toFloats expected
                    Expect.all (Array.zip sF eF) (fun (a,b) -> abs (a - b) < 1e-8<``optim-space``>)
                        ""
        ]
