module OptimisationTests

open Bristlecone.Optimisation
open Bristlecone.EstimationEngine
open Bristlecone
open Expecto
open FsCheck
open Config

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

    open Bristlecone.Optimisation.EndConditions
    open Bristlecone.EstimationEngine

    [<Tests>]
    let iterationTests =
        testList
            "Number of iterations"
            [

                testProperty "Ends on specified iteration"
                <| fun current ->
                    Expect.equal (atIteration current (Generate.resultList 5 current) current)
                        OptimStopReason.MaxIterations "Did not finish when at max iteration"

                testProperty "Ends if beyond maximum iteration"
                <| fun (PositiveInt currentIter) (PositiveInt finalIter) ->
                    let atEnd = atIteration (finalIter * 1<iteration>) []
                    if currentIter * 1<iteration> >= (finalIter * 1<iteration>)
                    then Expect.equal (atEnd (currentIter * 1<iteration>)) OptimStopReason.MaxIterations ""
                    else Expect.equal (atEnd (currentIter * 1<iteration>)) OptimStopReason.Continue ""
                  
            ]

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

module Gibbs =

    open Bristlecone.Optimisation.MonteCarlo.MetropolisWithinGibbs
    open Bristlecone.Tensors

    // Dummy domain and objective for testing
    let mkDomain p : Domain =
        [| for _ in 1 .. p -> 0.0<``optim-space``>,  0.0<``optim-space``>, Parameter.Constraint.Unconstrained |]

    let mkTheta floats =
        floats
        |> Array.map (fun f -> f * 1.0<``optim-space``>)
        |> Typed.ofVector

    let dummyObjective (theta: Point) : TypedTensor<Scalar,``-logL``> =
        // simple convex bowl: min at origin
        let arr = Typed.toFloatArray theta
        let v = Array.sumBy (fun x -> x * x) arr
        Typed.ofScalar v |> Typed.retype

    [<Tests>]
    let gibbsProps =
        testList "Core Gibbs property tests" [

            testProperty "propose changes only chosen coordinate" <|
            fun (NonEmptyArray (coords: NormalFloat array)) (PositiveInt jRaw) (NormalFloat lsj) ->
                let theta = coords |> Array.map (fun f -> f.Get * 1.0<``optim-space``>)
                let j = jRaw % theta.Length
                let rnd = System.Random(42)
                let proposed = Core.propose theta j lsj (mkDomain theta.Length) rnd
                // All coords except j are unchanged
                proposed
                |> Array.mapi (fun i v -> i, v)
                |> Array.forall (fun (i, v) -> i = j || v = theta.[i])
                |> fun t -> Expect.isTrue t "Only target coordinate should change"

            testProperty "mhStep1D preserves dimension" <|
            fun (NonEmptyArray (coords: NormalFloat array)) (PositiveInt jRaw) (NormalFloat lsj) ->
                let theta = mkTheta (coords |> Array.map(fun c -> c.Get))
                let l = dummyObjective theta
                let j = jRaw % coords.Length
                let rnd = System.Random(0)
                let theta', _, _ = Core.mhStep1D rnd (mkDomain coords.Length) dummyObjective j lsj (theta, l)
                Expect.equal (Typed.length theta') (Typed.length theta) "Dimensionality preserved"

            testProperty "sweepOnce returns acceptance counts for all coords" <|
            fun (NonEmptyArray (coords: NormalFloat array)) (sigmasRaw: NormalFloat array) ->
                let p = coords.Length
                let sigmas =
                    sigmasRaw
                    |> Array.truncate p
                    |> Array.map (fun (NormalFloat s) -> s)
                    |> fun arr -> if arr.Length < p then Array.append arr (Array.create (p - arr.Length) 0.1) else arr
                let theta = mkTheta (coords |> Array.map(fun c -> c.Get))
                let l = dummyObjective theta
                let rnd = System.Random(1)
                let _, _, accepts, _ = Core.sweepOnce rnd (mkDomain p) dummyObjective sigmas (theta, l)
                Expect.equal accepts.Length p "Acceptance counts length = param count"

            testProperty "runBatchMWG trace length matches iterations × params" <|
            fun (NonEmptyArray (coords: NormalFloat array)) (PositiveInt batchLenRaw) (sigmasRaw: NormalFloat array) ->
                let p = coords.Length
                let sigmas =
                    sigmasRaw
                    |> Array.truncate p
                    |> Array.map (fun (NormalFloat s) -> s)
                    |> fun arr -> if arr.Length < p then Array.append arr (Array.create (p - arr.Length) 0.1) else arr
                let theta = mkTheta (coords |> Array.map(fun c -> c.Get))
                let l = dummyObjective theta
                let rnd = System.Random(2)
                let batchLength = (batchLenRaw % 5 + 1) * 1<iteration> // keep small for test speed
                let _, _, _, trace = Core.runBatchMWG rnd (mkDomain p) dummyObjective batchLength sigmas (theta, l)
                let expectedLen = (Units.removeUnitFromInt batchLength) * p
                Expect.equal trace.Length expectedLen "Trace length matches sweeps × params"

            testProperty "acceptanceFromCounts yields rates in [0,1]" <|
            fun (NonEmptyArray accepts) (PositiveInt steps) ->
                let accepts' = accepts |> Array.map (fun a -> abs a % (steps + 1)) // cap at steps
                let rates = Core.acceptanceFromCounts accepts' (steps * 1<iteration>)
                Expect.all rates (fun r -> r >= 0.0 && r <= 1.0) "Rates in [0,1]"
    ]



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

    let untypeSolution s = s |> Tensors.Typed.toFloatArray |> Array.map Units.removeUnitFromFloat

    [<Tests>]
    let nelderMeadTests =
        testList "Amoeba geometric properties" [

            test "Centroid validation" {
                let amoeba = testAmoeba ()
                Expect.equal (centroid amoeba |> untypeSolution) [| 1.; 3. |] "Centroids are not equal"
            }

            testProperty "Replace updates simplex, keeping sorted order" <| fun (val1:NormalFloat) (val2:NormalFloat) ->
                let amoeba = testAmoeba ()
                let newSol = optimSolution (0.5, [| val1.Get; val2.Get |])
                let updated = replace amoeba newSol
                Expect.isTrue (fst updated.Best <= fst updated.Worst) ""
                Expect.isTrue (updated.Solutions |> Array.map (snd >> Tensors.Typed.toFloatArray) |> Array.contains [| val1.Get * 1.<``optim-space``>; val2.Get * 1.<``optim-space``> |]) ""

            // Reflection: r - c = α * (c - w)
            testProperty "Reflect is opposite to worst relative to centroid" <| fun (NonEmptyArray (cw: (NormalFloat * NormalFloat) array)) ->
                let c, w = cw |> Array.unzip
                let cVec = optimVector (c |> Array.map(fun c -> c.Get))
                let wVec = optimVector (w |> Array.map(fun c -> c.Get))
                let rVec = reflect cVec wVec (asTensorSettings Default)
                let lhs = toFloats (rVec - cVec)
                let rhs = toFloats (cVec - wVec) |> Array.map (fun x -> x * Default.Alpha)
                Expect.all (Array.zip lhs rhs) (fun (a,b) -> abs (a - b) < 1e-8<``optim-space``>)
                    ""

            // Expansion: e - c = γ * (x - c) from x
            testProperty "Expand moves further in same direction" <| fun (NonEmptyArray (cx: (NormalFloat * NormalFloat) array)) ->
                let c, x = cx |> Array.unzip
                let cVec = optimVector (c |> Array.map(fun c -> c.Get))
                let xVec = optimVector (x |> Array.map(fun c -> c.Get))
                let eVec = expand cVec xVec (asTensorSettings Default)
                let dirOrig = toFloats (xVec - cVec)
                let dirExp  = toFloats (eVec - cVec)
                Expect.all (Array.zip dirExp dirOrig) (fun (de,d) -> abs (de - Default.Gamma * d) < 1e-8<``optim-space``>)
                    ""

            // Contraction: k lies between c and w for 0<rho<1
            testProperty "Contract lies between centroid and worst" <| fun (NonEmptyArray (cw: (NormalFloat * NormalFloat) array)) ->
                let c, w = cw |> Array.unzip
                let cVec = optimVector (c |> Array.map(fun c -> c.Get))
                let wVec = optimVector (w |> Array.map(fun c -> c.Get))
                let kVec = contract cVec wVec (asTensorSettings Default)
                let cF, wF, kF = toFloats cVec, toFloats wVec, toFloats kVec
                Expect.all (Array.zip3 cF wF kF) (fun (ci,wi,ki) ->
                    (ci <= ki && ki <= wi) || (wi <= ki && ki <= ci)) ""

            // Shrink: best unchanged, others move towards best by sigma
            testProperty "Shrink moves points towards best" <| fun (NonEmptyArray (bestOther: (NormalFloat * NormalFloat) array)) ->
                let best, other = bestOther |> Array.unzip
                let bestVec = optimVector (best |> Array.map(fun c -> c.Get))
                let otherVec = optimVector (other |> Array.map(fun c -> c.Get))
                let shrunk = shrinkTowardsBest bestVec otherVec (asTensorSettings Default)
                let expected = bestVec + (otherVec - bestVec) * Tensors.Typed.ofScalar Default.Sigma
                let sF, eF = toFloats shrunk, toFloats expected
                Expect.all (Array.zip sF eF) (fun (a,b) -> abs (a - b) < 1e-8<``optim-space``>)
                    ""
        ]
