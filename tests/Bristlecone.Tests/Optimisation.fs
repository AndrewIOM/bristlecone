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
              yield rand () * 1.<``-logL``>, Array.init dimensions (fun _ -> rand () * 1.<``optim-space``>) ]


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

    [<Tests>]
    let jumpDistanceTests =

        testList "Mean Squared Jump Distance (MSJD)" [

            testProperty "Always continue when not enough data" <| fun (PositiveInt nBin) (PositiveInt nPoints) (PositiveInt n) (NonEmptyArray result) ->
                let results = List.replicate n result |> List.map(fun t -> 1.<``-logL``>, t)
                let msjd = EndConditions.stationarySquaredJumpDistance' (nBin * 1<iteration>) nPoints 1e-3 ignore results (results.Length * 1<iteration>)
                if nBin * nPoints <= results.Length then msjd.IsContinue else true

            testProperty "Stationary when theta is fixed through time" <| fun theta ->
                if Array.isEmpty theta || Array.exists Units.isNotFinite theta
                then true
                else
                    let stop = EndConditions.stationarySquaredJumpDistance ignore (List.init 1000 (fun _ -> 1.<``-logL``>, theta)) 1000<iteration>
                    stop.IsStationary

            testProperty "Not stationary when there is a non-linear trend in the data" <| fun theta (NormalFloat addition) ->
                if Array.length theta < 2 || addition = 0.0 || Units.isNotFinite addition
                then true
                else
                    let linear = List.init 1000 (fun i -> 1.<``-logL``>, theta |> Array.map(fun j -> (j + addition * 0.01 * float i * (float i)) * 1.<``optim-space``>))
                    let stop = EndConditions.stationarySquaredJumpDistance ignore linear 1000<iteration>
                    not stop.IsStationary
        ]


module Gibbs =

    open Bristlecone.Optimisation.MonteCarlo.MetropolisWithinGibbs
    open Bristlecone.Numerics
    open Bristlecone.Numerics.Typed

    // Dummy domain and objective for testing
    let mkDomain p : Optimisation.Domain<``optim-space``> =
        [| for _ in 1 .. p -> 0.0<``optim-space``>,  0.0<``optim-space``>, Parameter.Constraint.Unconstrained |]

    let mkTheta floats =
        floats
        |> Array.map (fun f -> f * 1.0<``optim-space``>)

    let dummyObjective : Optimisation.EvaluateObjective<``optim-space``> =
        let fn  (theta: float<``optim-space``> array) : float<``-logL``> = 
            // simple convex bowl: min at origin
            Array.sumBy (fun x -> x * x) theta |> Units.retype
        {Evaluate = fn
         Gradient = fun _ -> failwith "Dummy has no gradient"
         EvaluateVectorised = fun _ -> failwith "Dummy has no vectorised form" }

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
                let l = dummyObjective.Evaluate theta
                let j = jRaw % coords.Length
                let rnd = System.Random(0)
                let theta', _, _ = Core.mhStep1D rnd (mkDomain coords.Length) dummyObjective j lsj (theta, l)
                Expect.equal (Array.length theta') (Array.length theta) "Dimensionality preserved"

            testProperty "sweepOnce returns acceptance counts for all coords" <|
            fun (NonEmptyArray (coords: NormalFloat array)) (sigmasRaw: NormalFloat array) ->
                let p = coords.Length
                let sigmas =
                    sigmasRaw
                    |> Array.truncate p
                    |> Array.map (fun (NormalFloat s) -> s)
                    |> fun arr -> if arr.Length < p then Array.append arr (Array.create (p - arr.Length) 0.1) else arr
                let theta = mkTheta (coords |> Array.map(fun c -> c.Get))
                let l = dummyObjective.Evaluate theta
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
                let l = dummyObjective.Evaluate theta
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
        arr |> Array.map ((*) 1.<``optim-space-transformed``>)
    let optimSolution (l,v) = l * 1.<``-logL``>, optimVector v

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

            testProperty "Replace updates simplex, keeping sorted order" <| fun (val1:NormalFloat) (val2:NormalFloat) ->
                let amoeba = testAmoeba ()
                let newSol = optimSolution (0.5, [| val1.Get; val2.Get |])
                let updated = replace amoeba newSol
                Expect.isTrue (fst updated.Best <= fst updated.Worst) ""
                Expect.isTrue (updated.Solutions |> Array.map snd |> Array.contains [| val1.Get * 1.<``optim-space-transformed``>; val2.Get * 1.<``optim-space-transformed``> |]) ""

            // Reflection: r - c = α * (c - w)
            testProperty "Reflect is opposite to worst relative to centroid" <| fun (NonEmptyArray (cw: (NormalFloat * NormalFloat) array)) ->
                let c, w = cw |> Array.unzip
                let cVec = optimVector (c |> Array.map(fun c -> c.Get))
                let wVec = optimVector (w |> Array.map(fun c -> c.Get))
                let rVec = reflect cVec wVec Default
                let lhs = Array.map2 (-) rVec cVec
                let rhs = Array.map2 (-) cVec wVec |> Array.map (fun x -> x * Default.Alpha)
                Expect.all (Array.zip lhs rhs) (fun (a,b) -> abs (a - b) < 1e-8<``optim-space-transformed``>)
                    ""

            // Expansion: e - c = γ * (x - c) from x
            testProperty "Expand moves further in same direction" <| fun (NonEmptyArray (cx: (NormalFloat * NormalFloat) array)) ->
                let c, x = cx |> Array.unzip
                let cVec = optimVector (c |> Array.map(fun c -> c.Get))
                let xVec = optimVector (x |> Array.map(fun c -> c.Get))
                let eVec = expand cVec xVec Default
                let dirOrig = Array.map2 (-) xVec cVec
                let dirExp  = Array.map2 (-) eVec cVec
                Expect.all (Array.zip dirExp dirOrig) (fun (de,d) -> abs (de - Default.Gamma * d) < 1e-8<``optim-space-transformed``>)
                    ""

            // Contraction: k lies between c and w for 0<rho<1
            testProperty "Contract lies between centroid and worst" <| fun (NonEmptyArray (cw: (NormalFloat * NormalFloat) array)) ->
                let c, w = cw |> Array.unzip
                let cVec = optimVector (c |> Array.map(fun c -> c.Get))
                let wVec = optimVector (w |> Array.map(fun c -> c.Get))
                let kVec = contract cVec wVec Default
                Expect.all (Array.zip3 cVec wVec kVec) (fun (ci,wi,ki) ->
                    (ci <= ki && ki <= wi) || (wi <= ki && ki <= ci)) ""

            // Shrink: best unchanged, others move towards best by sigma
            testProperty "Shrink moves points towards best" <| fun (NonEmptyArray (bestOther: (NormalFloat * NormalFloat) array)) ->
                let best, other = bestOther |> Array.unzip
                let bestVec = optimVector (best |> Array.map(fun c -> c.Get))
                let otherVec = optimVector (other |> Array.map(fun c -> c.Get))
                let shrunk = shrinkTowardsBest bestVec otherVec Default
                
                // expected = bestVec + (otherVec - bestVec) * Default.Sigma
                let diff = Array.map2 (-) otherVec bestVec
                let scaledDiff = diff |> Array.map (fun x -> x * Default.Sigma)
                let expected = Array.map2 (+) bestVec scaledDiff

                Expect.all (Array.zip shrunk expected) (fun (a,b) -> abs (a - b) < 1e-8<``optim-space-transformed``>)
                    ""
        ]
