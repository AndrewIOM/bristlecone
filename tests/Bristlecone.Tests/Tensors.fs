module TensorTests

open Expecto
open DiffSharp
open Bristlecone.Tensors
open FsCheck
open Bristlecone

[<Measure>] type m
[<Measure>] type s

[<Tests>]
let typedTensorProps =
    testList "TypedTensor basic properties" [

        testProperty "ofScalar/toFloatScalar round-trips" <| fun (x: NormalFloat) ->
            let t = Typed.ofScalar (x.Get * 1.0<m>)
            let back = Typed.toFloatScalar t |> Units.removeUnitFromFloat
            Expect.floatClose Accuracy.high back (x.Get * 1.0) "Scalar round-trip failed"

        testProperty "ofVector/toFloatArray round-trips" <| fun (xs: FsCheck.NonEmptyArray<NormalFloat>) ->
            let data = xs.Get |> Array.map (fun f -> f.Get * 1.0<m>)
            let v = Typed.ofVector data
            let back = Typed.toFloatArray v
            Expect.sequenceEqual back data "Vector round-trip failed"

        testProperty "Scalar addition matches float addition" <| fun (a: NormalFloat) (b: NormalFloat) ->
            let ta = Typed.ofScalar (a.Get * 1.0<m>)
            let tb = Typed.ofScalar (b.Get * 1.0<m>)
            let sum = ta + tb
            let expected = (a.Get + b.Get) * 1.0<m> |> Units.removeUnitFromFloat
            Expect.floatClose Accuracy.high (Typed.toFloatScalar sum |> Units.removeUnitFromFloat) expected "Scalar addition mismatch"

        testProperty "Vector-scalar multiplication matches elementwise float multiplication" <| fun (xs: FsCheck.NonEmptyArray<NormalFloat>) (k: NormalFloat) ->
            let data = xs.Get |> Array.map (fun f -> f.Get * 1.0<m>)
            let v = Typed.ofVector data
            let s = Typed.ofScalar (k.Get * 1.0<s>)
            let prod = v * s
            let expected = data |> Array.map (fun f -> f * k.Get * 1.0<s>)
            Expect.sequenceEqual (Typed.toFloatArray prod) expected "Vector-scalar multiplication mismatch"

        testProperty "Retype does not change underlying numeric values" <| fun (xs: FsCheck.NonEmptyArray<NormalFloat>) ->
            let data = xs.Get |> Array.map (fun f -> f.Get * 1.0<m>)
            let v = Typed.ofVector data
            let v' = Typed.retype<m,s,_> v
            let back = Typed.toFloatArray v'
            let expected = data |> Array.map (fun f -> f * 1.0<s> / 1.0<m>)
            Expect.sequenceEqual back expected "Retype altered numeric values"

        testProperty "Tail drops the first element" <| fun (x: NormalFloat) (rest: FsCheck.NonEmptyArray<NormalFloat>) ->
            let arr = Array.append [| x.Get * 1.0<m> |] (rest.Get |> Array.map (fun f -> f.Get * 1.0<m>))
            let v = Typed.ofVector arr
            if arr.Length < 2 then Expect.throws (fun _ -> Typed.tail v |> ignore) "Tail should not allow passing a vector of 1 or less length"
            let tail = Typed.tail v
            Expect.sequenceEqual (Typed.toFloatArray tail) (arr |> Array.skip 1) "Tail did not drop first element"

        testProperty "Broadcast scalar to vector repeats value" <| fun (x: NormalFloat) (len: PositiveInt) ->
            let s = Typed.ofScalar (x.Get * 1.0<m>)
            let v = Typed.broadcastScalarToVector s len.Get
            let arr = Typed.toFloatArray v
            Expect.sequenceEqual arr (Array.create len.Get (x.Get * 1.0<m>)) "Broadcast did not repeat value"

        testProperty "Stack1D produces vector of correct length and values" <| fun (xs: FsCheck.NonEmptyArray<NormalFloat>) ->
            let scalars = xs.Get |> Array.map (fun f -> Typed.ofScalar (f.Get * 1.0<m>))
            let v = Typed.stack1D scalars
            Expect.equal (Typed.length v) xs.Get.Length "Stacked vector length mismatch"
            Expect.sequenceEqual (Typed.toFloatArray v) (xs.Get |> Array.map (fun f -> f.Get * 1.0<m>)) "Stacked vector values mismatch"

        testProperty "Active patterns classify shapes correctly" <| fun (x: NormalFloat) (xs: FsCheck.NonEmptyArray<NormalFloat>) ->
            let sTensor = (Typed.ofScalar (x.Get * 1.0<m>)).Value
            match sTensor with
            | Scalar _ -> ()
            | _ -> failwith "Scalar active pattern failed"

            let vTensor = (Typed.ofVector (xs.Get |> Array.map (fun f -> f.Get * 1.0<m>))).Value
            match vTensor with
            | Vector _ -> ()
            | _ -> failwith "Vector active pattern failed"

    ]

[<Tests>]
let integration =
    testList "Tensor integration tests" [

        testCase "Pythagoras with TypedTensor" <| fun _ ->
            let v1 = Typed.ofVector [| 3.0<m> ; 0.0<m> |]
            let v2 = Typed.ofVector [| 0.0<m> ; 4.0<m> |]
            let sum = Typed.addVector v1 v2
            let length = sum |> Typed.squareVector |> Typed.sumVector |> Typed.sqrtScalar
            Expect.floatClose Accuracy.high (Typed.toFloatScalar length |> Units.removeUnitFromFloat) 5.0 "Length should be 5m"

        testCase "Dot product matches magnitude*cosθ" <| fun _ ->
            let a = Typed.ofVector [| 1.0<m> ; 0.0<m> |]
            let b = Typed.ofVector [| 0.0<m> ; 1.0<m> |]
            let dot = Typed.dot a b |> Typed.toFloatScalar |> Units.removeUnitFromFloat
            Expect.floatClose Accuracy.high dot 0. "Perpendicular vectors should have dot=0"

        testCase "log(x^2) = 2 log x" <| fun _ ->
            let xs = Typed.ofVector [| 1.5; 2.0; 3.0 |]
            let lhs = xs |> Typed.squareVector |> Typed.logVector
            let rhs = Typed.scale (Typed.ofScalar 2.0) (Typed.logVector xs)
            Expect.sequenceEqual (Typed.toFloatArray lhs) (Typed.toFloatArray rhs) "Log identity failed"

        testCase "Rotation + scaling + dot product pipeline" <| fun _ ->
        // Step 1: original vector in metres
        let v = Typed.ofVector [| 3.0<m>; 4.0<m> |]

        // Step 2: 90° rotation matrix (counter‑clockwise)
        let rot = Typed.ofMatrix <| array2D [[| 0.0; -1.0 |]; [| 1.0;  0.0 |]]

        // Apply rotation: result is (-4, 3) m
        let vRot = Typed.matMul rot (Typed.ofMatrix <| array2D [| [| 3.0<m> |]; [| 4.0<m> |] |])

        // Convert back to vector form for next ops
        let vRotVec = Typed.ofVector [| vRot.Value.[0].toDouble() * 1.0<m>
                                        vRot.Value.[1].toDouble() * 1.0<m> |]

        // Step 3: scale by 2 s^-1
        let k = Typed.ofScalar (2.0<s^-1>)
        let vScaled = Typed.scale k vRotVec // units: m*s^-1

        // Step 4: dot with w = (1,1) m*s^-1
        let w = Typed.ofVector [| 1.0<m*s^-1>; 1.0<m*s^-1> |]
        let dot = Typed.dot vScaled w |> Typed.toFloatScalar

        // Step 5: expected result: (-8, 6) · (1,1) = -8 + 6 = -2 m^2*s^-2
        let expected = -2.0<m^2*s^-2>

        Expect.floatClose Accuracy.high
            (Units.removeUnitFromFloat dot)
            (Units.removeUnitFromFloat expected) "Chained operation result mismatch"
        
    ]


