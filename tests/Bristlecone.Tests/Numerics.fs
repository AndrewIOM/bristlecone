module TensorTests

open Expecto
open Bristlecone.Numerics
open Bristlecone.Numerics.Typed
open FsCheck
open Bristlecone

[<Measure>] type m
[<Measure>] type s

let typedNumerics (numEngine: NumericEngine<'S,'V,'M>) =
    testList "FsScalar/FsVector basic properties" [

        testProperty "ofScalar/toFloatScalar round-trips" <| fun (x: NormalFloat) ->
            let t = ofScalar numEngine.scalarBackend (x.Get * 1.0<m>)
            let back = Scalar.toFloat t |> Units.removeUnitFromFloat
            Expect.floatClose Accuracy.high back (x.Get * 1.0) "Scalar round-trip failed"

        testProperty "ofVector/toFloatArray round-trips" <| fun (xs: FsCheck.NonEmptyArray<NormalFloat>) ->
            let data = xs.Get |> Array.map (fun f -> f.Get * 1.0<m>)
            let v = ofVector numEngine.vectorBackend numEngine.vectorAccess data
            let back = Vector.toArrayFloat v
            Expect.sequenceEqual back data "Vector round-trip failed"

        testProperty "Scalar addition matches float addition" <| fun (a: NormalFloat) (b: NormalFloat) ->
            let ta = ofScalar numEngine.scalarBackend (a.Get * 1.0<m>)
            let tb = ofScalar numEngine.scalarBackend (b.Get * 1.0<m>)
            let sum = ta + tb
            let expected = (a.Get + b.Get) * 1.0<m> |> Units.removeUnitFromFloat
            Expect.floatClose Accuracy.high (Scalar.toFloat sum |> Units.removeUnitFromFloat) expected "Scalar addition mismatch"

        testProperty "Vector-scalar multiplication matches elementwise float multiplication" <| fun (xs: FsCheck.NonEmptyArray<NormalFloat>) (k: NormalFloat) ->
            let data = xs.Get |> Array.map (fun f -> f.Get * 1.0<m>)
            let v = ofVector numEngine.vectorBackend numEngine.vectorAccess data
            let s = ofScalar numEngine.scalarBackend (k.Get * 1.0<s>)
            let prod = v * s
            let expected = data |> Array.map (fun f -> f * k.Get * 1.0<s>)
            Expect.sequenceEqual (Vector.toArrayFloat prod) expected "Vector-scalar multiplication mismatch"

        testProperty "Retype does not change underlying numeric values" <| fun (xs: FsCheck.NonEmptyArray<NormalFloat>) ->
            let data = xs.Get |> Array.map (fun f -> f.Get * 1.0<m>)
            let v = ofVector numEngine.vectorBackend numEngine.vectorAccess data
            let v' = Vector.retype<'S,'V,m,s> v
            let back = Vector.toArrayFloat v'
            let expected = data |> Array.map (fun f -> f * 1.0<s> / 1.0<m>)
            Expect.sequenceEqual back expected "Retype altered numeric values"

        testProperty "Tail drops the first element" <| fun (x: NormalFloat) (rest: FsCheck.NonEmptyArray<NormalFloat>) ->
            let arr = Array.append [| x.Get * 1.0<m> |] (rest.Get |> Array.map (fun f -> f.Get * 1.0<m>))
            let v = ofVector numEngine.vectorBackend numEngine.vectorAccess arr
            if arr.Length < 2 then Expect.throws (fun _ -> Vector.tail v |> ignore) "Tail should not allow passing a vector of 1 or less length"
            let tail = Vector.tail v
            Expect.sequenceEqual (Vector.toArrayFloat tail) (arr |> Array.skip 1) "Tail did not drop first element"

        testProperty "Broadcast scalar to vector repeats value" <| fun (x: NormalFloat) (len: PositiveInt) ->
            let s = ofScalar numEngine.scalarBackend (x.Get * 1.0<m>)
            let v = Scalar.broadcast numEngine s len.Get
            let arr = Vector.toArrayFloat v
            Expect.sequenceEqual arr (Array.create len.Get (x.Get * 1.0<m>)) "Broadcast did not repeat value"

        testProperty "Stack1D produces vector of correct length and values" <| fun (xs: FsCheck.NonEmptyArray<NormalFloat>) ->
            let scalars = xs.Get |> Array.map (fun f -> ofScalar numEngine.scalarBackend (f.Get * 1.0<m>))
            let v = Vector.ofArray numEngine.vectorBackend numEngine.vectorAccess scalars
            Expect.equal (Vector.length v) xs.Get.Length "Stacked vector length mismatch"
            Expect.sequenceEqual (Vector.toArrayFloat v) (xs.Get |> Array.map (fun f -> f.Get * 1.0<m>)) "Stacked vector values mismatch"

        testProperty "Active patterns classify shapes correctly" <| fun (x: NormalFloat) (xs: FsCheck.NonEmptyArray<NormalFloat>) ->
            let v =
                xs.Get
                |> Array.map (fun f -> f.Get * 1.0<m>)
                |> ofVector numEngine.vectorBackend numEngine.vectorAccess
            match v with
            | ActivePatterns.VectorOfLen xs.Get.Length v -> ()
            | _ -> failwith "Vector active pattern failed"

    ]

[<Tests>]
let typedDiffSharp = typedNumerics Backends.DiffSharp.engine

let integration (numEngine: NumericEngine<'S,'V,'M>) =
    testList "Numerics (FsScalar/FsVector/FsMatrix) integration tests" [

        testCase "Pythagoras theorem" <| fun _ ->
            let v1 = ofVector numEngine.vectorBackend numEngine.vectorAccess [| 3.0<m> ; 0.0<m> |]
            let v2 = ofVector numEngine.vectorBackend numEngine.vectorAccess [| 0.0<m> ; 4.0<m> |]
            let sum = v1 + v2
            let length = sum |> Vector.square |> Vector.sum |> Scalar.sqrt
            Expect.floatClose Accuracy.high (Scalar.toFloat length |> Units.removeUnitFromFloat) 5.0 "Length should be 5m"

        testCase "Dot product matches magnitude*cosθ" <| fun _ ->
            let a = ofVector numEngine.vectorBackend numEngine.vectorAccess [| 1.0<m> ; 0.0<m> |]
            let b = ofVector numEngine.vectorBackend numEngine.vectorAccess [| 0.0<m> ; 1.0<m> |]
            let dot = Vector.dot a b |> Scalar.toFloat |> Units.removeUnitFromFloat
            Expect.floatClose Accuracy.high dot 0. "Perpendicular vectors should have dot=0"

        testCase "log(x^2) = 2 log x" <| fun _ ->
            let xs = ofVector numEngine.vectorBackend numEngine.vectorAccess [| 1.5; 2.0; 3.0 |]
            let lhs = xs |> Vector.square |> Vector.log
            let rhs = Vector.scale (ofScalar numEngine.scalarBackend 2.0) (Vector.log xs)
            Expect.sequenceEqual (Vector.toArrayFloat lhs) (Vector.toArrayFloat rhs) "Log identity failed"

        testCase "Rotate, scale, and dot product" <| fun _ ->

            let v = ofVector numEngine.vectorBackend numEngine.vectorAccess [| 3.0<m>; 4.0<m> |]
            let rot = ofMatrix numEngine.matrixBackend numEngine.matrixAccess <| array2D [[| 0.0; -1.0 |]; [| 1.0;  0.0 |]]
            let vRot = Matrix.matMul rot (ofMatrix numEngine.matrixBackend numEngine.matrixAccess <| array2D [| [| 3.0<m> |]; [| 4.0<m> |] |])
            let vRotVec = ofVector numEngine.vectorBackend numEngine.vectorAccess [| vRot.Value.[0].toDouble() * 1.0<m> vRot.Value.[1].toDouble() * 1.0<m> |]
            let k = ofScalar numEngine.scalarBackend (2.0<s^-1>)
            let vScaled = Vector.scale k vRotVec // m*s^-1

            // dot with w = (1,1) m*s^-1
            let w = ofVector numEngine.vectorBackend numEngine.vectorAccess [| 1.0<m*s^-1>; 1.0<m*s^-1> |]
            let dot = Vector.dot vScaled w |> Scalar.toFloat

            let expected = -2.0<m^2*s^-2>

            Expect.floatClose Accuracy.high
                (Units.removeUnitFromFloat dot)
                (Units.removeUnitFromFloat expected) "Chained operation result mismatch"
        
    ]

[<Tests>]
let integrationDiffSharp = integration Backends.DiffSharp.engine

