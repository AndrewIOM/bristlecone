module ParameterTests

open System
open Bristlecone
open Expecto

[<Measure>] type testUnit

[<Tests>]
let shortCode =
    testList
        "Short codes"
        [

          testProperty "Cannot be null or empty"
          <| fun s ->
              if String.IsNullOrEmpty s then
                  Expect.isNone (ShortCode.create s) "An empty string returned a code"
              else
                  ()

          testProperty "Must be between one and 20 characters"
          <| fun s ->
              match ShortCode.create s with
              | Some c -> c.Value.Length > 0 && c.Value.Length <= 20
              | None -> true ]

module Transforms =

    [<Measure>] type testSpace
    [<Measure>] type testSpaceTransformed

    let tensor1<[<Measure>] 'u> (v: float<'u>) =
        Tensors.Typed.ofScalar v

    [<Tests>]
    let transformTests =
        testList "Parameter transforms" [

            testCase "scalarTransformOptimSpace is identity for unconstrained" <| fun _ ->
                let t = Parameter.ParameterTransforms.scalarTransformOptimSpace Parameter.Constraint.Unconstrained
                let input = tensor1<``optim-space``> 42.0<``optim-space``>
                let roundTrip = t.Inverse (t.Forward input) |> Tensors.Typed.toFloatScalar |> Units.removeUnitFromFloat
                Expect.equal roundTrip 42. "Forward >> Inverse should be identity"

            testCase "scalarTransformOptimSpace is identity for positive-only" <| fun _ ->
                let t = Parameter.ParameterTransforms.scalarTransformOptimSpace Parameter.Constraint.PositiveOnly
                let input = tensor1<``optim-space``> 42.0<``optim-space``>
                let roundTrip = t.Inverse (t.Forward input) |> Tensors.Typed.toFloatScalar |> Units.removeUnitFromFloat
                Expect.equal roundTrip 42. "Forward >> Inverse should be identity"

            testCase "scalarTransformOptimSpaceTransformed unconstrained is identity" <| fun _ ->
                let t = Parameter.ParameterTransforms.scalarTransformOptimSpaceTransformed Parameter.Constraint.Unconstrained
                let input = tensor1<``optim-space-transformed``> -3.5<``optim-space-transformed``>
                let roundTrip = t.Inverse (t.Forward input) |> Tensors.Typed.toFloatScalar |> Units.removeUnitFromFloat
                Expect.equal roundTrip -3.5 "Forward >> Inverse should be identity"

            testCase "scalarTransformOptimSpaceTransformed positive-only applies exp/log" <| fun _ ->
                let t = Parameter.ParameterTransforms.scalarTransformOptimSpaceTransformed Parameter.Constraint.PositiveOnly
                let raw = 2.0<``optim-space-transformed``>
                let input = tensor1 raw
                let forward = t.Forward input
                let expectedForward = tensor1<``parameter``> (exp (float raw) * 1.0<``parameter``>)
                Expect.floatClose Accuracy.high (forward |> Tensors.Typed.toFloatScalar |> float) (expectedForward |> Tensors.Typed.toFloatScalar |> float) "Forward should exp the value"

                let inverse = t.Inverse forward
                Expect.floatClose Accuracy.high (inverse |> Tensors.Typed.toFloatScalar |> float) (float raw) "Inverse should log the value"

            testProperty "Forward >> Inverse round-trips for random values" <| fun (FsCheck.NormalFloat v) ->
                let v = if v = 0.0 then 1.0 else v // avoid log(0)
                let posOnly = Parameter.ParameterTransforms.scalarTransformOptimSpaceTransformed Parameter.Constraint.PositiveOnly
                let uncon = Parameter.ParameterTransforms.scalarTransformOptimSpaceTransformed Parameter.Constraint.Unconstrained
                let unconInput = tensor1<``optim-space-transformed``> (v * 1.0<``optim-space-transformed``>)
                let posInput = tensor1<``optim-space-transformed``> (abs v * 1.0<``optim-space-transformed``>)
                let unconRound = uncon.Inverse (uncon.Forward unconInput)
                let posRound = posOnly.Inverse (posOnly.Forward posInput)
                Expect.floatClose Accuracy.high (float (unconRound |> Tensors.Typed.toFloatScalar)) (float (unconInput |> Tensors.Typed.toFloatScalar)) "Unconstrained round-trip"
                Expect.floatClose Accuracy.high (float (posRound |> Tensors.Typed.toFloatScalar)) (float (posInput |> Tensors.Typed.toFloatScalar)) "Positive-only round-trip"
        ]


[<Tests>]
let parameters =
    testList
        "Parameters"
        [

          testList
              "Creation"
              [ testProperty "Only created when bounds are non-infinite and valid numbers"
                <| fun con bound1 bound2 ->
                    match Parameter.create con bound1 bound2 with
                    | Some _ ->
                        Expect.isNotNaN bound1 "Bound 1 was nan"
                        Expect.isNotNaN bound2 "Bound 2 was nan"
                        Expect.isNotInfinity bound1 "Bound 1 was infinity"
                        Expect.isNotInfinity bound2 "Bound 2 was infinity"
                    | None -> ()

                testProperty "Only created when bounds satisfy constraint"
                <| fun con bound1 bound2 ->
                    match Parameter.create con bound1 bound2 with
                    | Some _ ->
                        match con with
                        | Parameter.Constraint.PositiveOnly -> bound1 > 0. && bound2 > 0.
                        | Parameter.Constraint.Unconstrained -> true
                    | None -> true ]

        ]

        //   testList
        //       "Internal access (for optimisation)"
        //       [ testProperty "Internal transform in undoes transform out"
        //         <| fun c (value: FsCheck.NormalFloat) ->
        //             if c = Parameter.Constraint.PositiveOnly && value.Get <= 0. then
        //                 ()
        //             else
        //                 let transformed = value.Get |> Parameter.transformOut c |> Parameter.transformIn c
        //                 Expect.floatClose Accuracy.high transformed value.Get "Transform process modified values"

        //         testProperty "Transformed value cannot be set as infinite or nan"
        //         <| fun con bound1 bound2 setAs ->
        //             match Parameter.create con bound1 bound2 with
        //             | None -> ()
        //             | Some p ->
        //                 let estimated = Parameter.setTransformedValue p setAs

        //                 if Double.IsNaN setAs || Double.IsInfinity setAs then
        //                     Expect.isError estimated "Did not error when infinity or NaN"

        //         testProperty "Internal value cannot be set as infinite or nan"
        //         <| fun con bound1 bound2 setAs ->
        //             match Parameter.create con bound1 bound2 with
        //             | None -> ()
        //             | Some p ->
        //                 let estimated = Parameter.setTransformedValue p setAs
        //                 let internalVal = Parameter.transformIn con setAs

        //                 if Double.IsNaN internalVal || Double.IsInfinity internalVal then
        //                     Expect.isError estimated "Did not error when internal value was infinity or NaN"

        //         testProperty "Transformed value equals set value when unconstrained"
        //         <| fun bound1 bound2 (setAs: FsCheck.NormalFloat) ->
        //             match Parameter.create Parameter.Constraint.Unconstrained bound1 bound2 with
        //             | None -> ()
        //             | Some p ->
        //                 Expect.equal
        //                     (Parameter.setTransformedValue p setAs.Get
        //                      |> Result.map Parameter.getTransformedValue)
        //                     (Ok setAs.Get)
        //                     "The parameter was created but the estimate was different" ]

        //   testProperty "getEstimate returns the non-transformed (external) value"
        //   <| fun con bound1 bound2 (setAs: FsCheck.NormalFloat) ->
        //       match Parameter.create con bound1 bound2 with
        //       | None -> ()
        //       | Some p ->
        //           match Parameter.setTransformedValue p (setAs.Get |> Parameter.transformOut con) with
        //           | Ok p ->
        //               match p |> Parameter.getEstimate with
        //               | Ok e -> Expect.floatClose Accuracy.high e setAs.Get "the estimate was different to that set"
        //               | _ -> failwith "Could not get the parameter estimate"
        //           | Error _ -> ()

        //   testList
        //       "Detached mode"
        //       [ testProperty "Bounds always equal creation-time bounds"
        //         <| fun con bound1 bound2 ->
        //             match Parameter.create con bound1 bound2 with
        //             | None -> ()
        //             | Some p ->
        //                 match p |> Parameter.detatchConstraint |> fst |> Parameter.bounds with
        //                 | Some(low, up) ->
        //                     Expect.equal ([ bound1; bound2 ] |> Seq.min) low "Lower bounds were different"
        //                     Expect.equal ([ bound1; bound2 ] |> Seq.max) up "Upper bounds were different"
        //                 | None -> failtestf "Bounds were not accessible when they should be" ] ]

// module Pool =

//     module ``Basic container`` =

//         [<Tests>]
//         let properties =
//             testList "Basic container operations" [

//                 testProperty "fromList then toList returns the same entries" <| fun entries ->
//                     failwith "Not implemented"

//                 testProperty "count returns the number of parameters" <| fun entries ->
//                     failwith "Not implemented"

//                 testProperty "keys returns all ShortCodes in the pool" <| fun entries ->
//                     failwith "Not implemented"

//                 testProperty "boxParam preserves name and constraint" <| fun name constraint_ bounds ->
//                     failwith "Not implemented"
//             ]

//     module ``Value access`` =

//         [<Tests>]
//         let properties =
//             testList "Value access" [

//                 testPropertyWithConfig Config.config "TryGetReal returns None for unestimated parameters" <| fun pool ->
//                     let keyToFetch = Parameter.Pool.keys pool |> Seq.head
//                     let result = Parameter.Pool.tryGetRealValue keyToFetch.Value pool
//                     Expect.isNone result "Returned some value for an unestimated parameter"

//                 testProperty "TryGetReal returns Some for estimated parameters" <| fun pool value ->
//                     failwith "Not implemented"

//                 testProperty "ToTensorRealIO returns a scalar tensor matching the real value" <| fun p value ->
//                     failwith "Not implemented"

//                 testProperty "FromTensorRealIO updates the value and preserves metadata" <| fun p newValue ->
//                     failwith "Not implemented"

//                 testProperty "tryGetRealValue returns None for missing names" <| fun pool missingName ->
//                     failwith "Not implemented"

//                 testProperty "tryGetRealValue returns correct value for present names" <| fun pool name ->
//                     failwith "Not implemented"
//             ]

//     module ``Vector conversion`` =

//         [<Tests>]
//         let properties =
//             testList "Vector conversion" [

//                 testProperty "toTensorWithKeysReal returns keys in correct order" <| fun pool ->
//                     failwith "Not implemented"

//                 testProperty "toTensorWithKeysReal returns vector matching parameter values" <| fun pool ->
//                     failwith "Not implemented"

//                 testProperty "fromRealVector updates all parameter values in correct order" <| fun pool vec ->
//                     failwith "Not implemented"

//                 testProperty "fromRealVector and toTensorWithKeysReal round-trip" <| fun pool ->
//                     failwith "Not implemented"
//             ]

//     module ``Transform compilation`` =

//         [<Tests>]
//         let properties =
//             testList "Transform compilation" [

//                 testProperty "Compiled Keys match pool keys" <| fun pool ->
//                     failwith "Not implemented"

//                 testProperty "IndexByName maps each name to correct index" <| fun pool ->
//                     failwith "Not implemented"

//                 testProperty "Forward then Inverse is identity for bounded transforms" <| fun pool vec ->
//                     failwith "Not implemented"

//                 testProperty "Inverse then Forward is identity for bounded transforms" <| fun pool vec ->
//                     failwith "Not implemented"

//                 testProperty "Forward then Inverse is identity for transformed transforms" <| fun pool vec ->
//                     failwith "Not implemented"

//                 testProperty "Inverse then Forward is identity for transformed transforms" <| fun pool vec ->
//                     failwith "Not implemented"

//                 testProperty "ScalarTransforms length matches parameter count" <| fun pool ->
//                     failwith "Not implemented"

//                 testProperty "ScalarTransforms constraints match underlying parameters" <| fun pool ->
//                     failwith "Not implemented"
//             ]

//     module ``Domain building`` =

//         [<Tests>]
//         let properties =
//             testList "Domain building" [

//                 testProperty "buildDomainFromBounds transforms real bounds to optimiser space" <| fun pool ->
//                     failwith "Not implemented"

//                 testProperty "buildDomainFromBounds uses +/- infinity for missing bounds" <| fun pool ->
//                     failwith "Not implemented"

//                 testProperty "buildDomainFromBounds constraints match parameter constraints" <| fun pool ->
//                     failwith "Not implemented"
//             ]

//     module ``Optimiser config`` =

//         [<Tests>]
//         let properties =
//             testList "Optimiser config creation" [

//                 testProperty "Bounded config Constraints match pool constraints" <| fun pool ->
//                     failwith "Not implemented"

//                 testProperty "Bounded config Domain matches buildDomainFromBounds output" <| fun pool ->
//                     failwith "Not implemented"

//                 testProperty "Transformed config Constraints list is empty" <| fun pool ->
//                     failwith "Not implemented"

//                 testProperty "Compiled transforms in config match pool keys" <| fun pool ->
//                     failwith "Not implemented"
//             ]