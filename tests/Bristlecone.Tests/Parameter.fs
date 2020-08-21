module ParameterTests

open System
open Bristlecone
open Expecto

[<Tests>]
let shortCode =
    testList "Short codes" [

        testProperty "Cannot be null or empty" <| fun s ->
            if String.IsNullOrEmpty s 
            then Expect.isNone (ShortCode.create s) "An empty string returned a code"
            else ()

        testProperty "Must be between one and ten characters" <| fun s ->
            match ShortCode.create s with
            | Some c -> c.Value.Length > 0 && c.Value.Length <= 10
            | None -> true            
    ]

[<Tests>]
let test =
    testList "Parameters" [

        testList "Creation" [
            testProperty "Only created when bounds are non-infinite and valid numbers" <| fun con bound1 bound2 ->
                match Parameter.create con bound1 bound2 with
                | Some _ -> 
                    Expect.isNotNaN bound1 "Bound 1 was nan"
                    Expect.isNotNaN bound2 "Bound 2 was nan"
                    Expect.isNotInfinity bound1 "Bound 1 was infinity"
                    Expect.isNotInfinity bound2 "Bound 2 was infinity"
                | None -> ()

            testProperty "Only created when bounds satisfy constraint" <| fun con bound1 bound2 ->
                match Parameter.create con bound1 bound2 with
                | Some _ ->
                    match con with
                    | Parameter.Constraint.PositiveOnly -> bound1 > 0. && bound2 > 0.
                    | Parameter.Constraint.Unconstrained -> true
                | None -> true
        ]

        testList "Starting bounds" [
            testProperty "When positively-constrained, bounds are exponentiated" <| fun bound1 bound2 ->
                match Parameter.create Parameter.Constraint.PositiveOnly bound1 bound2 with
                | None -> ()
                | Some p -> 
                    match p |> Parameter.bounds with
                    | Some (low,up) -> 
                        Expect.equal ([ bound1; bound2 ] |> Seq.min |> exp) low "Lower bounds were different"
                        Expect.equal ([ bound1; bound2 ] |> Seq.max |> exp) up "Upper bounds were different"
                    | None -> failtestf "Bounds were not accessible when they should be"

            testProperty "When unconstrained, bounds equal creation-time bounds" <| fun bound1 bound2 ->
                match Parameter.create Parameter.Constraint.Unconstrained bound1 bound2 with
                | None -> ()
                | Some p ->
                    match Parameter.bounds p with
                    | Some (low,up) -> 
                        Expect.equal ([ bound1; bound2 ] |> Seq.min) low "Lower bounds were different"
                        Expect.equal ([ bound1; bound2 ] |> Seq.max) up "Upper bounds were different"
                    | None -> failtestf "Bounds were not accessible when they should be"
        ]

        testList "Internal access (for optimisation)" [
            testProperty "Transformed value cannot be set as infinite or nan" <| fun con bound1 bound2 setAs ->
                match Parameter.create con bound1 bound2 with
                | None -> ()
                | Some p ->
                    let estimated = Parameter.setTransformedValue p setAs
                    if Double.IsNaN setAs || Double.IsInfinity setAs
                    then Expect.isError estimated "Did not error when infinity or NaN"

            testProperty "Internal value cannot be set as infinite or nan" <| fun con bound1 bound2 setAs ->
                match Parameter.create con bound1 bound2 with
                | None -> ()
                | Some p ->
                    let estimated = Parameter.setTransformedValue p setAs
                    let internalVal = Parameter.transformOut con setAs
                    if Double.IsNaN internalVal || Double.IsInfinity internalVal
                    then Expect.isError estimated "Did not error when internal value was infinity or NaN"

            testProperty "Transformed value equals set value when unconstrained" <| fun bound1 bound2 (setAs:FsCheck.NormalFloat) ->
                match Parameter.create Parameter.Constraint.Unconstrained bound1 bound2 with
                | None -> ()
                | Some p ->
                    Expect.equal 
                        (Parameter.setTransformedValue p setAs.Get |> Result.map Parameter.getTransformedValue) 
                        (Ok setAs.Get)
                        "The parameter was created but the estimate was different"
        ]

        testProperty "getEstimate returns the non-transformed (external) value" <| fun con bound1 bound2 (setAs:FsCheck.NormalFloat) ->
            match Parameter.create con bound1 bound2 with
            | None -> ()
            | Some p ->
                match Parameter.setTransformedValue p (setAs.Get |> Parameter.transformOut con) with
                | Ok p -> Expect.equal (p |> Parameter.getEstimate) (Ok setAs.Get) "finalise returned a different value to that set"
                | Error _ -> ()

        testList "Detached mode" [
            testProperty "Bounds always equal creation-time bounds" <| fun con bound1 bound2 ->
                match Parameter.create con bound1 bound2 with
                | None -> ()
                | Some p -> 
                    match p |> Parameter.detatchConstraint |> fst |> Parameter.bounds with
                    | Some (low,up) -> 
                        Expect.equal ([ bound1; bound2 ] |> Seq.min) low "Lower bounds were different"
                        Expect.equal ([ bound1; bound2 ] |> Seq.max) up "Upper bounds were different"
                    | None -> failtestf "Bounds were not accessible when they should be"
        ]
]