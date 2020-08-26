module LanguageTests

open System
open Expecto
open Expecto.ExpectoFsCheck
open Bristlecone
open Bristlecone.Language
open FsCheck

let genStrings minLength maxLength = 
    gen {
        let! length = Gen.choose (minLength, maxLength)
        let! chars = Gen.arrayOfLength length Arb.generate<char>
        return string chars
    }

type ShortCodeGen() =
    static member ShortCode() : Arbitrary<ShortCode.ShortCode> =
        let createCode code = ShortCode.create code |> Option.get
        genStrings 1 10 |> Gen.map createCode |> Arb.fromGen


let config = { FsCheckConfig.defaultConfig with arbitrary = [typeof<ShortCodeGen>] }


[<Tests>]
let modelExpressionOperators =
    testList "Model expression - operators" [

        testProperty "Addition operator adds two constants" <| fun (a:NormalFloat) (b:NormalFloat) x t pool env ->
            Constant a.Get + Constant b.Get |> compute x t pool env = a.Get + b.Get

        testProperty "Subtraction operator subtracts two constants" <| fun (a:NormalFloat) (b:NormalFloat) x t pool env ->
            Constant a.Get - Constant b.Get |> compute x t pool env = a.Get - b.Get
        
        testProperty "Multiplication operator multiplies two constants" <| fun (a:NormalFloat) (b:NormalFloat) x t pool env ->
            Constant a.Get * Constant b.Get |> compute x t pool env = a.Get * b.Get
        
        testProperty "Division operator divides two constants" <| fun (a:NormalFloat) (b:NormalFloat) x t pool env ->
            Constant a.Get / Constant b.Get |> compute x t pool env = a.Get / b.Get

        testProperty "Modulus operator finds mod of constant" <| fun (a:NormalFloat) (b:NormalFloat) x t pool env ->
            if b.Get = 0. then true // Can't do mod of 0
            else Constant a.Get % b.Get |> compute x t pool env = a.Get % b.Get
    
        testProperty "Negative sign negates value" <| fun (a:NormalFloat) x t pool env ->
            - Constant a.Get |> compute x t pool env = - a.Get
    ]

[<Tests>]
let modelExpressions =
    testList "Model expression - compute" [

        testProperty "'This' equals the current state" <| fun (x:NormalFloat) t pool env ->
            This |> compute x.Get t pool env = x.Get

        testProperty "'Time' equals the current time" <| fun x (t:NormalFloat) pool env ->
            Time |> compute x t.Get pool env = t.Get

        testProperty "A constant is purely represented" <| fun (c:NormalFloat) x t pool env ->
            Constant c.Get |> compute x t pool env = c.Get

        // testProperty "Fails when parameter does not exist or returns value if present" <| fun code x t pool e ->
        //     let f () = Parameter code |> compute x t pool e
        //     match pool |> Parameter.Pool.asList |> List.tryFind (fun (k,v) -> k = ShortCode.create code) with
        //     | Some p -> Expect.equal (f ()) (p |> snd |> Parameter.getEstimate) "Did not fail when environment was not present"
        //     | None -> Expect.throws (fun () -> f |> ignore) "Environment was not present"

        // testProperty "Retrieves parameter value when present" <| fun paramCode con bound1 bound2 x t e ->
        //     printfn "Getting param: %A %A %A %A" paramCode con bound1 bound2
        //     printfn "Param is %A" (Parameter.create con bound1 bound2 )
        //     printfn "Pool is %A" (code "cool")
        //     let paramPool = Parameter.Pool.fromList [ code paramCode, Parameter.create con bound1 bound2 ]
        //     printfn "B: %A" ((paramPool |> Parameter.Pool.getEstimate paramCode))
        //     printfn "Computing... %A" (Parameter paramCode) 
        //     Parameter paramCode |> compute x t paramPool e = (paramPool |> Parameter.Pool.getEstimate paramCode)

//         testProperty "Fails when environmental (aka time-varying) data is not present" <| fun code x t pool e ->
//             let f () = Environment code |> compute x t pool e
//             match e |> Map.tryFind (ShortCode.create code) with
//             | Some environ -> Expect.equal (f ()) environ "Did not fail when parameter was not present"
//             | None -> Expect.throws (fun () -> f |> ignore) "The parameter was not present"

//         testProperty "Retrieves environment when present" <| fun c value x t pool ->
//             let e = Map.ofList [ code c, value ]
//             Environment c |> compute x t pool e = value
    ]

[<Tests>]
let modelBuilder =
    testList "Model builder" [

        testProperty "Does not compile when more than one likelihood function" <| fun (likelihoodFns:ModelSystem.Likelihood list) ->
            let mb = likelihoodFns |> Seq.fold (fun mb l -> mb |> Model.useLikelihoodFunction l) Model.empty
            let fn () = mb |> Model.addEquation "x" (Constant 1.) |> Model.compile
            if likelihoodFns |> Seq.length <> 1
            then Expect.throws (fun () -> fn() |> ignore) "Allowed more than one likelihood function"

        testProperty "Does not compile when no equations are specified" <| fun eqs ->
            let mb = eqs |> Seq.fold (fun mb (n,eq) -> mb |> Model.addEquation n eq) Model.empty
            let fn () = mb |> Model.compile
            if eqs |> Seq.length <> 1
            then Expect.throws (fun () -> fn() |> ignore) "Allowed more than one likelihood function"

        testPropertyWithConfig config "Compiles with one likelihood function and one or more equations" <| fun l eqs ->
            let mb = eqs |> Seq.fold (fun mb (n,eq) -> mb |> Model.addEquation n eq) (Model.empty |> Model.useLikelihoodFunction l)
            if eqs |> Seq.isEmpty
            then Expect.throws (fun () -> mb |> Model.compile |> ignore) "Did not error when no equations specified"
            else mb |> Model.compile |> ignore

        testProperty "Compiles whether measures are present or not" <| fun likelihood eq1 measures ->
            let model =
                Model.empty 
                |> Model.useLikelihoodFunction likelihood 
                |> Model.addEquation "eq1" eq1
            measures |> Seq.fold (fun mb (n,m) -> mb |> Model.includeMeasure n m) model |> Model.compile

        testProperty "Doesn't compile if duplicate keys exist" <| fun likelihood eqs measures ->
            let model = eqs |> Seq.fold (fun mb (n,eq) -> mb |> Model.addEquation n eq) (Model.empty |> Model.useLikelihoodFunction likelihood)
            let keys = [(eqs |> List.map fst); (measures |> List.map fst)] |> List.concat
            if keys.Length = (keys |> List.distinct |> List.length)
            then Model.compile model |> ignore
            else Expect.throws (fun () -> Model.compile model |> ignore) "Duplicate keys existed"

//         testProperty "Only compiles when all required parameters are specified" <| fail

//         testProperty "Only compiles when all specified parameters are used" <| fail

//         testProperty "Equations in the built model have the correct result" <| fail

   ]

// [<Tests>]
// let computableFragments =
//     testList "Computable fragments" [

//         testProperty "" <| fail

//     ]

[<Tests>]
let hypotheses =
    testList "Hypotheses (nested model systems)" [

//         testProperty "Sub-components must have unique IDs" <| fun baseModel comp ->
//             //Expect.throws(baseModel |> Hypotheses.createFromComponent comp) ""
//             fail

        // testProperty "The number of hypotheses generated equals the number of sub-components" <| fun baseModel comp ->
        //     if comp |> snd |> List.isEmpty then true
        //     else
        //         let h = baseModel |> Hypotheses.createFromComponent comp |> fun x -> printfn "%A" x; x |> Hypotheses.compile
        //         h.Length = (comp |> snd).Length

//         testProperty "The number of hypotheses equals the product of three component lists" <| fun baseModel comp1 comp2 comp3 ->
//             let h = 
//                 baseModel
//                 |> Hypotheses.createFromComponent comp1
//                 |> Hypotheses.useAnother comp2
//                 |> Hypotheses.useAnother comp3
//                 |> Hypotheses.compile
//             h.Length = ((snd comp1).Length * (snd comp2).Length * (snd comp3).Length)

//         testProperty "Can optionally pass component ID into model" <| fail

//         testProperty "Correctly names components included in each model" <| fun baseModel comp1 comp2 comp3 ->
//             let h =
//                 baseModel
//                 |> Hypotheses.createFromComponent comp1
//                 |> Hypotheses.useAnother comp2
//                 |> Hypotheses.useAnother comp3
//                 |> Hypotheses.compile
//             fail

    ]