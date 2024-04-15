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
        return String chars
    }

let tupleGen<'snd> fn =
    gen {
        let! length = Gen.choose (0, 100)
        let! list1 = Gen.listOfLength length fn
        let! list2 = Gen.listOfLength length (Arb.generate<'snd>)
        return Seq.zip list1 list2
    }

type ShortCodeGen() =
    static member ShortCode() : Arbitrary<ShortCode.ShortCode> =
        let createCode code = ShortCode.create code |> Option.get
        genStrings 1 10 |> Gen.map createCode |> Arb.fromGen

    static member EquationList = genStrings 1 10 |> tupleGen<ModelExpression> |> Arb.fromGen

    static member MeasureList =
        genStrings 1 10 |> tupleGen<ModelSystem.MeasureEquation> |> Arb.fromGen

    static member Pool =
        gen {
            let! n = Gen.choose (1, 100)
            let! codes = Gen.listOfLength n Arb.generate<ShortCode.ShortCode>
            let! bounds1 = Gen.listOfLength n Arb.generate<NormalFloat>
            let! bounds2 = Gen.listOfLength n Arb.generate<NormalFloat>

            return
                List.zip3 codes bounds1 bounds2
                |> List.map (fun (c, b1, b2) -> c, Parameter.create noConstraints b1.Get b2.Get |> Option.get)
                |> Parameter.Pool.fromList
        }
        |> Arb.fromGen

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<ShortCodeGen> ] }


[<Tests>]
let modelExpressionOperators =
    testList
        "Model expression - operators"
        [

          testProperty "Addition operator adds two constants"
          <| fun (a: NormalFloat) (b: NormalFloat) x t pool env ->
              Constant a.Get + Constant b.Get |> compute x t pool env = a.Get + b.Get

          testProperty "Subtraction operator subtracts two constants"
          <| fun (a: NormalFloat) (b: NormalFloat) x t pool env ->
              Constant a.Get - Constant b.Get |> compute x t pool env = a.Get - b.Get

          testProperty "Multiplication operator multiplies two constants"
          <| fun (a: NormalFloat) (b: NormalFloat) x t pool env ->
              Constant a.Get * Constant b.Get |> compute x t pool env = a.Get * b.Get

          testProperty "Division operator divides two constants"
          <| fun (a: NormalFloat) (b: NormalFloat) x t pool env ->
              Constant a.Get / Constant b.Get |> compute x t pool env = a.Get / b.Get

          testProperty "Modulus operator finds mod of constant"
          <| fun (a: NormalFloat) (b: NormalFloat) x t pool env ->
              if b.Get = 0. then
                  true // Can't do mod of 0
              else
                  Constant a.Get % Constant b.Get |> compute x t pool env = a.Get % b.Get

          testProperty "Negative sign negates value"
          <| fun (a: NormalFloat) x t pool env -> -Constant a.Get |> compute x t pool env = -a.Get ]

[<Tests>]
let modelExpressions =
    testList
        "Model expression - compute"
        [

          testProperty "'This' equals the current state"
          <| fun (x: NormalFloat) t pool env -> This |> compute x.Get t pool env = x.Get

          testProperty "'Time' equals the current time"
          <| fun x (t: NormalFloat) pool env -> Time |> compute x t.Get pool env = t.Get

          testProperty "A constant is purely represented"
          <| fun (c: NormalFloat) x t pool env -> Constant c.Get |> compute x t pool env = c.Get

          testProperty "Getting parameter value fails when parameter not present"
          <| fun code x t pool e ->
              let f () = Parameter code |> compute x t pool e

              match pool |> Parameter.Pool.toList |> List.tryFind (fun (k, v) -> k.Value = code) with
              | Some p ->
                  Expect.equal
                      (f ())
                      (p |> snd |> Parameter.getTransformedValue)
                      "Did not fail when parameter was not present"
              | None -> Expect.throws (fun () -> f |> ignore) "Parameter was not present"

          testPropertyWithConfig config "Getting parameter values returns real value when present"
          <| fun pool x t e ->
              let selectedCode =
                  Gen.elements (pool |> Parameter.Pool.toList |> List.map fst)
                  |> Gen.sample 1 1
                  |> List.head

              let result = Parameter selectedCode.Value |> compute x t pool e

              let existingValue =
                  pool |> Parameter.Pool.tryGetRealValue selectedCode.Value |> Option.get

              Expect.equal result existingValue "The parameter value was not correct"

          testProperty "Fails when environmental (aka time-varying) data is not present"
          <| fun code x t pool e ->
              let f () = Environment code |> compute x t pool e

              match e |> Map.tryFindBy (fun m -> m.Value = code) with
              | Some environ -> Expect.equal (f ()) environ "Did not fail when parameter was not present"
              | None -> Expect.throws (fun () -> f |> ignore) "The parameter was not present"

          testPropertyWithConfig config "Retrieves environment when present"
          <| fun identifier (value: NormalFloat) x t pool ->
              let e: CodedMap<float> = Map.ofList [ identifier, value.Get ]
              Environment identifier.Value |> compute x t pool e = value.Get ]

[<Tests>]
let modelBuilder =
    testList
        "Model builder"
        [

          testProperty "Does not compile when more than one likelihood function"
          <| fun (likelihoodFns: ModelSystem.LikelihoodFn list) ->
              let mb =
                  likelihoodFns
                  |> Seq.fold (fun mb l -> mb |> Model.useLikelihoodFunction l) Model.empty

              let fn () =
                  mb |> Model.addEquation "x" (Constant 1.) |> Model.compile

              if likelihoodFns |> Seq.length <> 1 then
                  Expect.throws (fun () -> fn () |> ignore) "Allowed more than one likelihood function"

          testProperty "Cannot add an equation with a blank identifier"
          <| fun name eq ->
              if String.IsNullOrEmpty name then
                  Expect.throws
                      (fun () -> Model.empty |> Model.addEquation name eq |> ignore)
                      "Allowed a blank identifier"

          testPropertyWithConfig config "Does not compile when no equations are specified"
          <| fun (eqs: (string * ModelExpression) seq) ->
              let mb =
                  eqs |> Seq.fold (fun mb (n, eq) -> mb |> Model.addEquation n eq) Model.empty

              let fn () = mb |> Model.compile

              if eqs |> Seq.length <> 1 then
                  Expect.throws (fun () -> fn () |> ignore) "Did not throw when no equations specified"

          testPropertyWithConfig config "Compiles with one likelihood function and one or more equations"
          <| fun l eqs ->
              let mb =
                  eqs
                  |> Seq.fold
                      (fun mb (n, eq) -> mb |> Model.addEquation n eq)
                      (Model.empty |> Model.useLikelihoodFunction l)

              if eqs |> Seq.isEmpty then
                  Expect.throws (fun () -> mb |> Model.compile |> ignore) "Did not error when no equations specified"
              else
                  mb |> Model.compile |> ignore

          testPropertyWithConfig config "Compiles whether measures are present or not"
          <| fun likelihood eq1 measures ->
              let model =
                  Model.empty
                  |> Model.useLikelihoodFunction likelihood
                  |> Model.addEquation "eq1" eq1

              measures
              |> Seq.fold (fun mb (n, m) -> mb |> Model.includeMeasure n m) model
              |> Model.compile
              |> ignore

          testProperty "Doesn't compile if duplicate keys exist"
          <| fun likelihood eqs measures ->

              // May fail if strings are null or empty
              // May fail if same string within group (e.g. equations)

              let model =
                  eqs
                  |> Seq.fold
                      (fun mb (n, eq) -> mb |> Model.addEquation n eq)
                      (Model.empty |> Model.useLikelihoodFunction likelihood)

              let keys = [ (eqs |> List.map fst); (measures |> List.map fst) ] |> List.concat

              if keys.Length = (keys |> List.distinct |> List.length) then
                  Model.compile model |> ignore
              else
                  Expect.throws (fun () -> Model.compile model |> ignore) "Duplicate keys existed"

          //         testProperty "Only compiles when all required parameters are specified" <| fail

          //         testProperty "Only compiles when all specified parameters are used" <| fail

          //         testProperty "Equations in the built model have the correct result" <| fail

          ]

// [<Tests>]
// let computableFragments =
//     testList "Computable fragments" [

//         testProperty "" <| fun x ->

//             let someFun x t z =
//                 x + t * 1.0 - z

//             let fragment =
//                 someFun
//                 |> ComputableFragment.apply This
//                 |> ComputableFragment.applyAgain Time
//                 |> ComputableFragment.applyAgain (Environment "z")
//                 //|> ComputableFragment.asBristleconeFunction

//             let fragment2 = fragment |> ComputableFragment.asBristleconeFunction

//             // Get something


//             fragment

//     ]

[<Tests>]
let hypotheses =
    testList
        "Hypotheses (nested model systems)"
        [

        // testProperty "Components must have at least one implementation" <| fun baseModel components ->
        //     if components |> List.isEmpty
        //     then Expect.throws (fun () -> Hypotheses.createFromComponent components baseModel |> ignore) ""

        // testProperty "Sub-component parameters are included in final models" <| fun baseModel (name:ShortCode.ShortCode) comp ->
        //     let h = baseModel |> Hypotheses.createFromComponent (name.Value, comp) |> Hypotheses.compile
        //     let p =
        //         comp
        //         |> List.map(fun (n,p) -> p.Parameters)
        //         |> List.zip h
        //         |> List.map(fun ((h1,_), p2) -> h1.Parameters, p2)
        //     Expect.all p (fun (pool,p2) -> p2 |> Map.map(fun k _ -> pool |> Parameter.Pool.toList |> Map.containsKey k) |> Map.toList |> List.map snd)

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
