module LanguageTests

open System
open Expecto
open Expecto.ExpectoFsCheck
open Bristlecone
open Bristlecone.Language
open FsCheck

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
          <| fun x (t: NormalFloat) pool env -> Time |> compute x (t.Get * 1.<Time.``time index``>) pool env = t.Get

          testProperty "A constant is purely represented"
          <| fun (c: NormalFloat) x t pool env -> Constant c.Get |> compute x t pool env = c.Get

          testPropertyWithConfig Config.config "Getting parameter value fails when parameter not present"
          <| fun (code: ShortCode.ShortCode) x t pool e ->
              let f () =
                  Parameter code.Value |> compute x t pool e

              match
                  pool
                  |> Parameter.Pool.toList
                  |> List.tryFind (fun (k, _) -> k.Value = code.Value)
              with
              | Some p ->
                  if p |> snd |> Parameter.isEstimated then
                      Expect.equal
                          (f ())
                          (p |> snd |> Parameter.getTransformedValue)
                          "Did not fail when parameter was not present"
              | None -> Expect.throws (fun () -> f () |> ignore) "Parameter was not present"

          testPropertyWithConfig Config.config "Getting parameter values returns real value when present"
          <| fun pool x t e ->
              let selectedCode =
                  Gen.elements (pool |> Parameter.Pool.toList |> List.map fst)
                  |> Gen.sample 1 1
                  |> List.head

              if
                  not
                      (pool
                       |> Parameter.Pool.hasParameter selectedCode.Value
                       |> Option.map Parameter.isEstimated)
                          .Value
              then
                  ()
              else

                  let existingValue =
                      pool |> Parameter.Pool.tryGetRealValue selectedCode.Value |> Option.get

                  let result = Parameter selectedCode.Value |> compute x t pool e

                  Expect.equal result existingValue "The parameter value was not correct"

          testPropertyWithConfig Config.config "Fails when environmental (aka time-varying) data is not present"
          <| fun (code: ShortCode.ShortCode) x t pool e ->
              let f () =
                  Environment code.Value |> compute x t pool e

              match e |> Map.tryFindBy (fun m -> m.Value = code.Value) with
              | Some environ -> Expect.equal (f ()) environ "Did not fail when environment data was not present"
              | None -> Expect.throws (fun () -> f () |> ignore) "Environmental data was not present"

          testPropertyWithConfig Config.config "Retrieves environment when present"
          <| fun identifier (value: NormalFloat) x t pool ->
              let e: CodedMap<float> = Map.ofList [ identifier, value.Get ]
              Environment identifier.Value |> compute x t pool e = value.Get ]

[<Tests>]
let modelBuilder =
    testList
        "Model builder"
        [

          testProperty "Does not compile when more than one likelihood function"
          <| fun (likelihoodFns: ModelSystem.LikelihoodFn<float> list) ->
              let f () =
                  likelihoodFns
                  |> Seq.fold (fun mb l -> mb |> Model.useLikelihoodFunction l) Model.empty
                  |> Model.addEquation "x" (Constant 1.)
                  |> Model.compile

              if likelihoodFns |> Seq.length <> 1 then
                  Expect.throws (fun () -> f () |> ignore) "Allowed more than one likelihood function"

          testProperty "Cannot add an equation with a blank identifier"
          <| fun name eq ->
              if String.IsNullOrEmpty name then
                  Expect.throws
                      (fun () -> Model.empty |> Model.addEquation name eq |> ignore)
                      "Allowed a blank identifier"

          testPropertyWithConfig Config.config "Does not compile when no equations are specified"
          <| fun (eqs: (string * ModelExpression) seq) ->
              if eqs |> Seq.map fst |> Seq.hasDuplicates then
                  ()
              else
                  let mb =
                      eqs |> Seq.fold (fun mb (n, eq) -> mb |> Model.addEquation n eq) Model.empty

                  let fn () = mb |> Model.compile

                  if eqs |> Seq.length <> 1 then
                      Expect.throws (fun () -> fn () |> ignore) "Did not throw when no equations specified"

          testPropertyWithConfig
              Config.config
              "Compiles with one likelihood function and one or more equations (no duplicate keys)"
          <| fun l (eqs: ShortCode.ShortCode list) ->
              if eqs |> Seq.hasDuplicates then
                  ()
              else
                  let mb =
                      eqs
                      |> List.fold
                          (fun mb k -> mb |> Model.addEquation k.Value (Constant 1.))
                          (Model.empty |> Model.useLikelihoodFunction l)

                  if eqs |> Seq.isEmpty then
                      Expect.throws
                          (fun () -> mb |> Model.compile |> ignore)
                          "Did not error when no equations specified"
                  else
                      mb |> Model.compile |> ignore

          testPropertyWithConfig Config.config "Compiles whether measures are present or not"
          <| fun likelihood (measures: CodedMap<ModelSystem.Measurement<float>>) ->
              if measures.Keys |> Seq.hasDuplicates then
                  ()
              else
                  let model =
                      Model.empty
                      |> Model.useLikelihoodFunction likelihood
                      |> Model.addEquation "eq1" (Constant 1.)

                  measures
                  |> Map.fold (fun mb n m -> mb |> Model.includeMeasure n.Value m) model
                  |> Model.compile
                  |> ignore

          testPropertyWithConfig Config.config "Doesn't compile if duplicate keys exist"
          <| fun
                 likelihood
                 (eqs: ShortCode.ShortCode list)
                 (measures: (ShortCode.ShortCode * ModelSystem.Measurement<float>) list) ->
              if eqs.IsEmpty then
                  ()
              else
                  let compile () =
                      eqs
                      |> Seq.fold
                          (fun mb n -> mb |> Model.addEquation n.Value This)
                          (Model.empty |> Model.useLikelihoodFunction likelihood)
                      |> fun mb ->
                          Seq.fold
                              (fun mb (n: ShortCode.ShortCode, eq) -> mb |> Model.includeMeasure n.Value eq)
                              mb
                              measures
                      |> Model.compile

                  let keys = [ eqs; (measures |> List.map fst) ] |> List.concat

                  if keys |> Seq.hasDuplicates then
                      Expect.throws (compile >> ignore) "Duplicate keys existed"
                  else
                      compile () |> ignore

          // testProperty "Only compiles when all required parameters are specified" <| fun (pool:Parameter.Pool) ->

          // testProperty "Only compiles when all specified parameters are used" <| fail

          // testProperty "Equations in the built model have the correct result" <| fail

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
