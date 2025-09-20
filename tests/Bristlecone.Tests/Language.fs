module LanguageTests

open System
open Expecto
open Expecto.ExpectoFsCheck
open Bristlecone
open Bristlecone.Language
open FsCheck

let rnd = System.Random()

let randomiseList ls =
    ls |> List.map(fun l -> l, rnd.Next())
    |> List.sortBy snd
    |> List.map fst

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


open Bristlecone.Tensors

let dummyParameterT = [|0.<parameter>|] |> Typed.ofVector


[<Tests>]
let modelExpressionsTensor =
    testList
        "Model expression - DSL compilation (tensors)"
        [

            testPropertyWithConfig Config.config "Will not compile if environment key is missing"
            <| fun (code: ShortCode.ShortCode) x t pool e ->
                let envState = environment<Config.testModelUnit> code.Value
                let f () = Environment envState |> ExpressionCompiler.compileRate pool e |> ignore
                if e |> List.contains code |> not
                then Expect.throws f "Environmental data was not present"

        ]


[<Tests>]
let modelExpressions =
    testList
        "Model expressions (compiled)"
        [

            testPropertyWithConfig Config.config "Constant expressions produce constant tensors" <| fun (value: NormalFloat) pool ->
                let expr = Constant value.Get
                let compiled = ExpressionCompiler.compileRate pool [] expr
                let t, this = Typed.ofScalar 0., Typed.ofScalar 0.<ModelSystem.state>
                let result = compiled dummyParameterT Map.empty t this
                Expect.floatClose Accuracy.high (result |> Typed.toFloatScalar |> Units.removeUnitFromFloat) value.Get "Constant should match"

            testPropertyWithConfig Config.config "'This' equals the current state"
            <| fun (x: NormalFloat) (t:NormalFloat) pool ->
                let fn : ModelSystem.RateEquation<Time.year> = This |> ExpressionCompiler.compileRate pool []
                let t = Typed.ofScalar <| t.Get * 1.<Time.year>
                let this = Typed.ofScalar (x.Get * 1.<ModelSystem.state>)
                let result = fn dummyParameterT Map.empty t this
                Expect.floatClose Accuracy.high (result |> Typed.toFloatScalar |> Units.removeUnitFromFloat) x.Get "Constant should match"

            testPropertyWithConfig Config.config "'Time' equals the current time"
            <| fun (x: NormalFloat) (t:NormalFloat) pool ->
                let fn : ModelSystem.RateEquation<Time.year> = Time |> ExpressionCompiler.compileRate pool []
                let t' = Typed.ofScalar <| t.Get * 1.<Time.year>
                let this = Typed.ofScalar (x.Get * 1.<ModelSystem.state>)
                let result = fn dummyParameterT Map.empty t' this
                Expect.floatClose Accuracy.high (result |> Typed.toFloatScalar |> Units.removeUnitFromFloat) t.Get "Constant should match"

            testPropertyWithConfig Config.config "Environment data retrieved to true value"
            <| fun (code: ShortCode.ShortCode) (x:NormalFloat) (t:NormalFloat) pool ->
                let envState = environment<Config.testModelUnit> code.Value
                let envMap = Map.ofList [ envState.Code, x.Get * 1.<ModelSystem.environment> |> Typed.ofScalar ]
                let t' = Typed.ofScalar <| t.Get * 1.<Time.year>
                let this = Typed.ofScalar (x.Get * 1.<ModelSystem.state>)
                let fn = Environment envState |> ExpressionCompiler.compileRate pool [ code ]
                let result = fn dummyParameterT envMap t' this
                Expect.floatClose Accuracy.high (result |> Typed.toFloatScalar |> Units.removeUnitFromFloat) t.Get "Constant should match"

            // testPropertyWithConfig Config.config "Parameter retrieved to true value"
            // <| fun (code: ShortCode.ShortCode) (x:NormalFloat) (t:NormalFloat) pool ->
            //     let randomFromPool = pool |> Parameter.Pool.keys |> randomiseList |> List.head
            //     let fromPool = Parameter.Pool.tryGetRealValue randomFromPool.Value pool |> Option.get
            //     let pLang = parameter randomFromPool.Value 
            //     let t' = Typed.ofScalar <| t.Get * 1.<Time.year>
            //     let this = Typed.ofScalar (x.Get * 1.<ModelSystem.state>)
            //     let fn = P randomFromPool |> ExpressionCompiler.compileRate pool [ code ]
            //     let result = fn dummyParameterT envMap t' this
            //     Expect.floatClose Accuracy.high (result |> Typed.toFloatScalar |> Units.removeUnitFromFloat) t.Get "Constant should match"

        ]


[<Tests>]
let modelBuilder =
    testList
        "Model builder"
        [

            testProperty "Throws if more than one likelihood function"
            <| fun (likelihoodFns: ModelSystem.Likelihood<ModelSystem.state> list) ->
                let f () =
                    likelihoodFns
                    |> Seq.fold (fun mb l -> mb |> Model.useLikelihoodFunction l) Model.empty
                    |> Model.addRateEquation (state "x") (Constant 1.)
                    |> Model.compile

                if likelihoodFns |> Seq.length <> 1 then
                    Expect.throws (fun () -> f () |> ignore) "Allowed more than one likelihood function"

            testPropertyWithConfig Config.config "Throws if no equations are specified"
            <| fun (eqs: (ShortCode.ShortCode * ModelExpression<Config.testModelUnit>) seq) ->
                if eqs |> Seq.map fst |> Seq.hasDuplicates then
                    ()
                else
                    let mb =
                        eqs |> Seq.fold (fun mb (n, eq) ->
                            let n = state n.Value
                            mb |> Model.addRateEquation n eq) Model.empty
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
                            (fun mb k ->
                                let k = state k.Value
                                mb |> Model.addRateEquation k (Constant 1.))
                            (Model.empty |> Model.useLikelihoodFunction l)

                    if eqs |> Seq.isEmpty then
                        Expect.throws
                            (fun () -> mb |> Model.compile |> ignore)
                            "Did not error when no equations specified"
                    else
                        mb |> Model.compile |> ignore

            testPropertyWithConfig Config.config "Compiles whether measures are present or not"
            <| fun likelihood (measures: CodedMap<ModelExpression<ModelSystem.state>>) ->
                if measures.Keys |> Seq.hasDuplicates then
                    ()
                else
                    let model =
                        Model.empty
                        |> Model.useLikelihoodFunction likelihood
                        |> Model.addDiscreteEquation (state "eq1") (Constant 1.)

                    measures
                    |> Map.fold (fun mb n m -> mb |> Model.addMeasure (measure n.Value) m) model
                    |> Model.compile
                    |> ignore

            testPropertyWithConfig Config.config "Doesn't compile if duplicate keys exist"
            <| fun
                    likelihood
                    (eqs: ShortCode.ShortCode list)
                    (measures: (ShortCode.ShortCode * ModelExpression<ModelSystem.state>) list) ->
                if eqs.IsEmpty then
                    ()
                else
                    let compile () =
                        eqs
                        |> Seq.fold
                            (fun mb n -> mb |> Model.addDiscreteEquation (state n.Value) This)
                            (Model.empty |> Model.useLikelihoodFunction likelihood)
                        |> fun mb ->
                            Seq.fold
                                (fun mb (n: ShortCode.ShortCode, eq) ->
                                    mb |> Model.addMeasure (measure n.Value) eq)
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
