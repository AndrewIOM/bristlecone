module LanguageTests

open System
open Expecto
open Expecto.ExpectoFsCheck
open Bristlecone
open Bristlecone.Language
open Bristlecone.Numerics.Typed
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
              Constant a.Get + Constant b.Get |> compute Config.defaultNumEngine x t pool env = a.Get + b.Get

          testProperty "Subtraction operator subtracts two constants"
          <| fun (a: NormalFloat) (b: NormalFloat) x t pool env ->
              Constant a.Get - Constant b.Get |> compute Config.defaultNumEngine x t pool env = a.Get - b.Get

          testProperty "Multiplication operator multiplies two constants"
          <| fun (a: NormalFloat) (b: NormalFloat) x t pool env ->
              Constant a.Get * Constant b.Get |> compute Config.defaultNumEngine x t pool env = a.Get * b.Get

          testProperty "Division operator divides two constants"
          <| fun (a: NormalFloat) (b: NormalFloat) x t pool env ->
              Constant a.Get / Constant b.Get |> compute Config.defaultNumEngine x t pool env = a.Get / b.Get

          testProperty "Modulus operator finds mod of constant"
          <| fun (a: NormalFloat) (b: NormalFloat) x t pool env ->
              if b.Get = 0. then
                  true // Can't do mod of 0
              else
                  Constant a.Get % Constant b.Get |> compute x t pool env = a.Get % b.Get

          testProperty "Negative sign negates value"
          <| fun (a: NormalFloat) x t pool env -> -Constant a.Get |> compute x t pool env = -a.Get ]


let dummyParameterT = [|0.<parameter>|] |> Config.defaultOfVector

[<Tests>]
let inverseTests =
    testList
        "Inverse node"
        [

            testPropertyWithConfig Config.config "Round trip" <| fun pool ->
                let f r : ModelExpression<Config.testModelUnit ^2> = r * r
                let y = f (Constant 3.0<Config.testModelUnit>)
                let x' = Inverse f y (Constant 0.0<Config.testModelUnit>) (Constant 10.0<Config.testModelUnit>)

                let compiled = ExpressionCompiler.compileRate Config.defaultNumEngine.scalarBackend pool [] x'
                let t, this = Config.defaultOfScalar 0., Config.defaultOfScalar 0.<ModelSystem.state>
                let result = compiled dummyParameterT Map.empty t this |> Scalar.toFloat

                Config.floatEqualTol Accuracy.high result 3.0<ModelSystem.state/Time.``time index``> "Round-trip did not return expected value (tensor-mode)"
        ]


[<Tests>]
let modelExpressions =
    testList
        "Model expressions (compiled)"
        [

            testPropertyWithConfig Config.config "Constant expressions produce constant tensors" <| fun (value: NormalFloat) pool ->
                let expr = Constant value.Get
                let compiled = ExpressionCompiler.compileRate Config.defaultNumEngine.scalarBackend pool [] expr
                let t, this = Config.defaultOfScalar 0., Config.defaultOfScalar 0.<ModelSystem.state>
                let result = compiled dummyParameterT Map.empty t this
                Expect.floatClose Accuracy.high (result |> Scalar.toFloat |> Units.removeUnitFromFloat) value.Get "Constant should match"

            testPropertyWithConfig Config.config "'This' equals the current state"
            <| fun (x: NormalFloat) (t:NormalFloat) pool ->
                let fn = This |> ExpressionCompiler.compileRate Config.defaultNumEngine.scalarBackend pool []
                let t = Config.defaultOfScalar <| t.Get * 1.<Time.year>
                let this = Config.defaultOfScalar (x.Get * 1.<ModelSystem.state>)
                let result = fn dummyParameterT Map.empty t this
                Expect.floatClose Accuracy.high (result |> Scalar.toFloat |> Units.removeUnitFromFloat) x.Get "Constant should match"

            testPropertyWithConfig Config.config "'Time' equals the current time"
            <| fun (x: NormalFloat) (t:NormalFloat) pool ->
                let fn = Time |> ExpressionCompiler.compileRate Config.defaultNumEngine.scalarBackend pool []
                let t' = Config.defaultOfScalar <| t.Get * 1.<Time.year>
                let this = Config.defaultOfScalar (x.Get * 1.<ModelSystem.state>)
                let result = fn dummyParameterT Map.empty t' this
                Expect.floatClose Accuracy.high (result |> Scalar.toFloat |> Units.removeUnitFromFloat) t.Get "Constant should match"

            testPropertyWithConfig Config.config "Environment data retrieved to true value"
            <| fun (pool:Parameter.Pool.ParameterPool) (eCode:ShortCode.ShortCode) (eVal:NormalFloat) (t:NormalFloat) ->
                let envState = environment<Config.testModelUnit> eCode.Value
                let envMap = Map.ofList [ (envState.Code, eVal.Get * 1.<ModelSystem.environment> |> Config.defaultOfScalar) ]
                let t' = Config.defaultOfScalar <| t.Get * 1.<Time.year>
                let dummyThis = Config.defaultOfScalar 999.<ModelSystem.state>
                let fn = Environment envState |> ExpressionCompiler.compileRate Config.defaultNumEngine.scalarBackend pool [ eCode ]
                let result = fn dummyParameterT envMap t' dummyThis
                Expect.floatClose Accuracy.high (result |> Scalar.toFloat |> Units.removeUnitFromFloat) eVal.Get "Constant should match"

            testPropertyWithConfig Config.config "Parameter retrieved to true value"
            <| fun (pool:Parameter.Pool.ParameterPool) (pVal:NormalFloat) (t:NormalFloat) ->
                let paramToTest = pool |> Parameter.Pool.keys |> randomiseList |> Seq.head
                let dummyParam = parameter paramToTest.Value NoConstraints 0.1 0.2 
                let fakePoolVector = pool |> Parameter.Pool.keys |> Seq.map(fun pn -> if pn = paramToTest then pVal.Get * 1.<parameter> else nan * 1.<parameter>) |> Seq.toArray |> Tensors.Typed.ofVector
                let t' = Config.defaultOfScalar <| t.Get * 1.<Time.year>
                let this = Config.defaultOfScalar 999.<ModelSystem.state>
                let fn = P dummyParam |> ExpressionCompiler.compileRate Config.defaultNumEngine.scalarBackend pool []
                let result = fn fakePoolVector Map.empty t' this
                Expect.floatClose Accuracy.high (result |> Scalar.toFloat |> Units.removeUnitFromFloat) pVal.Get "Parameter value didn't match"

            testPropertyWithConfig Config.config "Greater than short-circuits on invalid (nan)" <| fun _ ->
                let safe = Conditional (Constant 200. .> Constant 100.) (Constant 1.0) (Invalid)
                let r = ExpressionCompiler.compileSimple safe
                Expect.floatClose Accuracy.high r 1.0

        ]


[<Tests>]
let modelBuilder =
    testList
        "Model builder"
        [

            testProperty "Throws if more than one likelihood function"
            <| fun likelihoodFns ->
                let f () =
                    likelihoodFns
                    |> Seq.fold (fun mb l -> mb |> Model.useLikelihoodFunction l) Model.empty
                    |> Model.addRateEquation (state "x") (Constant 1.)
                    |> Model.compileWith Config.defaultNumEngine

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
            <| fun likelihood (NonEmptyArray (measures: ShortCode.ShortCode array)) ->
                if measures |> Seq.hasDuplicates then ()
                else
                    let model =
                        Model.discrete
                        |> Model.useLikelihoodFunction likelihood
                        |> Model.addDiscreteEquation (state "eq1") (Constant 1.)

                    measures
                    |> Array.fold (fun mb measureCode -> mb |> Model.addMeasure (measure measureCode.Value) (Constant 1.)) model
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
                            (Model.discrete |> Model.useLikelihoodFunction likelihood)
                        |> fun mb ->
                            Seq.fold
                                (fun mb (n: ShortCode.ShortCode, _) ->
                                    mb |> Model.addMeasure (measure n.Value) (Constant 1.))
                                mb
                                measures
                        |> Model.compile

                    let keys = [ eqs; (measures |> List.map fst) ] |> List.concat

                    if keys |> Seq.hasDuplicates then
                        Expect.throws (compile >> ignore) "Duplicate keys existed"
                    else
                        compile () |> ignore

          ]

let dummyBaseModel comp =
    let s = state "some_random_code"
    Model.empty<Bristlecone.Time.day>
    |> Model.addRateEquation s (comp * Constant 1.</Time.day>)
    |> Model.useLikelihoodFunction  (ModelLibrary.NegLogLikelihood.SumOfSquares [ Require.state s ])

[<Tests>]
let hypotheses =
    testList
        "Hypotheses (nested model systems)"
        [

            testPropertyWithConfig Config.config "Components must have at least one implementation" <| fun (comp:Components.ModelComponent<ModelExpression<1>>) ->
                if comp.Implementations.IsEmpty
                then Expect.throws (fun () -> Hypotheses.createFromModel dummyBaseModel |> Hypotheses.apply comp |> ignore) ""

            testPropertyWithConfig Config.config "Sub-component parameters are included in final models" <| fun (comp:Components.ModelComponent<ModelExpression<1>>) ->
                if comp.Implementations.IsEmpty then ()
                else 
                    let h = dummyBaseModel |> Hypotheses.createFromModel |> Hypotheses.apply comp |> Hypotheses.compile Config.defaultNumEngine
                    let p =
                        comp.Implementations
                        |> List.allPairs h
                        |> List.map(fun (h1, subC) -> h1.Model.Parameters, subC)
                    Expect.all p (fun (pool,subC) ->
                        let poolKeys = pool |> Parameter.Pool.keys
                        subC.Parameters |> Seq.map(fun kv -> poolKeys |> List.contains kv.Key) |> Seq.contains false |> not)
                        "Hypothesis did not include sub-component's parameters"

            testPropertyWithConfig Config.config "The number of hypotheses generated equals the number of sub-components" <| fun (comp:Components.ModelComponent<ModelExpression<1>>) ->
                if comp.Implementations.IsEmpty then true
                else
                    let h = dummyBaseModel |> Hypotheses.createFromModel |> Hypotheses.apply comp |> Hypotheses.compile Config.defaultNumEngine
                    h.Length = comp.Implementations.Length

            testPropertyWithConfig Config.config "The number of hypotheses equals the product of three component lists"
            <| fun
                (comp1:Components.ModelComponent<ModelExpression<1>>) 
                (comp2:Components.ModelComponent<ModelExpression<1>>) 
                (comp3:Components.ModelComponent<ModelExpression<1>>) ->
                if comp1.Implementations.IsEmpty || comp2.Implementations.IsEmpty || comp3.Implementations.IsEmpty then true
                else
                    let dummy = fun c1 c2 c3 -> dummyBaseModel (c1 * c2 * c3)
                    let h =
                        dummy
                        |> Hypotheses.createFromModel
                        |> Hypotheses.apply comp1
                        |> Hypotheses.apply comp2
                        |> Hypotheses.apply comp3
                        |> Hypotheses.compile Config.defaultNumEngine
                    h.Length = comp1.Implementations.Length * comp2.Implementations.Length * comp3.Implementations.Length

        ]

[<Tests>]
let modelCE =
    testList "model computation expression" [

        testCase "Cool" <| fun _ ->
            let m = model {
                estimate x
                likelihood cool
            }
        
    ]