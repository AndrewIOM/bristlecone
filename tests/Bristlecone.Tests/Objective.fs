module ``Objective creation tests``

open System
open Expecto
open FsCheck
open Bristlecone
open Bristlecone.ModelSystem
open Bristlecone.EstimationEngine
open Bristlecone.Numerics

let dummySolver code returnValue len : Solver.ConfiguredSolver<'S,'V> =
    fun _ ->
        Map.ofList [ (code, Array.init len (fun i -> returnValue i |> Units.removeUnitFromFloat |> (*) 1.<state>) |> Config.defaultOfVector) ],
        Map.ofList [ (code, returnValue 0 |> Units.removeUnitFromFloat |> (*) 1.<state> |> Config.defaultOfScalar) ]

[<Tests>]
let initialBounds =
    testList
        "Objective"
        [

            testPropertyWithConfig Config.config "Time-series are paired to correct years" <| fun (NonEmptyArray (data: NormalFloat array)) pool ->
                let optimConfig = Parameter.Pool.toOptimiserConfigBounded Config.defaultNumEngine pool
                let code = ShortCode.create "x" |> Option.get
                let data' = [ (code, data |> Array.map (fun g -> g.Get * 1.<state>) |> Config.defaultOfVector) ] |> Map.ofList
                let point = Parameter.Pool.drawRandom (Random()) pool |> Parameter.Pool.toVectorWithKeysReal Config.defaultNumEngine |> snd |> optimConfig.Compiled.Inverse
                let pred =
                    Objective.createPredictor
                       ((Typed.Scalar.constants Config.defaultNumEngine).invalid |> Typed.Scalar.retype)
                        Map.empty
                        (dummySolver code (fun i -> float i * 1.2) data.Length)
                        optimConfig
                        point
                
                let prediction = Seq.init data.Length (fun i -> float i * 1.2)
                let paired = Objective.pairObservationsToExpected data' pred
                Expect.hasLength paired 1 "Map of expected data should only have one variable"

                let paired' = paired |> Map.find (ShortCode.create "x").Value
                Config.sequenceEqualTol
                    (paired'.Expected |> Typed.Vector.toArrayFloat Config.defaultNumEngine.scalarBackend |> Seq.map Units.removeUnitFromFloat)
                    prediction
                    "Prediction was not as expected after pairing"
                Config.sequenceEqualTol
                    (paired'.Observed |> Typed.Vector.toArrayFloat Config.defaultNumEngine.scalarBackend |> Seq.map Units.removeUnitFromFloat)
                    (data |> Array.map (fun g -> g.Get * 1.<state>))
                    "Observed vector was not as expected after pairing"


            testPropertyWithConfig Config.config "Likelihood functions use 'real' parameter values"
            <| fun shouldTransform (data: float list) (b1: NormalFloat) (b2: NormalFloat) ->
                
                if b1.Get = b2.Get || b1.Get = 0. || b2.Get = 0. then
                    ()
                else
                    let b1 = if b1.Get < 0. then b1.Get * -1. else b1.Get
                    let b2 = if b2.Get < 0. then b2.Get * -1. else b2.Get

                    let mode =
                        if shouldTransform then
                            Language.Positive
                        else
                            Language.NoConstraints

                    let X = Language.state "x"
                    let a = Language.parameter "a" mode (min b1 b2) (max b1 b2)

                    // Returns the parameter value
                    let fakeLikelihood (code:ShortCode.ShortCode) : ModelSystem.Likelihood<'S,'V,'u> =
                        { Evaluate = fun paramAccessor _ -> paramAccessor.Get "a" |> Typed.Scalar.retype
                          RequiredParameters = [ ModelLibrary.NegLogLikelihood.paramToAny a ]
                          RequiredCodes = [ State code ] }

                    let model =
                        Language.Model.empty
                        |> Language.Model.addRateEquation X (Language.P a)
                        |> Language.Model.estimateParameter a
                        |> Language.Model.useLikelihoodFunction (fakeLikelihood X.Code )
                        |> Language.Model.compile

                    let optimConfig = Parameter.Pool.toOptimiserConfigBounded Config.defaultNumEngine model.Parameters

                    let code = (ShortCode.create "x").Value
                    let testObjective =
                        Objective.create
                            Config.defaultNumEngine.scalarBackend
                            model.NegLogLikelihood.Evaluate
                            Map.empty
                            (dummySolver code (fun _ -> 2.0<state>) data.Length)
                            optimConfig
                            ((Typed.Scalar.constants Config.defaultNumEngine).invalid |> Typed.Scalar.retype)
                            ([ (code, data |> List.map ((*) 1.<state>) |> List.toArray |> Config.defaultOfVector) ] |> Map.ofList)

                    // The point given to the objective should be in optim-space.
                    let optimPoint =
                        [| min b1 b2 * 1.<``optim-space``> |] |> Config.defaultOfVector

                    Expect.floatClose
                        Accuracy.high
                        (testObjective optimPoint |> Typed.Scalar.toFloat |> Units.removeUnitFromFloat)
                        (min b1 b2)
                        "The likelihood function did not retrieve the 'real' parameter value"

            ]
