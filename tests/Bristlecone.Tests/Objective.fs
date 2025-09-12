module ``Objective creation tests``

open System
open Expecto
open FsCheck
open Bristlecone
open Bristlecone.ModelSystem
open Bristlecone.EstimationEngine
open Bristlecone.Tensors

let dummySolver returnValue len : Solver.ConfiguredSolver =
    fun _ ->
        [ (ShortCode.create "x").Value,
          Array.create len (returnValue |> Units.removeUnitFromFloat |> (*) 1.<state>) |> Typed.ofVector ]
        |> Map.ofList

[<Tests>]
let initialBounds =
    testList
        "Objective"
        [

            // testProperty "Time-series are paired to correct years" <| fun x ->
            //     false

            // testProperty "Throws when time-series are not the same length" <| fun x ->
            //     false

            testPropertyWithConfig Config.config "Likelihood functions use 'real' parameter values"
            <| fun shouldTransform (data: float list) (b1: NormalFloat) (b2: NormalFloat) ->

                // Returns the parameter value
                let fakeLikelihood: ModelSystem.Likelihood<'u> =
                    fun paramAccessor data ->
                        printfn "a = %A" (paramAccessor.Get "a")
                        paramAccessor.Get "a" |> Tensors.Typed.retype

                if b1.Get = b2.Get || b1.Get = 0. || b2.Get = 0. then
                    ()
                else
                    let b1 = if b1.Get < 0. then b1.Get * -1. else b1.Get
                    let b2 = if b2.Get < 0. then b2.Get * -1. else b2.Get

                    let mode =
                        if shouldTransform then
                            Language.notNegative
                        else
                            Language.noConstraints

                    let model =
                        Language.Model.empty
                        |> Language.Model.addEquation "x" (Language.Parameter "a")
                        |> Language.Model.estimateParameter "a" mode (min b1 b2) (max b1 b2)
                        |> Language.Model.useLikelihoodFunction fakeLikelihood
                        |> Language.Model.compile

                    let optimConfig = Parameter.Pool.DetachedConfig (Parameter.Pool.toOptimiserConfigBounded model.Parameters)

                    let testObjective =
                        Objective.create
                            model.NegLogLikelihood
                            Map.empty
                            (dummySolver 2.0<state> data.Length)
                            optimConfig
                            ([ (ShortCode.create "x").Value, data |> List.toArray ] |> Map.ofList)

                    // The point given to the objective should be in optim-space.
                    let optimPoint =
                        [| min b1 b2 * 1.<``optim-space``> |] |> Tensors.Typed.ofVector

                    Expect.floatClose
                        Accuracy.high
                        (testObjective optimPoint |> Tensors.Typed.toFloatScalar |> Units.removeUnitFromFloat)
                        b1
                        "The likelihood function did not retrieve the 'real' parameter value"

            ]
