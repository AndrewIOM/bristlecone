module IntegrationTests

open System
open Expecto
open Expecto.ExpectoFsCheck
open Bristlecone
open Bristlecone.Time
open Bristlecone.Integration.RungeKutta
open Bristlecone.Numerics.Typed
open Bristlecone.ModelSystem

let epsilon = 1e-10

[<Tests>]
let rk4Tests =
    testList "Runge-Kutta 4 trajectory tests" [

        test "RK4 matches known exp(-t) solution" {
            // dy/dt = -y, y(0) = 1 → y(t) = exp(-t)
            let odeTensor (t: FsScalar<'S,Time.``time index``>) (y: FsScalar<'S,'u>) : FsScalar<'S,/Time.``time index``> = -y

            let steps = 20
            let tInitial = 0.0<``time index``> |> Config.defaultOfScalar
            let tEnd = 1.0<``time index``> |> Config.defaultOfScalar
            let y0 = [| 1.0</``time index``> |] |> Config.defaultOfVector

            let trajectory : FsMatrix<'S,'a,'s> =
                rk4WithStepCount
                    Config.defaultNumEngine
                    tInitial
                    tEnd
                    steps
                    y0
                    odeTensor

            let finalVal = trajectory.[steps, 0]
            let expected = System.Math.Exp(-1.0)
            Expect.floatClose Accuracy.high expected finalVal "Final RK4 value should be close to e^-1"
        }
    ]

[<Tests>]
let rk4EdgeCases =
    testList "RK4 edge cases" [

        testCase "RK4 handles single-state, single-length vector without collapsing to scalar" <| fun _ ->
            let key = ShortCode.create "x" |> Option.get
            let t0 = Config.defaultOfScalar 0.0<``time index``>
            let t1 = Config.defaultOfScalar 1.0<``time index``>
            let dt = Config.defaultOfScalar 0.5<``time index``>
            let t0Map = Map.ofList [ (key, Config.defaultOfScalar 1.0<state>) ]

            // RHS: dy/dt = constant 2.0
            let rhs : EstimationEngine.ParameterisedRHS<'S,'V> =
                fun _t _yVec -> Map.ofList [ (key, Config.defaultOfScalar 2.0<state/``time index``>) ]

            let keys, y0 = flattenState Config.defaultNumEngine t0Map
            let traj = rk4WithStepWidth Config.defaultNumEngine t0 t1 dt y0 (wrapRhs Config.defaultNumEngine keys rhs)
            let series = unflattenTrajectory keys traj

            let xs = series.[key] |> Vector.toArrayFloat
            Expect.floatClose Accuracy.high (xs.[xs.Length - 1] |> Units.removeUnitFromFloat) (1.0 + 2.0*1.0) "Final value mismatch"

    ]