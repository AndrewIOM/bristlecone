module IntegrationTests

open System
open Expecto
open Expecto.ExpectoFsCheck
open FsCheck
open DiffSharp
open Bristlecone
open Bristlecone.Time
open Bristlecone.Integration.RungeKutta
open Bristlecone.Tensors
open Bristlecone.ModelSystem

dsharp.config(dtype = Dtype.Float64)

let epsilon = 1e-10

[<Tests>]
let rk4Tests =
    testList "Runge-Kutta 4 trajectory tests" [

        test "Tensor and float RK4 trajectories match at every step" {
            // Simple ODE: dy/dt = -y
            let odeTensor (t: Tensor) (y: Tensor) = -y
            let odeFloat (t: float) (y: float[]) = [| -y.[0] |]

            let steps = 10
            let tInitial = 0.<``time index``>
            let tEnd     = 1.<``time index``>
            let tStep    = (tEnd - tInitial) / float steps
            let y0       = [| 1.0 |]
            printfn "Starting 2..."
            // Tensor-based integration
            let tensorTrajectory : float[,] =
                rk4WithStepCount
                    (Tensors.Typed.ofScalar tInitial)
                    (Tensors.Typed.ofScalar tEnd)
                    steps
                    (Tensors.Typed.ofVector y0).Value
                    odeTensor
                |> fun t -> t.toArray2D()
            
            // Float-based integration
            let floatTrajectory =
                rk4float tInitial tEnd tStep y0 odeFloat

            // Compare every step and every variable
            Expect.equal (Array2D.length1 tensorTrajectory) floatTrajectory.Length "Step count mismatch"

            for i in 0 .. floatTrajectory.Length - 1 do
                for j in 0 .. floatTrajectory.[i].Length - 1 do
                    let a = tensorTrajectory.[i,j]
                    let b = floatTrajectory.[i].[j]
                    if abs (a - b) > epsilon then
                        failwithf "Mismatch at step %d, var %d: tensor=%.12f float=%.12f" i j a b
        }

        test "RK4 matches known exp(-t) solution" {
            // dy/dt = -y, y(0) = 1 → y(t) = exp(-t)
            let odeTensor (t: Tensor) (y: Tensor) = -y

            let steps = 20
            let tInitial = 0.0<``time index``>
            let tEnd     = 1.0<``time index``>
            let y0       = [| 1.0 |]

            let trajectory : float[,] =
                rk4WithStepCount
                    (Tensors.Typed.ofScalar tInitial)
                    (Tensors.Typed.ofScalar tEnd)
                    steps
                    (Tensors.Typed.ofVector y0).Value
                    odeTensor
                |> fun t -> t.toArray2D()

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
            let t0 = Typed.ofScalar 0.0<``time index``>
            let t1 = Typed.ofScalar 1.0<``time index``>
            let dt = Typed.ofScalar 0.5<``time index``>
            let t0Map = Map.ofList [ key, Typed.ofScalar 1.0<state> ]

            // RHS: dy/dt = constant 2.0
            let rhs : EstimationEngine.ParameterisedRHS =
                fun _t _yVec -> Map.ofList [ key, Typed.ofScalar 2.0<state> ]

            let keys, y0 = flattenState t0Map
            let traj = rk4WithStepWidth t0 t1 dt y0 (wrapRhs keys rhs)
            let series = unflattenTrajectory keys traj

            let xs = series.[key] |> Typed.toFloatArray
            Expect.floatClose Accuracy.high (xs.[xs.Length - 1] |> Units.removeUnitFromFloat) (1.0 + 2.0*1.0) "Final value mismatch"

    ]