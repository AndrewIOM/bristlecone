namespace Bristlecone

/// More shape-aware intermediary layer for
/// translating between DiffSharp tensors and
/// Bristlecone functions.
module Tensors =

    open DiffSharp

    // Phantom markers for shape categories only
    type Scalar = private Scalar of unit
    type Vector = private Vector of unit
    type Matrix = private Matrix of unit

    /// 'Shape = Scalar | Vector | Matrix
    /// 'U = unit of measure for element type (or float for no UoM)
    type TypedTensor<'Shape, [<Measure>] 'u> = private { Inner: Tensor }
    with
        member this.Value = this.Inner

    module Typed =

        // Constructors
        let ofScalar (value: float<'u>) : TypedTensor<Scalar, 'u> =
            { Inner = dsharp.scalar (float value, dtype = Dtype.Float64) }

        let ofVector (data: float<'u>[]) : TypedTensor<Vector, 'u> =
            { Inner = dsharp.tensor (data |> Array.map float, dtype = Dtype.Float64) }

        let ofMatrix (data: float<'u>[,]) : TypedTensor<Matrix, 'u> =
            { Inner = dsharp.tensor (data |> Array2D.map float, dtype = Dtype.Float64) }

        let addScalar (a: TypedTensor<Scalar, 'u>) (b: TypedTensor<Scalar, 'u>) =
            { Inner = a.Inner + b.Inner }

        let dot (a: TypedTensor<Vector, 'u>) (b: TypedTensor<Vector, 'u>) : TypedTensor<Scalar, 'u ^ 2> =
            { Inner = a.Inner.dot b.Inner }

        let matMul
            (a: TypedTensor<Matrix, 'u>)
            (b: TypedTensor<Matrix, 'v>)
            : TypedTensor<Matrix, 'u * 'v> =
            { Inner = a.Inner.matmul b.Inner }


    // ---------------------
    // Shape-category active patterns
    // ---------------------
    let (|Scalar|_|) (t: Tensor) =
        match t.shape with
        | [||] -> Some { Inner = t } : TypedTensor<Scalar,'u> option
        | _    -> None

    let (|Vector|_|) (t: Tensor) =
        match t.shape with
        | [| _ |] -> Some { Inner = t } : TypedTensor<Vector,'u> option
        | _       -> None

    let (|Matrix|_|) (t: Tensor) =
        match t.shape with
        | [| _; _ |] -> Some { Inner = t } : TypedTensor<Matrix,'u> option
        | _          -> None

    // ---------------------
    // Size-aware active patterns
    // ---------------------
    let (|VectorOfLen|_|) (len: int) (t: Tensor) =
        match t.shape with
        | [| n |] when n = len ->
            Some { Inner = t } : TypedTensor<Vector,'u> option
        | _ -> None

    let (|MatrixOfShape|_|) (rows: int, cols: int) (t: Tensor) =
        match t.shape with
        | [| r; c |] when r = rows && c = cols ->
            Some { Inner = t } : TypedTensor<Matrix,'u> option
        | _ -> None

    // Classification stays the same, but no size in the type
    type Untyped =
        | UScalar of Tensor
        | UVector of Tensor
        | UMatrix of Tensor
        | UOther  of Tensor

    let classify (t: Tensor) =
        match t.shape with
        | [||]         -> UScalar t
        | [| _ |]      -> UVector t
        | [| _; _ |]   -> UMatrix t
        | _            -> UOther t

    // Upgrade to typed form (unit of measure still applies)
    let tryAsScalar<[<Measure>] 'u> t =
        match classify t with
        | UScalar s -> Some { Inner = s } : TypedTensor<Scalar,'u> option
        | _ -> None

    let tryAsVector<[<Measure>] 'u> t =
        match classify t with
        | UVector v -> Some { Inner = v } : TypedTensor<Vector,'u> option
        | _ -> None

    let tryAsMatrix<[<Measure>] 'u> t =
        match classify t with
        | UMatrix m -> Some { Inner = m } : TypedTensor<Matrix,'u> option
        | _ -> None

    let asScalar<[<Measure>] 'u> t =
        match classify t with
        | UScalar s -> { Inner = s } : TypedTensor<Scalar,'u>
        | _ -> failwithf "%A is not a valid scalar" t

    // type ModelTimeIndexTensor = ModelTimeIndexTensor of Tensor
    //     with
    //         static member Create (v:float<``time index``>) = ModelTimeIndexTensor (dsharp.tensor v)
    //         member this.Value = this |> fun (ModelTimeIndexTensor v) -> v
    //         member this.Increment by = this.Value + by |> ModelTimeIndexTensor

    // type PointTensor = PointTensor of Tensor
    //     with member this.Value = this |> fun (PointTensor v) -> v

    // type ParameterPoolTensor = ParameterPoolTensor of Tensor
    //     with
    //         member this.Value = this |> fun (ParameterPoolTensor v) -> v
    //         member this.ValueFor (s:ShortCode.ShortCode) = dsharp.tensor 1

