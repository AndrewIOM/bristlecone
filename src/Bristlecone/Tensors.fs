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
    [<NoEquality; NoComparison>]
    type TypedTensor<'Shape, [<Measure>] 'u> =
        private
            { Inner: Tensor }

        member this.Value = this.Inner

        // Scalar–Scalar arithmetic
        static member (+)(a: TypedTensor<Scalar, 'u>, b: TypedTensor<Scalar, 'u>) : TypedTensor<Scalar, 'u> =
            { Inner = a.Value + b.Value }

        static member (-)(a: TypedTensor<Scalar, 'u>, b: TypedTensor<Scalar, 'u>) : TypedTensor<Scalar, 'u> =
            { Inner = a.Value - b.Value }

        static member (*)(a: TypedTensor<Scalar, 'u>, b: TypedTensor<Scalar, 'v>) : TypedTensor<Scalar, 'u * 'v> =
            { Inner = a.Value * b.Value }

        static member (/)(a: TypedTensor<Scalar, 'u>, b: TypedTensor<Scalar, 'v>) : TypedTensor<Scalar, 'u / 'v> =
            { Inner = a.Value / b.Value }

        // Allow float * scalar and scalar * float
        static member (*)(k: float, a: TypedTensor<Scalar, 'u>) = { Inner = dsharp.scalar k * a.Value }

        static member (*)(a: TypedTensor<Scalar, 'u>, k: float) = { Inner = a.Value * dsharp.scalar k }

        // Vector–Vector elementwise
        static member (+)(a: TypedTensor<Vector, 'u>, b: TypedTensor<Vector, 'u>) = { Inner = a.Value + b.Value }

        static member (-)(a: TypedTensor<Vector, 'u>, b: TypedTensor<Vector, 'u>) : TypedTensor<Vector, 'u> =
            { Inner = a.Value - b.Value }

        static member (*)(a: TypedTensor<Vector, 'u>, b: TypedTensor<Vector, 'v>) : TypedTensor<Vector, 'u * 'v> =
            { Inner = dsharp.mul (a.Value, b.Value) }

        static member (/)(a: TypedTensor<Vector, 'u>, b: TypedTensor<Vector, 'v>) : TypedTensor<Vector, 'u / 'v> =
            { Inner = a.Value / b.Value }

        // Vector–Scalar broadcast
        static member (*)(v: TypedTensor<Vector, 'u>, s: TypedTensor<Scalar, 'v>) : TypedTensor<Vector, 'u * 'v> =
            { Inner = v.Value * s.Value }

        static member (+)(v: TypedTensor<Vector, 'u>, s: TypedTensor<Scalar, 'u>) : TypedTensor<Vector, 'u> =
            { Inner = dsharp.add (v.Value, s.Value) }

        static member (*)(s: TypedTensor<Scalar, 'u>, v: TypedTensor<Vector, 'v>) : TypedTensor<Vector, 'u * 'v> =
            { Inner = s.Value * v.Value }

        static member (/)(v: TypedTensor<Vector, 'u>, s: TypedTensor<Scalar, 'v>) : TypedTensor<Vector, 'u / 'v> =
            { Inner = v.Value / s.Value }

        // Vector–float exponent
        static member ( ** )(v: TypedTensor<Vector, 'u>, p: float) = { Inner = v.Value ** p }



    module Typed =

        // Constructors
        let ofScalar (value: float<'u>) : TypedTensor<Scalar, 'u> =
            { Inner = dsharp.scalar (float value, dtype = Dtype.Float64) }

        let ofVector (data: float<'u>[]) : TypedTensor<Vector, 'u> =
            if Array.isEmpty data then
                invalidArg "data" "Cannot create a TypedTensor<Vector,_> from an empty array."

            { Inner = dsharp.tensor (data |> Array.map float, dtype = Dtype.Float64) }

        let ofMatrix (data: float<'u>[,]) : TypedTensor<Matrix, 'u> =
            { Inner = dsharp.tensor (data |> Array2D.map float, dtype = Dtype.Float64) }

        let addScalar (a: TypedTensor<Scalar, 'u>) (b: TypedTensor<Scalar, 'u>) : TypedTensor<Scalar, 'u> =
            { Inner = a.Inner + b.Inner }

        let minusScalar (a: TypedTensor<Scalar, 'u>) (b: TypedTensor<Scalar, 'u>) : TypedTensor<Scalar, 'u> =
            { Inner = a.Inner - b.Inner }

        let mulScalar (a: TypedTensor<Scalar, 'u>) (b: TypedTensor<Scalar, 'v>) : TypedTensor<Scalar, 'u * 'v> =
            { Inner = a.Value * b.Value }

        let divScalar (a: TypedTensor<Scalar, 'u>) (b: TypedTensor<Scalar, 'v>) : TypedTensor<Scalar, 'u / 'v> =
            { Inner = a.Value / b.Value }

        let divVectorByScalar (v: TypedTensor<Vector, 'u>) (s: TypedTensor<Scalar, 'v>) : TypedTensor<Vector, 'u / 'v> =
            { Inner = v.Value / s.Value }

        let logScalar (a: TypedTensor<Scalar, 'u>) : TypedTensor<Scalar, 1> = { Inner = a.Value.log () }

        let logVector (a: TypedTensor<Vector, 'u>) : TypedTensor<Vector, 1> = { Inner = a.Value.log () }

        let expVector (a: TypedTensor<Vector, 1>) : TypedTensor<Vector, 1> = { Inner = a.Value.exp () }

        let square (x: TypedTensor<Scalar, 'u>) : TypedTensor<Scalar, 'u^2> = { Inner = x.Value ** 2.0 }

        let squareVector (x: TypedTensor<Vector, 'u>) : TypedTensor<Vector, 'u^2> = { Inner = x.Value ** 2.0 }

        /// Squared Euclidean length of a vector.
        let squaredLength (v: TypedTensor<Vector, 'u>) : TypedTensor<Scalar, 'u^2> =
            let inner = v.Value
            let sq = dsharp.sum (dsharp.mul (inner, inner))
            { Inner = sq }

        let sqrtScalar (x: TypedTensor<Scalar, 'u^2>) : TypedTensor<Scalar, 'u> = { Inner = x.Value.sqrt () }

        let dot (a: TypedTensor<Vector, 'u>) (b: TypedTensor<Vector, 'u>) : TypedTensor<Scalar, 'u^2> =
            { Inner = a.Inner.dot b.Inner }

        let scale (s: TypedTensor<Scalar, 'a>) (v: TypedTensor<Vector, 'b>) : TypedTensor<Vector, 'a * 'b> =
            { Inner = s.Value * v.Value }

        let addVector (a: TypedTensor<Vector, 'u>) (b: TypedTensor<Vector, 'u>) : TypedTensor<Vector, 'u> =
            { Inner = a.Value + b.Value }

        let subVector (a: TypedTensor<Vector, 'u>) (b: TypedTensor<Vector, 'u>) : TypedTensor<Vector, 'u> =
            { Inner = a.Value - b.Value }

        let sumVector (a: TypedTensor<Vector, 'u>) : TypedTensor<Scalar, 'u> = { Inner = a.Value.sum () }

        let tail (v: TypedTensor<Vector, 'u>) : TypedTensor<Vector, 'u> =
            let len = v.Value.shape.[0]

            if len < 2 then
                invalidArg "v" "Vector must have at least two elements to take tail."
            else
                // Use range slicing to preserve the 1‑D shape
                { Inner = v.Value.[1..] }

        let stack1D (items: TypedTensor<Scalar, 'u>[]) : TypedTensor<Vector, 'u> =
            if Array.isEmpty items then
                invalidArg "items" "Cannot stack an empty array of scalars into a vector."

            let rawTensors = items |> Array.map (fun t -> t.Value)
            let stacked = dsharp.stack (rawTensors, dim = 0)
            { Inner = stacked }

        /// Prepend a scalar to the front of a vector, keeping it differentiable
        let prepend1D (head: TypedTensor<Scalar, 'u>) (tail: TypedTensor<Vector, 'u>) : TypedTensor<Vector, 'u> =
            let headTensor = head.Value.unsqueeze (0)
            let concatenated = dsharp.cat ([ headTensor; tail.Value ], dim = 0)
            { Inner = concatenated }

        let matMul (a: TypedTensor<Matrix, 'u>) (b: TypedTensor<Matrix, 'v>) : TypedTensor<Matrix, 'u * 'v> =
            { Inner = a.Inner.matmul b.Inner }

        /// Filter a vector tensor by a boolean mask.
        /// The mask length must match the vector length.
        let filterByMask (mask: bool[]) (v: TypedTensor<Vector, 'u>) =
            if v.Value.dim <> 1 then
                invalidArg "v" "Expected 1‑D vector"

            if mask.Length <> v.Value.shape.[0] then
                invalidArg "mask" (sprintf "Mask length mismatch (%i vs %i data length)" mask.Length v.Value.shape.[0])

            let idx =
                mask
                |> Array.mapi (fun i keep -> if keep then Some i else None)
                |> Array.choose id

            let selected = idx |> Array.map (fun i -> v.Value.[i]) |> dsharp.stack

            { Inner = selected }


        let toFloatScalar (t: TypedTensor<Scalar, 'u>) : float<'u> =
            float t.Value |> LanguagePrimitives.FloatWithMeasure<'u>

        let toFloatArray (v: TypedTensor<Vector, 'u>) : float<'u>[] =
            (v.Value.toArray () :?> float[])
            |> Array.map LanguagePrimitives.FloatWithMeasure<'u>

        let toFloatValueAt (i: int) (t: TypedTensor<Vector, 'u>) =
            t.Value.[i].toDouble () |> LanguagePrimitives.FloatWithMeasure<'u>

        let length (t: TypedTensor<Vector, 'u>) = t.Value.shape.[0]

        let itemAt (i: int) (v: TypedTensor<Vector, 'u>) : TypedTensor<Scalar, 'u> = { Inner = v.Value.[i] }

        /// Change the unit-of-measure phantom type of a TypedTensor without altering its value.
        /// This is purely a compile-time reinterpretation; the underlying DiffSharp tensor is unchanged.
        let retype<[<Measure>] 'u, [<Measure>] 'v, 'Shape> (t: TypedTensor<'Shape, 'u>) : TypedTensor<'Shape, 'v> =
            { Inner = t.Value }

        // And scalar-to-vector broadcast
        let broadcastScalarToVector (s: TypedTensor<Scalar, 'u>) (len: int) : TypedTensor<Vector, 'u> =
            let arr =
                Array.init len (fun _ -> LanguagePrimitives.FloatWithMeasure<'u>(float s.Value))

            ofVector arr

    // ---------------------
    // Shape-category active patterns
    // ---------------------
    let (|Scalar|_|) (t: Tensor) =
        match t.shape with
        | [||] -> Some { Inner = t }: TypedTensor<Scalar, 'u> option
        | _ -> None

    let (|Vector|_|) (t: Tensor) =
        match t.shape with
        | [| _ |] -> Some { Inner = t }: TypedTensor<Vector, 'u> option
        | _ -> None

    let (|Matrix|_|) (t: Tensor) =
        match t.shape with
        | [| _; _ |] -> Some { Inner = t }: TypedTensor<Matrix, 'u> option
        | _ -> None

    // ---------------------
    // Size-aware active patterns
    // ---------------------
    let (|VectorOfLen|_|) (len: int) (t: Tensor) =
        match t.shape with
        | [| n |] when n = len -> Some { Inner = t }: TypedTensor<Vector, 'u> option
        | _ -> None

    let (|MatrixOfShape|_|) (rows: int, cols: int) (t: Tensor) =
        match t.shape with
        | [| r; c |] when r = rows && c = cols -> Some { Inner = t }: TypedTensor<Matrix, 'u> option
        | _ -> None

    // Classification stays the same, but no size in the type
    type Untyped =
        | UScalar of Tensor
        | UVector of Tensor
        | UMatrix of Tensor
        | UOther of Tensor

    let classify (t: Tensor) =
        match t.shape with
        | [||] -> UScalar t
        | [| _ |] -> UVector t
        | [| _; _ |] -> UMatrix t
        | _ -> UOther t

    // Upgrade to typed form (unit of measure still applies)
    let tryAsScalar<[<Measure>] 'u> t =
        match classify t with
        | UScalar s -> Some { Inner = s }: TypedTensor<Scalar, 'u> option
        | _ -> None

    let tryAsVector<[<Measure>] 'u> t =
        match classify t with
        | UVector v -> Some { Inner = v }: TypedTensor<Vector, 'u> option
        | _ -> None

    let tryAsMatrix<[<Measure>] 'u> t =
        match classify t with
        | UMatrix m -> Some { Inner = m }: TypedTensor<Matrix, 'u> option
        | _ -> None

    let asScalar<[<Measure>] 'u> t =
        match classify t with
        | UScalar s -> { Inner = s }: TypedTensor<Scalar, 'u>
        | _ -> failwithf "%A is not a valid scalar" t

    let allocateTensor value =
        dsharp.tensor (value, dtype = Dtype.Float64)

    /// Operators that rely on the 0/1 encoding of booleans
    /// in DiffSharp for working with raw tensors.
    module Unsafe =
        let logicalNot (b: Tensor) = dsharp.eq (b, dsharp.zerosLike b)

        let logicalOr (a: Tensor) (b: Tensor) =
            dsharp.gt (dsharp.add (a, b), dsharp.zerosLike a)

        let logicalAnd (a: Tensor) (b: Tensor) =
            dsharp.eq (dsharp.add (a, b), dsharp.onesLike a)
