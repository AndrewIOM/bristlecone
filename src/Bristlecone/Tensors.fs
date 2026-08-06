namespace Bristlecone

/// More shape-aware intermediary layer for
/// translating between DiffSharp tensors and
/// Bristlecone functions.
module Tensors =

    open DiffSharp.AD.Float64

    [<NoEquality; NoComparison>]
    type TypedScalar<[<Measure>] 'u> =
        private { Inner: D }

        static member inline (+)(a: TypedScalar<'u>, b: TypedScalar<'u>) : TypedScalar<'u> = { Inner = a.Inner + b.Inner }
        static member inline (-)(a: TypedScalar<'u>, b: TypedScalar<'u>) : TypedScalar<'u> = { Inner = a.Inner - b.Inner }
        static member inline (*)(a: TypedScalar<'u>, b: TypedScalar<'v>) : TypedScalar<'u * 'v> = { Inner = a.Inner * b.Inner }
        static member inline (/)(a: TypedScalar<'u>, b: TypedScalar<'v>) : TypedScalar<'u / 'v> = { Inner = a.Inner / b.Inner }

        static member inline (*)(k: float, a: TypedScalar<'u>) = { Inner = D k * a.Inner }
        static member inline (*)(a: TypedScalar<'u>, k: float) = { Inner = a.Inner * D k }


    [<NoEquality; NoComparison>]
    type TypedVector<[<Measure>] 'u> =
        private { Inner: DV }

        // Vector–Vector elementwise
        static member inline (+)(a: TypedVector< 'u>, b: TypedVector< 'u>) = { Inner = a.Inner + b.Inner }
        static member inline (-)(a: TypedVector< 'u>, b: TypedVector< 'u>) : TypedVector< 'u> = { Inner = a.Inner - b.Inner }
        static member inline (*)(a: TypedVector< 'u>, b: TypedVector< 'v>) : TypedVector< 'u * 'v> = { Inner = a.Inner * b.Inner }
        static member inline (/)(a: TypedVector< 'u>, b: TypedVector< 'v>) : TypedVector< 'u / 'v> = { Inner = a.Inner / b.Inner }

        // Vector–Scalar broadcast
        static member (*)(v: TypedVector< 'u>, s: TypedScalar<'v>) : TypedVector< 'u * 'v> =
            { Inner = v.Inner * s.Inner }

        static member (+)(v: TypedVector< 'u>, s: TypedScalar<'u>) : TypedVector< 'u> =
            { Inner = v.Inner + s.Inner }

        static member (*)(s: TypedScalar<'u>, v: TypedVector< 'v>) : TypedVector< 'u * 'v> =
            { Inner = s.Inner * v.Inner }

        static member (/)(v: TypedVector< 'u>, s: TypedScalar<'v>) : TypedVector< 'u / 'v> =
            { Inner = v.Inner / s.Inner }

        // Vector–float exponent
        static member ( ** )(v: TypedVector< 'u>, p: float) = { Inner = v.Inner ** p }



    [<NoEquality; NoComparison>]
    type TypedMatrix<[<Measure>] 'u> =
        private { Inner: DM }

    [<NoEquality; NoComparison>]
    type TypedBoolScalar =
        private { Inner: D }

        with
            member this.AsTensor () : TypedScalar<1> = { Inner = this.Inner }
            member this.AsBool () : bool = this.Inner |> float = 1.0
        

    module Typed =

        // Constructors
        let ofScalar (Inner: float<'u>) : TypedScalar<'u> =
            { Inner = D (float Inner) }

        let ofVector (data: float<'u>[]) : TypedVector< 'u> =
            if Array.isEmpty data then
                invalidArg "data" "Cannot create a TypedVector<_> from an empty array."

            { Inner = DV (data |> Array.map float) }

        let ofMatrix (data: float<'u>[,]) : TypedMatrix<'u> =
            { Inner = DM (data |> Array2D.map float) }


        module internal Constants =
            let invalidPenalty = ofScalar 1e8
            let penalty = ofScalar 1e6
            let absTol = ofScalar 1e-8
            let relTol = ofScalar 1e-6
            let nan = ofScalar nan

            let zero = ofScalar 0.
            let half = ofScalar 0.5
            let one = ofScalar 1.0
            let two = ofScalar 2.0


        let grad (fn: TypedScalar<'u> -> TypedScalar<'v>) (x:TypedScalar<'u>) : TypedScalar<'v / 'u> =
            let fnRaw (rawX: D) : D =
                let boxedX : TypedScalar<'u> = { Inner = rawX }
                let boxedY = fn boxedX
                boxedY.Inner            
            let result = grad fnRaw x.Inner
            { Inner = result }

        let addScalar (a: TypedScalar<'u>) (b: TypedScalar<'u>) : TypedScalar<'u> =
            { Inner = a.Inner + b.Inner }

        let minusScalar (a: TypedScalar<'u>) (b: TypedScalar<'u>) : TypedScalar<'u> =
            { Inner = a.Inner - b.Inner }

        let mulScalar (a: TypedScalar<'u>) (b: TypedScalar<'v>) : TypedScalar<'u * 'v> =
            { Inner = a.Inner * b.Inner }

        let divScalar (a: TypedScalar<'u>) (b: TypedScalar<'v>) : TypedScalar<'u / 'v> =
            { Inner = a.Inner / b.Inner }

        let isNan (scalar: TypedScalar<'u>) : TypedBoolScalar =
            let absDiff = D.Abs(scalar.Inner - scalar.Inner)
            { Inner = D.ReLU (D.Sign absDiff) }

        let isInf (scalar: TypedScalar<'u>) : TypedBoolScalar =
            let invAbs = divScalar Constants.one { Inner = D.Abs scalar.Inner}
            let isZero = minusScalar Constants.one { Inner = D.ReLU (D.Sign invAbs.Inner)}
            { Inner = isZero.Inner }

        let isFinite (x: TypedScalar<'u>) : TypedBoolScalar =
            let nanMask = isNan x
            let infMask = isInf x
            let badMask = D.Max(nanMask.AsTensor().Inner, infMask.AsTensor().Inner)
            { Inner = Constants.one.Inner - badMask }

        let gt (l: TypedScalar<'u>) (r: TypedScalar<'u>) : TypedBoolScalar =
            { Inner = D.ReLU (D.Sign(l.Inner - r.Inner)) }
            
        let lt (l: TypedScalar<'u>) (r: TypedScalar<'u>) : TypedBoolScalar =
            { Inner = D.ReLU (D.Sign(r.Inner - l.Inner)) }

        let eq (l: TypedScalar<'u>) (r: TypedScalar<'u>) =
            let absDiff = D.Abs(l.Inner - r.Inner)
            let diffMask = D.ReLU (D.Sign absDiff)
            { Inner = Constants.one.Inner - diffMask }

        let clamp (v:TypedScalar<'a>) (low: TypedScalar<'a>) (high: TypedScalar<'a>) : TypedScalar<'a> =
            { Inner = D.Min(high.Inner, D.Max(low.Inner, v.Inner))}

        let max (v:TypedScalar<'a>) (v2: TypedScalar<'a>) : TypedScalar<'a> =
            { Inner = D.Max(v.Inner, v2.Inner) }

        let min (v:TypedScalar<'a>) (v2: TypedScalar<'a>) : TypedScalar<'a> =
            { Inner = D.Min(v.Inner, v2.Inner) }

        /// TODO Check this works. Return type in DV.init?
        let linspace (start: TypedScalar<'u>) (stop: TypedScalar<'u>) (num: int) : TypedVector<'u> =
            if num <= 1 then failwith "Cannot space less than two points along a line"
            let steps = ofScalar (float (num - 1))
            let stepSize = (stop - start) / steps
            let rawVector = DV.init num (fun i -> 
                let idx = ofScalar (float i)
                start + idx * stepSize
            )
            
            { Inner = rawVector }

        let divVectorByScalar (v: TypedVector< 'u>) (s: TypedScalar<'v>) : TypedVector< 'u / 'v> =
            { Inner = v.Inner / s.Inner }

        let logScalar (a: TypedScalar<'u>) : TypedScalar<1> = { Inner = D.Log a.Inner }

        let logVector (a: TypedVector< 'u>) : TypedVector< 1> = { Inner = DV.Log a.Inner }

        let pow (a: TypedScalar<'u>) (p: TypedScalar<'u>) : TypedScalar<'u> = { Inner = D.Pow(a.Inner, p.Inner) }

        let exp (a: TypedScalar< 1>) : TypedScalar< 1> = { Inner = D.Exp a.Inner }
        let expVector (a: TypedVector< 1>) : TypedVector< 1> = { Inner = DV.Exp a.Inner }

        let floor (a: TypedScalar<'u>) : TypedScalar<'u> = { Inner = D.Floor a.Inner }

        let square (x: TypedScalar<'u>) : TypedScalar<'u^2> = { Inner = x.Inner ** 2.0 }

        let squareVector (x: TypedVector< 'u>) : TypedVector< 'u^2> = { Inner = x.Inner ** 2.0 }

        let sigmoid (s: TypedScalar<1>) : TypedScalar<1> = { Inner = D.Sigmoid s.Inner }

        /// Squared Euclidean length of a vector.
        let squaredLength (v: TypedVector< 'u>) : TypedScalar<'u^2> =
            { Inner = DV.L2NormSq v.Inner }

        let sqrtScalar (x: TypedScalar<'u^2>) : TypedScalar<'u> = { Inner = D.Sqrt x.Inner }

        let dot (a: TypedVector< 'u>) (b: TypedVector< 'u>) : TypedScalar<'u^2> =
            { Inner = a.Inner * b.Inner }

        let scale (s: TypedScalar<'a>) (v: TypedVector< 'b>) : TypedVector< 'a * 'b> =
            { Inner = s.Inner * v.Inner }

        let addVector (a: TypedVector< 'u>) (b: TypedVector< 'u>) : TypedVector< 'u> =
            { Inner = a.Inner + b.Inner }

        let subVector (a: TypedVector< 'u>) (b: TypedVector< 'u>) : TypedVector< 'u> =
            { Inner = a.Inner - b.Inner }

        let sumVector (a: TypedVector< 'u>) : TypedScalar<'u> = { Inner = DV.sum a.Inner }

        let signVector (v: TypedVector< 'u>) : TypedVector<1> = { Inner = DV.Sign v.Inner }

        let tail (v: TypedVector< 'u>) : TypedVector< 'u> =
            let len = v.Inner.Length

            if len < 2 then
                invalidArg "v" "Vector must have at least two elements to take tail."
            else
                { Inner = v.Inner.[1..] }

        let stack1D (items: TypedScalar<'u>[]) : TypedVector< 'u> =
            if Array.isEmpty items then
                invalidArg "items" "Cannot stack an empty array of scalars into a vector."

            let rawTensors = items |> Array.map (fun t -> t.Inner)
            { Inner = DV.ofArray rawTensors }

        let stack2D (items: TypedVector<'u> seq) : TypedMatrix<'u> =
            failwith "not implemented!"

        let unstack2D (matrix: TypedMatrix<'u>) : TypedVector<'u>[] =
            failwith "not implemented!"


        /// Prepend a scalar to the front of a vector, keeping it differentiable
        let prepend1D (head: TypedScalar<'u>) (tail: TypedVector< 'u>) : TypedVector< 'u> =
            let headVector = DV.ofArray [| head.Inner |]
            let concatenated = DV.concat [ headVector; tail.Inner ]
            { Inner = concatenated }

        let matMul (a: TypedMatrix<'u>) (b: TypedMatrix<'v>) : TypedMatrix<'u * 'v> =
            { Inner = a.Inner * b.Inner }

        /// Filter a vector tensor by a boolean mask.
        /// The mask length must match the vector length.
        let filterByMask (mask: bool[]) (v: TypedVector< 'u>) : TypedVector<'u> =
            if mask.Length <> v.Inner.Length then
                invalidArg "mask" (sprintf "Mask length mismatch (%i vs %i data length)" mask.Length v.Inner.Length)

            let selectedElements = 
                mask
                |> Array.mapi (fun i keep -> if keep then Some v.Inner.[i] else None)
                |> Array.choose id

            { Inner = DV.ofArray selectedElements }


        let toFloatScalar (t: TypedScalar<'u>) : float<'u> =
            float t.Inner |> LanguagePrimitives.FloatWithMeasure<'u>

        let toArray (v: TypedVector< 'u>) : TypedScalar<'u>[] =
            v.Inner.ToArray() |> Array.map(fun t -> { Inner = t })

        let toFloatArray (v: TypedVector< 'u>) : float<'u>[] =
            v.Inner.ToArray()
            |> Array.map (float >> LanguagePrimitives.FloatWithMeasure<'u>)

        let toFloatValueAt (i: int) (t: TypedVector< 'u>) =
            t.Inner.[i] |> float |> LanguagePrimitives.FloatWithMeasure<'u>

        let length (t: TypedVector< 'u>) = t.Inner.Length

        let itemAt (i: int) (v: TypedVector< 'u>) : TypedScalar<'u> = { Inner = v.Inner.[i] }

        /// Change the unit-of-measure phantom type of a TypedTensor without altering its Inner.
        /// This is purely a compile-time reinterpretation; the underlying DiffSharp tensor is unchanged.
        let retypeScalar<[<Measure>] 'u, [<Measure>] 'v> (t: TypedScalar<'u>) : TypedScalar<'v> =
            { Inner = t.Inner }

        let retypeVector<[<Measure>] 'u, [<Measure>] 'v> (t: TypedVector<'u>) : TypedVector<'v> =
            { Inner = t.Inner }

        // And scalar-to-vector broadcast
        let broadcastScalarToVector (s: TypedScalar<'u>) (len: int) : TypedVector< 'u> =
            let arr =
                Array.init len (fun _ -> LanguagePrimitives.FloatWithMeasure<'u>(float s.Inner))

            ofVector arr


    let (|VectorOfLen|_|) (len: int) (t: TypedVector<'u>) =
        match t.Inner.Length with
        | l when l = len -> Some t: TypedVector< 'u> option
        | _ -> None

    let (|MatrixOfShape|_|) (rows: int, cols: int) (t: TypedMatrix<'u>) =
        match t.Inner.Rows, t.Inner.Cols with
        | r, c when r = rows && c = cols -> Some t: TypedMatrix<'u> option
        | _ -> None
