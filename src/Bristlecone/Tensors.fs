namespace Bristlecone.Numerics

type Constants<'T> = {
    invalid: 'T
    penalty: 'T
    absTol: 'T
    relTol: 'T
    nan: 'T
    zero: 'T
    half: 'T
    one: 'T
    two: 'T
}

/// Represents mathematical operations on
/// atomic (scalar) values.
type AtomicBackend<'S> = {
    addS       : 'S -> 'S -> 'S
    subS       : 'S -> 'S -> 'S
    mulS       : 'S -> 'S -> 'S
    divS       : 'S -> 'S -> 'S
    fromFloatS : float -> 'S
    constants  : Constants<'S>
}

/// A unified container for numeric functions,
/// where scalar can be a degenerate case of
/// a container with only one element.
type NumericBackend<'S,'V> = {
    add      : 'V -> 'V -> 'V
    sub      : 'V -> 'V -> 'V
    mul      : 'V -> 'V -> 'V
    div      : 'V -> 'V -> 'V

    scaleVS   : 'V -> 'S -> 'V
    scaleSV   : 'S -> 'V -> 'V

    ofScalar  : 'S -> 'V
    reduce  : ('S -> 'S -> 'S) -> 'V -> 'S
    length  : 'V -> int
    promote : 'S -> int -> 'V

    min : 'V -> 'V -> 'V
    max : 'V -> 'V -> 'V
    abs : 'V -> 'V
    exp : 'V -> 'V
    log : 'V -> 'V
    pow : 'V -> 'V -> 'V
    sqrt : 'V -> 'V
    floor : 'V -> 'V
    gt : 'V -> 'V -> 'V
    lt : 'V -> 'V -> 'V
    eq : 'V -> 'V -> 'V
    isNan : 'V -> 'V
    isInf : 'V -> 'V

    initV     : int -> (int -> 'S) -> 'V
    atomic:   AtomicBackend<'S>
}


/// Typed representations of scalars, vectors, and matricies,
/// which support units of measure and different numeric engines (backends).
module Typed =

    [<NoEquality; NoComparison; StructuredFormatDisplay("{Inner}")>]
    type TypedScalar<'S,[<Measure>] 'u> =
        private {
            Inner: 'S
            Backend : NumericBackend<'S,'S> }

        member internal this.Backend = this.Backend

        static member inline (+)(a: TypedScalar<'S,'u>, b: TypedScalar<'S,'u>) : TypedScalar<'S,'u> = { Inner = a.Backend.add a.Inner b.Inner; Backend = a.Backend }
        static member inline (-)(a: TypedScalar<'S,'u>, b: TypedScalar<'S,'u>) : TypedScalar<'S,'u> = { Inner = a.Backend.sub a.Inner b.Inner; Backend = a.Backend }
        static member inline (*)(a: TypedScalar<'S,'u>, b: TypedScalar<'S,'v>) : TypedScalar<'S,'u * 'v> = { Inner = a.Backend.mul a.Inner b.Inner; Backend = a.Backend }
        static member inline (/)(a: TypedScalar<'S,'u>, b: TypedScalar<'S,'v>) : TypedScalar<'S,'u / 'v> = { Inner = a.Backend.div a.Inner b.Inner; Backend = a.Backend }

        static member inline (*)(k: float, a: TypedScalar<'S,'u>) = { Inner = a.Backend.mul a.Inner (a.Backend.ofFloat k); Backend = a.Backend }
        static member inline (*)(a: TypedScalar<'S,'u>, k: float) = { Inner = a.Backend.mul a.Inner (a.Backend.ofFloat k); Backend = a.Backend }


    [<NoEquality; NoComparison>]
    type TypedBoolScalar<'B> =
        private { Inner: 'B }

        with
            member this.AsTensor () : TypedScalar<'S,1> = { Inner = this.Inner }
            member this.AsBool () : bool = this.Inner |> float = 1.0


    [<NoEquality; NoComparison; StructuredFormatDisplay("{Inner}")>]
    type TypedVector<'S,'V,[<Measure>] 'u> =
        private {
            Inner: 'V
            Backend: NumericBackend<'S,'V> }

        // Vector–Vector elementwise
        static member inline (+)(a: TypedVector<'S,'V,'u>, b: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,'u> = { Inner = a.Backend.add a.Inner b.Inner; Backend = a.Backend }
        static member inline (-)(a: TypedVector<'S,'V,'u>, b: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,'u> = { Inner = a.Backend.sub a.Inner b.Inner; Backend = a.Backend }
        static member inline (*)(a: TypedVector<'S,'V,'u>, b: TypedVector<'S,'V,'v>) : TypedVector<'S,'V,'u * 'v> = { Inner = a.Backend.mul a.Inner b.Inner; Backend = a.Backend }
        static member inline (/)(a: TypedVector<'S,'V,'u>, b: TypedVector<'S,'V,'v>) : TypedVector<'S,'V,'u / 'v> = { Inner = a.Backend.div a.Inner b.Inner; Backend = a.Backend }

        // Vector–Scalar broadcast
        static member (*)(v: TypedVector<'S,'V,'u>, s: TypedScalar<'S,'v>) : TypedVector<'S,'V,'u * 'v> =
            { Inner = v.Backend.scaleVS v.Inner s.Inner
              Backend = v.Backend }

        static member (*)(s: TypedScalar<'S,'u>, v: TypedVector<'S,'V,'v>) : TypedVector<'S,'V,'u * 'v> =
            { Inner = v.Backend.scaleSV s.Inner v.Inner
              Backend = v.Backend }

        static member (+)(v: TypedVector<'S,'V,'u>, s: TypedScalar<'S,'u>) : TypedVector<'S,'V,'u> =
            { Inner = v.Backend.add v.Inner (v.Backend.ofScalar s.Inner)
              Backend = v.Backend }

        static member (/)(v: TypedVector<'S,'V,'u>, s: TypedScalar<'S,'v>) : TypedVector<'S,'V,'u / 'v> =
            { Inner = v.Backend.scaleVS v.Inner s.Inner
              Backend = v.Backend }

        // Vector–float exponent
        static member ( ** )(v: TypedVector<'S,'V,'u>, p: float) : TypedVector<'S,'V,1> =
            { Inner = v.Backend.mul v.Inner (v.Backend.ofScalar (v.Backend.atomic.fromFloatS p))
              Backend = v.Backend }

    let ofScalar backend (Inner: float<'u>) : TypedScalar<'S,'u> =
        { Inner = backend.atomic.fromFloatS (float Inner); Backend = backend }

    let ofVector backend (data: float<'u>[]) : TypedVector<'S,'V,'u> =
        if Array.isEmpty data then
            invalidArg "data" "Cannot create a TypedVector<_> from an empty array."

        { Inner = backend.ofFloatArray (data |> Array.map float); Backend = backend }

    let ofMatrix (data: float<'u>[,]) : TypedMatrix<'u> =
        { Inner = DM (data |> Array2D.map float) }


    /// Functions for working with typed scalars.
    module Scalar =

        let broadcast (s: TypedScalar<'S,'u>) (len: int) : TypedVector<'S,'V,'u> =
            s.Backend.promote s.Inner len
            |> ofVector s.Backend

        let clamp (s:TypedScalar<'S,'a>) (low: TypedScalar<'S,'a>) (high: TypedScalar<'S,'a>) : TypedScalar<'S,'a> =
            let clamped = s.Backend.min high.Inner (s.Backend.max low.Inner s.Inner)
            { s with Inner = clamped }

        let exp (a: TypedScalar<'S,1>) : TypedScalar<'S,1> = { a with Inner = a.Backend.exp a.Inner }

        let isNan (s: TypedScalar<'S,'u>) : TypedBoolScalar<'S> =
            let absDiff = s.Backend.abs (s.Inner - s.Inner)
            { Inner = s.Backend.reLU (a.Backend.sign absDiff) }

        let isInf (scalar: TypedScalar<'S,'u>) : TypedBoolScalar<'B> =
            let absThis = scalar.Backend.abs scalar
            let invAbs = scalar.Backend.div Constants.one absThis
            let isZero = scalar.Backend.sub Constants.one { Inner = D.ReLU (D.Sign invAbs.Inner)}
            { Inner = isZero.Inner }

        let isFinite (x: TypedScalar<'S,'u>) : TypedBoolScalar<'B> =
            let nanMask = isNan x
            let infMask = isInf x
            let badMask = x.Backend.max (nanMask.AsTensor().Inner, infMask.AsTensor().Inner)
            { Inner = Constants.one.Inner - badMask }

        let floor (s: TypedScalar<'S,'u>) : TypedScalar<'S,'u> = { s with Inner = s.Backend.floor s.Inner }

        let log (s: TypedScalar<'S,'u>) : TypedScalar<'S,1> = { s with Inner = s.Backend.log s.Inner }

        let max (v:TypedScalar<'a>) (v2: TypedScalar<'a>) : TypedScalar<'a> =
            { Inner = D.Max(v.Inner, v2.Inner) }

        let min (v:TypedScalar<'a>) (v2: TypedScalar<'a>) : TypedScalar<'a> =
            { Inner = D.Min(v.Inner, v2.Inner) }

        let pow (a: TypedScalar<'u>) (p: TypedScalar<'u>) : TypedScalar<'u> = { Inner = D.Pow(a.Inner, p.Inner) }

        let toFloat (t: TypedScalar<'u>) : float<'u> =
            float t.Inner |> LanguagePrimitives.FloatWithMeasure<'u>

        let gt (l: TypedScalar<'S,'u>) (r: TypedScalar<'S,'u>) : TypedBoolScalar<'B> =
            let r = l.Backend.max l.Backend.atomic.constants.zero (l.Backend.sign)
            { Inner = l.Backend.max l.Backend.atomic.constants.zero D.ReLU (D.Sign(l.Inner - r.Inner)) }
            
        let lt (l: TypedScalar<'u>) (r: TypedScalar<'u>) : TypedBoolScalar =
            { Inner = D.ReLU (D.Sign(r.Inner - l.Inner)) }

        let eq (l: TypedScalar<'u>) (r: TypedScalar<'u>) =
            let absDiff = D.Abs(l.Inner - r.Inner)
            let diffMask = D.ReLU (D.Sign absDiff)
            { Inner = Constants.one.Inner - diffMask }

        let sign (s:TypedScalar<'S, 'u>) =
            let zero = s.Backend.ofScalar s.Backend.atomic.constants.zero
            let result = s.Backend.sub (s.Backend.gt s.Inner zero) (s.Backend.lt s.Inner zero)
            { s with Inner = result }

        let grad (fn: TypedScalar<'S, 'u> -> TypedScalar<'v>) (x:TypedScalar<'u>) : TypedScalar<'v / 'u> =
            let fnRaw (rawX: D) : D =
                let boxedX : TypedScalar<'u> = { Inner = rawX }
                let boxedY = fn boxedX
                boxedY.Inner            
            let result = grad fnRaw x.Inner
            { Inner = result }

        let sqrt (x: TypedScalar<'u^2>) : TypedScalar<'u> = { Inner = D.Sqrt x.Inner }


    module Vector =

        let length (t: TypedVector<_,_,'u>) = t.Backend.length t.Inner

        let itemAtFloat (i: int) (t: TypedVector<_,_,'u>) =
            t.Inner.[i] |> float |> LanguagePrimitives.FloatWithMeasure<'u>

        let itemAt (i: int) (v: TypedVector<'S,_,'u>) : TypedScalar<'S,'u> =
            { Inner = v.Inner.[i] }

        /// Prepend a scalar to the front of a vector.
        let prepend (head: TypedScalar<'S,'u>) (tail: TypedVector<'S,_,'u>) : TypedVector<'S,_,'u> =
            let headVector = DV.ofArray [| head.Inner |]
            let concatenated = DV.concat [ headVector; tail.Inner ]
            { Inner = concatenated }

        let toArray (v: TypedVector<_,_,'u>) : TypedScalar<'u>[] =
            v.Inner.ToArray() |> Array.map(fun t -> { Inner = t })

        let toArrayFloat (v: TypedVector<_,_,'u>) : float<'u>[] =
            v.Inner.ToArray()
            |> Array.map (float >> LanguagePrimitives.FloatWithMeasure<'u>)

        /// Filter a vector tensor by a boolean mask.
        /// The mask length must match the vector length.
        let filterByMask (mask: bool[]) (v: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,'u> =
            if mask.Length <> length v then
                invalidArg "mask" (sprintf "Mask length mismatch (%i vs %i data length)" mask.Length (length v))

            let selectedElements = 
                mask
                |> Array.mapi (fun i keep -> if keep then Some (itemAt i v) else None)
                |> Array.choose id

            { Inner = DV.ofArray selectedElements }


        let dot (a: TypedVector<'S,'V,'u>) (b: TypedVector<'S,'V,'u>) : TypedScalar<'S,'u^2> =
            { Inner = a.Inner * b.Inner }

        let scale (s: TypedScalar<'a>) (v: TypedVector< 'b>) : TypedVector< 'a * 'b> =
            { Inner = s.Inner * v.Inner }

        let sum (a: TypedVector<'S,'V,'u>) : TypedScalar<'S,'u> = { Inner = DV.sum a.Inner }

        let sign (v: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,1> = { Inner = DV.Sign v.Inner }

        let tail (v: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,'u> =
            let len = length v

            if len < 2 then
                invalidArg "v" "Vector must have at least two elements to take tail."
            else
                { Inner = v.Inner.[1..] }

        let divVectorByScalar (v: TypedVector<'S,'V,'u>) (s: TypedScalar<'S,'v>) : TypedVector<'S,'V,'u / 'v> =
            let x = v.Backend.scaleVS
            
            { Inner = v.Inner / s.Inner }

        let logVector (a: TypedVector< 'u>) : TypedVector< 1> = { Inner = DV.Log a.Inner }
        let expVector (a: TypedVector< 1>) : TypedVector< 1> = { Inner = DV.Exp a.Inner }



    [<NoEquality; NoComparison>]
    type TypedMatrix<'M, [<Measure>] 'u> =
        private { Inner: 'M }

    module Matrix =

        let matMul (a: TypedMatrix<'u>) (b: TypedMatrix<'v>) : TypedMatrix<'u * 'v> =
            { Inner = a.Inner * b.Inner }
    

module Stats =

    open Typed

    let mean (b : NumericBackend<'S,'V>) (values : 'V) : 'S =
        let total = b.reduce b.atomic.addS values
        let len   = b.length values |> float |> b.atomic.fromFloatS
        b.atomic.divS total len

    let linspace
        (backendV : NumericBackend<'S,'V>)
        (start: TypedScalar<'S, 'u>) (stop: TypedScalar<'S, 'u>) (num: int) : TypedVector<'S,'V,'u> =
        if num <= 1 then failwith "Cannot space less than two points along a line"
        let steps = ofScalar start.Backend (float (num - 1))
        let stepSize = (stop - start) / steps
        let rawVector = backendV.initV num (fun i -> 
            let idx = ofScalar start.Backend (float i)
            start + idx * stepSize
        )
        ofVector backendV rawVector

    let stack1D (items: TypedScalar<'u>[]) : TypedVector< 'u> =
        if Array.isEmpty items then
            invalidArg "items" "Cannot stack an empty array of scalars into a vector."

        let rawTensors = items |> Array.map (fun t -> t.Inner)
        { Inner = DV.ofArray rawTensors }

    let stack2D (items: TypedVector<'u> seq) : TypedMatrix<'u> =
        failwith "not implemented!"

    let unstack2D (matrix: TypedMatrix<'u>) : TypedVector<'u>[] =
        failwith "not implemented!"

    let square (x: TypedScalar<'u>) : TypedScalar<'u^2> = { Inner = x.Inner ** 2.0 }

    /// Squared Euclidean length of a vector.
    let squaredLength (v: TypedVector< 'u>) : TypedScalar<'u^2> =
        { Inner = DV.L2NormSq v.Inner }

    let squareVector (x: TypedVector< 'u>) : TypedVector< 'u^2> = { Inner = x.Inner ** 2.0 }

    let sigmoid (s: TypedScalar<1>) : TypedScalar<1> = { Inner = D.Sigmoid s.Inner }

    let inline call f (x:^T) =
        f (^T : (member Backend : NumericBackend<'S,'V>) x)
            (^T : (member Inner : 'V) x)



module Conversions =

    /// Change the unit-of-measure phantom type of a TypedTensor without altering its Inner.
    /// This is purely a compile-time reinterpretation; the underlying DiffSharp tensor is unchanged.
    let retypeScalar<[<Measure>] 'u, [<Measure>] 'v> (t: Typed.TypedScalar<_,'u>) : Typed.TypedScalar<_,'v> =
        { Inner = t.Inner }

    let retypeVector<[<Measure>] 'u, [<Measure>] 'v> (t: TypedVector<_,_,'u>) : TypedVector<_,_,'v> =
        { Inner = t.Inner }


module ActivePatterns =

    open Typed

    let (|VectorOfLen|_|) (len: int) (t: TypedVector<'S,'V,'u>) =
        match Vector.length t with
        | l when l = len -> Some t: TypedVector<'S,'V,'u> option
        | _ -> None

    let (|MatrixOfShape|_|) (rows: int, cols: int) (t: TypedMatrix<'M,'u>) =
        match t.Inner.Rows, t.Inner.Cols with
        | r, c when r = rows && c = cols -> Some t: TypedMatrix<'M,'u> option
        | _ -> None


module Backends =

    module DiffSharp =

        open DiffSharp.AD.Float64

        let constants = {
            invalid = D 1e8
            penalty = D 1e6
            absTol = D 1e-8
            relTol = D 1e-6
            nan = D nan
            zero = D 0.
            half = D 0.5
            one = D 1.0
            two = D 2.0
        }

        let scalar : NumericBackend<D,D> =
            { 
                add    = fun x y -> x + y
                sub    = fun x y -> x - y
                mul    = fun x y -> x * y
                div = failwith "Not Implemented"
                scaleVS = failwith "Not Implemented"
                scaleSV = failwith "Not Implemented"
                ofScalar = failwith "Not Implemented"
                min = failwith "Not Implemented"
                max = failwith "Not Implemented"
                reduce  = fun _ x -> x
                length  = fun _ -> 1
                promote = fun s _ -> s
                initV = failwith "Not Implemented"
                atomic = {
                    addS = fun x y -> x + y
                    subS = fun x y -> x + y
                    mulS = fun x y -> x * y
                    divS = fun x y -> x / y
                    fromFloatS = D
                    constants = constants
                }
            }

        let vector : NumericBackend<D,DV> = {
                add    = fun x y -> x + y
                sub    = fun x y -> x - y
                mul    = fun x y -> x * y
                div = fun x y -> x / y
                scaleVS = fun x y -> x * y
                scaleSV = fun x y -> x * y
                ofScalar = failwith "Not Implemented"
                min = fun x y -> DV.Min(x,y)
                max = fun x y -> DV.Max(x,y)
                reduce  = fun _ x -> x
                length  = fun _ -> 1
                promote = fun s _ -> s
                initV = failwith "Not Implemented"
                atomic = {
                    addS = fun x y -> x + y
                    subS = fun x y -> x + y
                    mulS = fun x y -> x * y
                    divS = fun x y -> x / y
                    fromFloatS = D
                    constants = constants
                }
        }

module Tests =

    open Stats

    let typedT = Typed.ofScalar Backends.DiffSharp.scalar 1.2
    let x = call mean typedT
