namespace Bristlecone.Numerics

type Constants<'S> = {
    invalid: 'S
    penalty: 'S
    absTol: 'S
    relTol: 'S
    nan: 'S
    zero: 'S
    half: 'S
    one: 'S
    two: 'S
    six: 'S
    pi: 'S
}

/// Represents mathematical operations on
/// atomic (scalar) values.
type AtomicBackend<'S> = {
    addS       : 'S -> 'S -> 'S
    subS       : 'S -> 'S -> 'S
    mulS       : 'S -> 'S -> 'S
    divS       : 'S -> 'S -> 'S
    fromFloatS : float -> 'S
    toFloatS   : 'S -> float
    constants  : Constants<'S>
}

/// A unified container for numeric functions,
/// where scalar can be a degenerate case of
/// a container with only one element.
type NumericBackend<'S,'C> = {
    add      : 'C -> 'C -> 'C
    sub      : 'C -> 'C -> 'C
    mul      : 'C -> 'C -> 'C
    div      : 'C -> 'C -> 'C

    scaleVS   : 'C -> 'S -> 'C
    scaleSV   : 'S -> 'C -> 'C

    ofScalar  : 'S -> 'C
    reduce  : ('S -> 'S -> 'S) -> 'C -> 'S
    length  : 'C -> int
    promote : 'S -> int -> 'C

    min : 'C -> 'C -> 'C
    max : 'C -> 'C -> 'C
    abs : 'C -> 'C
    exp : 'C -> 'C
    log : 'C -> 'C
    pow : 'C -> 'S -> 'C
    sqrt : 'C -> 'C
    floor : 'C -> 'C
    gt : 'C -> 'C -> 'C
    lt : 'C -> 'C -> 'C
    eq : 'C -> 'C -> 'C
    isNan : 'C -> 'C
    isInf : 'C -> 'C

    sin: 'C -> 'C

    sigmoid : 'C -> 'C

    initV     : int -> (int -> 'S) -> 'C
    atomic:   AtomicBackend<'S>
}

/// Vector-specific access ops
type VectorAccessOps<'S,'V> = {
    toArray : 'V -> 'S array
    ofArray : 'S array -> 'V
    length  : 'V -> int
    get     : 'V -> int -> 'S
    concat : 'V -> 'V -> 'V
    tail : 'V -> 'V
}

/// Matrix-specific structural access ops
type MatrixAccessOps<'V,'M> = {
    toArray2d : 'M -> 'V array2d
    ofArray2d : 'V array2d -> 'M
    nrows     : 'M -> int
    ncols     : 'M -> int
    get       : 'M -> int -> int -> 'V
    transpose : 'M -> 'M
}

/// Represents operations in a numerical system,
/// where there are three numerical types: scalar,
/// vector, and matrix.
type NumericEngine<'S,'V,'M> = {
    scalarBackend : NumericBackend<'S,'S>
    vectorBackend : NumericBackend<'S,'V>
    matrixBackend : NumericBackend<'S,'M>
    vectorAccess  : VectorAccessOps<'S,'V>
    matrixAccess  : MatrixAccessOps<'S,'M>
}

type AccessOps<'S,'C,'R> =
    | ScalarAccess of unit
    | VectorAccess of VectorAccessOps<'S,'C>
    | MatrixAccess of MatrixAccessOps<'S,'C>

[<Struct; NoEquality; NoComparison; StructuredFormatDisplay("{Inner}")>]
type Numeric<'S,'C,'R,[<Measure>] 'u> =
    {
        Inner     : 'C
        Backend   : NumericBackend<'S,'C>
        Access    : AccessOps<'S,'C,'R>
    }


/// Typed representations of scalars, vectors, and matricies,
/// which support units of measure and different numeric engines (backends).
module Typed =

    [<Struct; NoEquality; NoComparison; StructuredFormatDisplay("{Inner}")>]
    type FsScalar<'S,[<Measure>] 'u> =
        {
            Inner: 'S
            Backend : NumericBackend<'S,'S> }

        static member inline (+)(a: FsScalar<'S,'u>, b: FsScalar<'S,'u>) : FsScalar<'S,'u> = { Inner = a.Backend.add a.Inner b.Inner; Backend = a.Backend }
        static member inline (-)(a: FsScalar<'S,'u>, b: FsScalar<'S,'u>) : FsScalar<'S,'u> = { Inner = a.Backend.sub a.Inner b.Inner; Backend = a.Backend }
        static member inline (*)(a: FsScalar<'S,'u>, b: FsScalar<'S,'v>) : FsScalar<'S,'u * 'v> = { Inner = a.Backend.mul a.Inner b.Inner; Backend = a.Backend }
        static member inline (/)(a: FsScalar<'S,'u>, b: FsScalar<'S,'v>) : FsScalar<'S,'u / 'v> = { Inner = a.Backend.div a.Inner b.Inner; Backend = a.Backend }

        static member inline (*)(k: float, a: FsScalar<'S,'u>) : FsScalar<'S,'u> = { Inner = a.Backend.mul a.Inner (a.Backend.atomic.fromFloatS k); Backend = a.Backend }
        static member inline (*)(a: FsScalar<'S,'u>, k: float) : FsScalar<'S,'u> = { Inner = a.Backend.mul a.Inner (a.Backend.atomic.fromFloatS k); Backend = a.Backend }


    [<Struct; NoEquality; NoComparison; StructuredFormatDisplay("{Inner}")>]
    type FsVector<'S,'V,[<Measure>] 'u> =
        {
            Inner: 'V
            Backend: NumericBackend<'S,'V>
            Access: VectorAccessOps<'S,'V> }

        // Vector–Vector elementwise
        static member inline (+)(a: FsVector<'S,'V,'u>, b: FsVector<'S,'V,'u>) : FsVector<'S,'V,'u> = { Inner = a.Backend.add a.Inner b.Inner; Backend = a.Backend; Access = a.Access }
        static member inline (-)(a: FsVector<'S,'V,'u>, b: FsVector<'S,'V,'u>) : FsVector<'S,'V,'u> = { Inner = a.Backend.sub a.Inner b.Inner; Backend = a.Backend; Access = a.Access }
        static member inline (*)(a: FsVector<'S,'V,'u>, b: FsVector<'S,'V,'v>) : FsVector<'S,'V,'u * 'v> = { Inner = a.Backend.mul a.Inner b.Inner; Backend = a.Backend; Access = a.Access }
        static member inline (/)(a: FsVector<'S,'V,'u>, b: FsVector<'S,'V,'v>) : FsVector<'S,'V,'u / 'v> = { Inner = a.Backend.div a.Inner b.Inner; Backend = a.Backend; Access = a.Access }

        // Vector–Scalar broadcast
        static member (*)(v: FsVector<'S,'V,'u>, s: FsScalar<'S,'v>) : FsVector<'S,'V,'u * 'v> =
            { Inner = v.Backend.scaleVS v.Inner s.Inner
              Backend = v.Backend
              Access = v.Access }

        static member (*)(s: FsScalar<'S,'u>, v: FsVector<'S,'V,'v>) : FsVector<'S,'V,'u * 'v> =
            { Inner = v.Backend.scaleSV s.Inner v.Inner
              Backend = v.Backend
              Access = v.Access }

        static member (+)(v: FsVector<'S,'V,'u>, s: FsScalar<'S,'u>) : FsVector<'S,'V,'u> =
            { Inner = v.Backend.add v.Inner (v.Backend.ofScalar s.Inner)
              Backend = v.Backend
              Access = v.Access }

        static member (/)(v: FsVector<'S,'V,'u>, s: FsScalar<'S,'v>) : FsVector<'S,'V,'u / 'v> =
            { Inner = v.Backend.scaleVS v.Inner s.Inner
              Backend = v.Backend
              Access = v.Access }

        // Vector–float exponent
        static member ( ** )(v: FsVector<'S,'V,'u>, p: float) : FsVector<'S,'V,1> =
            { Inner = v.Backend.mul v.Inner (v.Backend.ofScalar (v.Backend.atomic.fromFloatS p))
              Backend = v.Backend
              Access = v.Access }


    [<Struct; NoEquality; NoComparison>]
    type FsMatrix<'S, 'M, [<Measure>] 'u> =
        private { Inner: 'M; Backend: NumericBackend<'S,'M>; Access: MatrixAccessOps<'S,'M> }


    let ofScalar (backend: NumericBackend<'S,'S>) (Inner: float<'u>) : FsScalar<'S,'u> =
        { Inner = backend.atomic.fromFloatS (float Inner); Backend = backend }

    let ofVector backend accessOps (data: float<'u>[]) : FsVector<'S,'V,'u> =
        if Array.isEmpty data then
            invalidArg "data" "Cannot create a TypedVector<_> from an empty array."
        let v = data |> Array.map (float >> backend.atomic.fromFloatS) |> accessOps.ofArray
        { Inner = v; Backend = backend; Access = accessOps }

    let ofMatrix backend accessOps (data: float<'u>[,]) : FsMatrix<'S,'M,'u> =
        if Array2D.length1 data = 0 || Array2D.length2 data = 0 then
            invalidArg "data" "Cannot create a TypedVector<_> from an empty array."
        let v = data |> Array2D.map (float >> backend.atomic.fromFloatS) |> accessOps.ofArray2d
        { Inner = v; Backend = backend; Access = accessOps }


    /// Functions for working with typed scalars.
    module Scalar =

        /// Change the unit-of-measure phantom type of a TypedTensor without altering its Inner.
        /// This is purely a compile-time reinterpretation; the underlying value is unchanged.
        let retype<'S, [<Measure>] 'u, [<Measure>] 'v> (t: FsScalar<'S,'u>) : FsScalar<'S,'v> =
            { Inner = t.Inner; Backend = t.Backend }

        let ofRaw backend v : FsScalar<'S,1> =
            { Inner = v; Backend = backend }

        let toRaw (v:FsScalar<'S,'u>) : 'S = v.Inner

        let constants eng = {
            invalid = eng.scalarBackend.atomic.constants.invalid |> ofRaw eng.scalarBackend
            penalty = eng.scalarBackend.atomic.constants.penalty |> ofRaw eng.scalarBackend
            absTol = eng.scalarBackend.atomic.constants.absTol |> ofRaw eng.scalarBackend
            relTol = eng.scalarBackend.atomic.constants.relTol |> ofRaw eng.scalarBackend
            nan = eng.scalarBackend.atomic.constants.nan |> ofRaw eng.scalarBackend
            zero = eng.scalarBackend.atomic.constants.zero |> ofRaw eng.scalarBackend
            half = eng.scalarBackend.atomic.constants.half |> ofRaw eng.scalarBackend
            one = eng.scalarBackend.atomic.constants.one |> ofRaw eng.scalarBackend
            two = eng.scalarBackend.atomic.constants.two |> ofRaw eng.scalarBackend
            six = eng.scalarBackend.atomic.constants.six |> ofRaw eng.scalarBackend
            pi = eng.scalarBackend.atomic.constants.pi |> ofRaw eng.scalarBackend
        }

        let broadcast (engine: NumericEngine<'S,'V,'M>) (s: FsScalar<'S,'u>) (len: int) : FsVector<'S,'V,'u> =
            let data : 'V = engine.vectorBackend.promote s.Inner len
            { Inner = data; Backend = engine.vectorBackend; Access = engine.vectorAccess }

        let add (s1:FsScalar<'S,'a>) (s2: FsScalar<'S,'a>) : FsScalar<'S,'a> =
            { Inner = s1.Backend.add s1.Inner s2.Inner; Backend = s1.Backend }

        let sub (s1:FsScalar<'S,'a>) (s2: FsScalar<'S,'a>) : FsScalar<'S,'a> =
            { Inner = s1.Backend.sub s1.Inner s2.Inner; Backend = s1.Backend }

        let clamp (s:FsScalar<'S,'a>) (low: FsScalar<'S,'a>) (high: FsScalar<'S,'a>) : FsScalar<'S,'a> =
            let clamped = s.Backend.min high.Inner (s.Backend.max low.Inner s.Inner)
            { s with Inner = clamped }

        let sign (s:FsScalar<'S, 'u>) : FsScalar<'S,1> =
            let zero = s.Backend.ofScalar s.Backend.atomic.constants.zero
            let result = s.Backend.sub (s.Backend.gt s.Inner zero) (s.Backend.lt s.Inner zero)
            { Inner = result; Backend = s.Backend }

        /// Rectified Linear Unit. If s is positive, returns s, otherwise zero.
        let reLU (s:FsScalar<'S, 'u>) =
            { s with Inner = s.Backend.max s.Backend.atomic.constants.zero s.Inner }

        let exp (a: FsScalar<'S,1>) : FsScalar<'S,1> = { a with Inner = a.Backend.exp a.Inner }

        let isNan (s: FsScalar<'S,'u>) : FsScalar<'S,1> =
            let absDiff = { s with Inner = s.Backend.abs (s.Backend.sub s.Inner s.Inner) }
            let sign = sign absDiff
            reLU sign

        // TODO is this correct?
        let isInf (s: FsScalar<'S,'u>) : FsScalar<'S,1> =
            let absThis = s.Backend.abs s.Inner
            let invAbs = s.Backend.div s.Backend.atomic.constants.one absThis
            let invAbsE = {Inner = invAbs; Backend = s.Backend }
            let isZero = s.Backend.sub s.Backend.atomic.constants.one (reLU (sign invAbsE)).Inner
            { Inner = isZero; Backend = s.Backend }

        // let isFinite (x: TypedScalar<'S,'u>) : TypedScalar<'S,1> =
        //     let nanMask = isNan x
        //     let infMask = isInf x
        //     let badMask = x.Backend.max (nanMask.AsTensor().Inner, infMask.AsTensor().Inner)
        //     { Inner = x.Backend.atomic.constants.one.Inner - badMask; Backend = x.Backend }

        let floor (s: FsScalar<'S,'u>) : FsScalar<'S,'u> = { s with Inner = s.Backend.floor s.Inner }

        let log (s: FsScalar<'S,'u>) : FsScalar<'S,1> = { Inner = s.Backend.log s.Inner; Backend = s.Backend }

        let max (s1:FsScalar<'S,'a>) (s2: FsScalar<'S,'a>) : FsScalar<'S,'a> =
            { s1 with Inner = s1.Backend.max s2.Inner s2.Inner }

        let min (s1:FsScalar<'S,'a>) (s2: FsScalar<'S,'a>) : FsScalar<'S,'a> =
            { s1 with Inner = s1.Backend.min s2.Inner s2.Inner }

        let pow (a: FsScalar<'S,'u>) (p: FsScalar<'S,'u>) : FsScalar<'S,'u> = { Inner = a.Backend.pow a.Inner p.Inner; Backend = a.Backend }

        let toFloat (t: FsScalar<'S,'u>) : float<'u> =
            t.Backend.atomic.toFloatS t.Inner |> LanguagePrimitives.FloatWithMeasure<'u>

        // let gt (l: TypedScalar<'S,'u>) (r: TypedScalar<'S,'u>) : TypedScalar<'B,1> =
        //     let r = l.Backend.max l.Backend.atomic.constants.zero (l.Backend.sign)
        //     { Inner = l.Backend.max l.Backend.atomic.constants.zero D.ReLU (D.Sign(l.Inner - r.Inner)) }
            
        // let lt (l: TypedScalar<'S,'u>) (r: TypedScalar<'S,'u>) : TypedScalar<'B,1> =
        //     reLU (sign (r - l))

        let eq (l: FsScalar<'S,'u>) (r: FsScalar<'S,'u>) =
            let absDiff = { l with Inner = l.Backend.abs (l.Backend.sub l.Inner r.Inner) }
            let diffMask = absDiff |> sign |> reLU
            { Inner = l.Backend.sub l.Backend.atomic.constants.one diffMask.Inner; Backend = l.Backend }

        // let grad (fn: TypedScalar<'S, 'u> -> TypedScalar<'S,'v>) (x:TypedScalar<'S,'u>) : TypedScalar<'S,'v / 'u> =
        //     let fnRaw (rawX: D) : D =
        //         let boxedX : TypedScalar<'S,'u> = { Inner = rawX }
        //         let boxedY = fn boxedX
        //         boxedY.Inner            
        //     let result = grad fnRaw x.Inner
        //     { Inner = result }

        let square (x: FsScalar<'S,'u>) : FsScalar<'S,'u^2> = { Inner = x.Backend.pow x.Inner x.Backend.atomic.constants.two; Backend = x.Backend }

        let sqrt (x: FsScalar<'S,'u^2>) : FsScalar<'S,'u> =
            { Inner = x.Backend.sqrt x.Inner; Backend = x.Backend }

        let sigmoid (s: FsScalar<'S,1>) : FsScalar<'S,1> =
            { Inner = s.Backend.sigmoid s.Inner; Backend = s.Backend }


    module Vector =

        let retype<'S,'V,[<Measure>] 'u, [<Measure>] 'v> (t: FsVector<'S,'V,'u>) : FsVector<'S,'V,'v> =
            { Inner = t.Inner; Backend = t.Backend; Access = t.Access }

        let toRaw (v:FsVector<'S,'V,'u>) : 'V = v.Inner

        let ofRaw backend access v : FsVector<'S,'V, 1> =
            { Inner = v; Backend = backend; Access = access }

        let ofArray backend access (s:FsScalar<'S,'u>[]) : FsVector<'S,'V,'u> =
            { Inner = access.ofArray (s |> Array.map(fun s -> s.Inner)); Backend = backend; Access = access }

        let length (t: FsVector<'S,'V,'u>) = t.Backend.length t.Inner

        let itemAt scalarBackend (i: int) (v: FsVector<'S,'C,'u>) : FsScalar<'S,'u> =
            { Inner = v.Access.get v.Inner i; Backend = scalarBackend }

        let itemAtFloat scalarBackend (i: int) (v: FsVector<'S,'V,'u>) =
            itemAt scalarBackend i v
            |> fun s -> s.Backend.atomic.toFloatS s.Inner
            |> LanguagePrimitives.FloatWithMeasure<'u>

        /// Prepend a scalar to the front of a vector.
        let prepend (head: FsScalar<'S,'u>) (tail: FsVector<'S,'V,'u>) : FsVector<'S,'V,'u> =
            let headVector = tail.Access.ofArray [| head.Inner |]
            let concatenated = tail.Access.concat headVector tail.Inner
            { Inner = concatenated; Backend = tail.Backend; Access = tail.Access }

        let toArray scalarBackend (v: FsVector<'S,'V,'u>) : FsScalar<'S,'u>[] =
            v.Access.toArray v.Inner
            |> Array.map(fun t -> { Inner = t; Backend = scalarBackend })

        let toArrayFloat scalarBackend (v: FsVector<'S,'V,'u>) : float<'u>[] =
            toArray scalarBackend v
            |> Array.map(fun s -> s.Backend.atomic.toFloatS s.Inner)
            |> Array.map LanguagePrimitives.FloatWithMeasure<'u>

        /// Filter a vector tensor by a boolean mask.
        /// The mask length must match the vector length.
        let filterByMask scalarBackend (mask: bool[]) (v: FsVector<'S,'V,'u>) : FsVector<'S,'V,'u> =
            if mask.Length <> length v then
                invalidArg "mask" (sprintf "Mask length mismatch (%i vs %i data length)" mask.Length (length v))

            let selectedElements = 
                mask
                |> Array.mapi (fun i keep -> if keep then Some (itemAt scalarBackend i v) else None)
                |> Array.choose id
                |> Array.map(fun s -> s.Inner)

            { Inner = v.Access.ofArray selectedElements; Backend = v.Backend; Access = v.Access }


        let dot scalarBackend (a: FsVector<'S,'V,'u>) (b: FsVector<'S,'V,'v>) : FsScalar<'S,'u * 'v> =
            let m = a.Backend.mul a.Inner b.Inner
            let r = a.Backend.reduce a.Backend.atomic.addS m
            { Inner = r; Backend = scalarBackend }

        let scale (s: FsScalar<'S,'a>) (v: FsVector<'S,'V,'b>) : FsVector<'S,'V,'a * 'b> = s * v

        let square (x: FsVector<'S,'V,'u>) : FsVector<'S,'V,'u^2> = { Inner = x.Backend.pow x.Inner x.Backend.atomic.constants.two; Backend = x.Backend; Access = x.Access }
        let sqrt (x: FsVector<'S,'V,'u^2>) : FsVector<'S,'V,'u> = { Inner = x.Backend.sqrt x.Inner; Backend = x.Backend; Access = x.Access }

        let sum scalarBackend (a: FsVector<'S,'V,'u>) : FsScalar<'S,'u> =
            let sum = a.Backend.reduce a.Backend.atomic.addS a.Inner
            { Inner = sum; Backend = scalarBackend }

        let sign (v: FsVector<'S,'V,'u>) : FsVector<'S,'V,1> =
            let zero = v.Backend.ofScalar v.Backend.atomic.constants.zero
            let result = v.Backend.sub (v.Backend.gt v.Inner zero) (v.Backend.lt v.Inner zero)
            { Inner = result; Backend = v.Backend; Access = v.Access }

        let tail (v: FsVector<'S,'V,'u>) : FsVector<'S,'V,'u> =
            let len = length v

            if len < 2 then
                invalidArg "v" "Vector must have at least two elements to take tail."
            else
                { Inner = v.Access.tail v.Inner; Backend = v.Backend; Access = v.Access }

        let log (a: FsVector<'S,'V,'u>) : FsVector<'S,'V,1> = { Inner = a.Backend.log a.Inner; Backend = a.Backend; Access = a.Access }
        let exp (a: FsVector<'S,'V,1>) : FsVector<'S,'V,1> = { Inner = a.Backend.exp a.Inner; Backend = a.Backend; Access = a.Access }


    module Matrix =

        let toRaw (m:FsMatrix<'S,'M,'u>) : 'M = m.Inner
        let ofRaw backend access (v:'M) : FsMatrix<'S,'M, 'u> =
            { Inner = v; Backend = backend; Access = access }

        let matMul (a: FsMatrix<'S,'M,'u>) (b: FsMatrix<'S,'M,'v>) : FsMatrix<'S,'M,'u * 'v> =
            { Inner = a.Backend.mul a.Inner b.Inner; Backend = a.Backend; Access = a.Access }
    
        let nRows (t: FsMatrix<'S,'M,'u>) = t.Access.nrows t.Inner
        let nCols (t: FsMatrix<'S,'M,'u>) = t.Access.ncols t.Inner

        let transpose (t: FsMatrix<'S,'M,'u>) =
            { Inner = t.Access.transpose t.Inner; Backend = t.Backend; Access = t.Access }

module Stats =

    open Typed

    let mean (b : NumericBackend<'S,'V>) (values : 'V) : 'S =
        let total = b.reduce b.atomic.addS values
        let len   = b.length values |> float |> b.atomic.fromFloatS
        b.atomic.divS total len

    let linspace
        (engine : NumericEngine<'S,'V,'M>)
        (start: FsScalar<'S, 'u>) (stop: FsScalar<'S, 'u>) (num: int) : FsVector<'S,'V,'u> =
        if num <= 1 then failwith "Cannot space less than two points along a line"
        let steps = ofScalar start.Backend (float (num - 1))
        let stepSize = (stop - start) / steps
        Array.init num (fun i -> 
        let idx = ofScalar start.Backend (float i)
        start + idx * stepSize
        )
        |> Vector.ofArray engine.vectorBackend engine.vectorAccess

    let stack1D vectorBackend (vectorAccess:VectorAccessOps<'S,'V>) (items: FsScalar<'S,'u>[]) : FsVector<'S,'V,'u> =
        if Array.isEmpty items then
            invalidArg "items" "Cannot stack an empty array of scalars into a vector."

        let arr = items |> Array.map Scalar.toRaw
        let v   = vectorAccess.ofArray arr
        Vector.ofRaw vectorBackend vectorAccess v
        |> Vector.retype

    /// Stack *row* vectors into a matrix.
    let stack2D numEngine (items: FsVector<'S,'V,'u>[]) : FsMatrix<'S,'M,'u> =

        if items.Length = 0 then array2D [] |> ofMatrix numEngine.matrixBackend numEngine.matrixAccess
        else
            let rows = items.Length
            let cols = items |> Array.map Vector.length
            if Array.distinct cols |> Array.length <> 1
            then failwith "Cannot stack vectors of different lengths"

            let m =
                Array2D.init rows cols.[0] (fun row col ->
                    numEngine.vectorAccess.get items.[row].Inner col
                )
                |> numEngine.matrixAccess.ofArray2d
            
            Matrix.ofRaw numEngine.matrixBackend numEngine.matrixAccess m


    /// Unstack a matrix as an array of rows.
    let unstack2D numEngine (matrix: FsMatrix<'S,'M,'u>) : FsVector<'S,'V,'u>[] =
        let grid2d = Matrix.toRaw matrix |> numEngine.matrixAccess.toArray2d
        let rows = Array2D.length1 grid2d
        let cols = Array2D.length2 grid2d
        Array.init rows (fun r ->
            let rowScalars = Array.init cols (fun c -> grid2d.[r, c])
            let rawVector = numEngine.vectorAccess.ofArray rowScalars
            Vector.ofRaw numEngine.vectorBackend numEngine.vectorAccess rawVector
            |> Vector.retype
        )

    /// Squared Euclidean length of a vector.
    let squaredLength scalarBackend (v: FsVector<'S,'V,'u>) : FsScalar<'S,'u^2> =
        Vector.dot scalarBackend v v

    let inline call f (x:^T) =
        f (^T : (member Backend : NumericBackend<'S,'V>) x)
            (^T : (member Inner : 'V) x)



module ActivePatterns =

    open Typed

    let (|VectorOfLen|_|) (len: int) (t: FsVector<'S,'V,'u>) =
        match Vector.length t with
        | l when l = len -> Some t: FsVector<'S,'V,'u> option
        | _ -> None

    let (|MatrixOfShape|_|) (rows: int, cols: int) (t: FsMatrix<'S,'M,'u>) =
        match Matrix.nRows t, Matrix.nCols t with
        | r, c when r = rows && c = cols -> Some t: FsMatrix<'S,'M,'u> option
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
            six = D 6.0
            pi = D System.Math.PI
        }

        let scalar : NumericBackend<D,D> =
            { 
                add = fun x y -> x + y
                sub = fun x y -> x - y
                mul = fun x y -> x * y
                div = fun x y -> x / y
                scaleVS = fun s1 s2 -> s1 * s2
                scaleSV = fun s1 s2 -> s1 * s2
                ofScalar = id
                min = fun x y -> D.Min(x,y)
                max = fun x y -> D.Max(x,y)
                reduce  = fun _ x -> x
                length  = fun _ -> 1
                promote = fun s _ -> s
                initV = fun n fn -> failwith "Cannot make a V when just an S?"
                abs = D.Abs
                exp = D.Exp
                log = D.Log
                pow = fun s p -> D.Pow(s,p)
                sqrt = D.Sqrt
                floor = D.Floor
                gt = fun x y -> if x > y then D 1. else D 0.
                lt = fun x y -> if x < y then D 1. else D 0.
                eq = fun x y -> if x = y then D 1. else D 0.
                isNan = fun x -> if System.Double.IsNaN(float x) then D 1. else D 0.
                isInf = fun x -> if System.Double.IsInfinity(float x) then D 1. else D 0.
                atomic = {
                    addS = fun x y -> x + y
                    subS = fun x y -> x - y
                    mulS = fun x y -> x * y
                    divS = fun x y -> x / y
                    fromFloatS = D
                    toFloatS = fun x -> float x
                    constants = constants
                }
                sigmoid = D.Sigmoid
                sin = D.Sin
            }

        module VectorOps =

            /// Take the sign of the difference between the two
            /// vectors (element-wise).
            let private mask (diff: DV) : DV =
                let s = DV.Sign diff
                let len = DV.length diff
                let ones = DV.create len (D 1.0)                
                let shifted = s + ones
                let halfShifted = 0.5 * shifted
                DV.Floor halfShifted

            let gt x y = x - y |> mask
            let lt x y = y - x |> mask
            let eq x y =
                let diff = x - y
                let s = DV.Abs (DV.Sign diff)
                let ones = DV.create (DV.length x) (D 1.0)
                ones - s

            let elementwiseOp op (v1: DV) (v2: DV) : DV =
                let arr1 = v1.ToArray()
                let arr2 = v2.ToArray()
                let multipliedScalars = Array.map2 op arr1 arr2
                DV.ofArray multipliedScalars

            let div v1 v2 : DV = elementwiseOp (/) v1 v2
            let mul v1 v2 : DV = elementwiseOp (*) v1 v2

            let private primalOf (scalar: D) : float =
                match scalar with
                | D n -> n

                | DF(p, _, _) -> match p with | D n -> n | _ -> 0.
                | DR(p, _, _, _, _) -> match p with | D n -> n | _ -> 0.

            let isNan (v: DV) : DV =
                v.ToArray()
                |> Array.map (fun scalar ->
                    if System.Double.IsNaN (primalOf scalar) then D 1. else D 0.)
                |> DV.ofArray

            let isInf (v: DV) : DV =
                v.ToArray()
                |> Array.map (fun scalar ->
                    if System.Double.IsInfinity(primalOf scalar) then D 1.0 else D 0.0)
                |> DV.ofArray

        let vector : NumericBackend<D,DV> = {
                add = fun x y -> x + y
                sub = fun x y -> x - y
                mul = VectorOps.mul
                div = VectorOps.div
                scaleVS = fun x y -> x * y
                scaleSV = fun x y -> x * y
                ofScalar = fun x -> [| x |] |> DV.ofArray
                min = fun x y -> DV.Min(x,y)
                max = fun x y -> DV.Max(x,y)
                reduce = fun fn x -> x |> DV.toArray |> Array.reduce fn
                length = DV.length
                promote = fun s n -> DV.create n s
                initV = fun n fn -> Array.init n fn |> DV.ofArray
                abs = DV.Abs
                exp = DV.Exp
                log = DV.Log
                pow = fun s p -> DV.Pow(s,p)
                sqrt = DV.Sqrt
                floor = DV.Floor
                gt = VectorOps.gt
                lt = VectorOps.lt
                eq = VectorOps.eq
                isNan = VectorOps.isNan
                isInf = VectorOps.isInf
                atomic = {
                    addS = fun x y -> x + y
                    subS = fun x y -> x - y
                    mulS = fun x y -> x * y
                    divS = fun x y -> x / y
                    fromFloatS = D
                    toFloatS = fun x -> float x
                    constants = constants
                }
                sigmoid = DV.Sigmoid
                sin = DV.Sin
        }

        module MatrixOps =

            let elementwise (op: D -> D -> D) (m1: DM) (m2: DM) : DM =
                let arr1 = m1 |> DM.toArray2D
                let arr2 = m2 |>  DM.toArray2D
                let rows = Array2D.length1 arr1
                let cols = Array2D.length2 arr1
                let resultGrid = Array2D.init rows cols (fun r c -> op arr1.[r, c] arr2.[r, c])
                DM.ofArray2D resultGrid

            let internal primalOf (scalar: D) : float =
                match scalar with
                | D n -> n
                | DF(p, _, _) -> match p with | D n -> n | _ -> 0.0
                | DR(p, _, _, _, _) -> match p with | D n -> n | _ -> 0.0

            let unaryMask (pred: D -> bool) (m: DM) : DM =
                let arr = m |> DM.toArray2D
                let rows = Array2D.length1 arr
                let cols = Array2D.length2 arr
                let grid = Array2D.init rows cols (fun r c -> if pred arr.[r, c] then D 1.0 else D 0.0)
                DM.ofArray2D grid

            let flatten2D (arr2d: 'T[,]) : 'T[] =
                let rows = Array2D.length1 arr2d
                let cols = Array2D.length2 arr2d
                let res = Array.zeroCreate (rows * cols)
                let mutable i = 0
                for r in 0 .. rows - 1 do
                    for c in 0 .. cols - 1 do
                        res.[i] <- arr2d.[r, c]
                        i <- i + 1
                res

        let matrix : NumericBackend<D,DM> = {
                add = fun x y -> x + y
                sub = fun x y -> x - y
                mul = fun x y -> x * y
                div = MatrixOps.elementwise (/)
                scaleVS = fun x y -> x * y
                scaleSV = fun x y -> x * y
                ofScalar = fun s -> DM.ofArray 1 [| s |]
                min = fun x y -> DM.Min(x,y)
                max = fun x y -> DM.Max(x,y)
                reduce = fun op m -> 
                    let flatScalars = m |> DM.toArray2D |> MatrixOps.flatten2D
                    Array.reduce op flatScalars
                length = fun m -> 
                    let arr = m |> DM.toArray2D
                    Array2D.length1 arr * Array2D.length2 arr
                promote = fun s size -> 
                    let grid = Array2D.create size size s
                    DM.ofArray2D grid
                initV = fun size initializer -> 
                    let grid = Array2D.init size size (fun r c -> initializer r)
                    DM.ofArray2D grid
                abs = DM.Abs
                exp = DM.Exp
                log = DM.Log
                pow = fun x y -> DM.Pow(y,x)
                sqrt = DM.Sqrt
                floor = DM.Floor
                gt = MatrixOps.elementwise (fun x y -> if x > y then D 1.0 else D 0.0)
                lt = MatrixOps.elementwise (fun x y -> if x < y then D 1.0 else D 0.0)
                eq = MatrixOps.elementwise (fun x y -> if x = y then D 1.0 else D 0.0)
                isNan = MatrixOps.unaryMask (fun s -> System.Double.IsNaN(MatrixOps.primalOf s))
                isInf = MatrixOps.unaryMask (fun s -> System.Double.IsInfinity(MatrixOps.primalOf s))
                atomic = {
                    addS = fun x y -> x + y
                    subS = fun x y -> x - y
                    mulS = fun x y -> x * y
                    divS = fun x y -> x / y
                    fromFloatS = D
                    toFloatS = fun x -> float x
                    constants = constants
                }
                sigmoid = DM.Sigmoid
                sin = DM.Sign
        }

        let vectorAccess : VectorAccessOps<D,DV> = {
            toArray = DV.toArray
            ofArray = DV.ofArray
            length = DV.length
            get = fun c i -> c.Item i
            concat = fun a b -> DV.concat [ a; b ]
            tail = fun v -> v.[1..]
        }

        let matrixAccess : MatrixAccessOps<D,DM> = {
            get = fun c i j -> c.[i,j]
            ncols = DM.cols
            nrows = DM.rows
            ofArray2d = DM.ofArray2D
            toArray2d = DM.toArray2D
            transpose = DM.transpose
        }

        let engine = {
            scalarBackend = scalar
            vectorBackend = vector
            matrixBackend = matrix
            vectorAccess = vectorAccess
            matrixAccess = matrixAccess
        }
