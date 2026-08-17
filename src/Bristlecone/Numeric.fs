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
    pow : 'C -> 'C -> 'C
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
type VectorAccessOps<'S,'C> = {
    toArray : 'C -> 'S array
    ofArray : 'S array -> 'C
    length  : 'C -> int
    get     : 'C -> int -> 'S
}

/// Matrix-specific structural access ops
type MatrixAccessOps<'S,'C> = {
    toArray2d : 'C -> 'S array2d
    ofArray2d : 'S array2d -> 'C
    nrows     : 'C -> int
    ncols     : 'C -> int
    get       : 'C -> int -> int -> 'S
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

[<NoEquality; NoComparison; StructuredFormatDisplay("{Inner}")>]
type Numeric<'S,'C,'R,[<Measure>] 'u> =
    {
        Inner     : 'C
        Backend   : NumericBackend<'S,'C>
        Access    : AccessOps<'S,'C,'R>
    }

// type RankScalar = Rank0
// type RankVector = Rank1
// type RankMatrix = Rank2

// type NumericScalar<'S,[<Measure>] 'u> = Numeric<'S,'S,RankScalar,'u>
// type NumericVector<'S,'V,[<Measure>] 'u> = Numeric<'S,'V,RankVector,'u>
// type NumericMatrix<'S,'M,[<Measure>] 'u> = Numeric<'S,'M,RankMatrix,'u>


// module Lift =

//     // type Lift<'S,'C,'R,'C2,'R2, [<Measure>] 'U> =
//     //     Numeric<'S,'C,'R,'U> -> Numeric<'S,'C2,'R2,'U>

//     let scalarToVector
//         (backendV : NumericBackend<'S,'V>)
//         (accessV  : VectorAccessOps<'S,'V>)
//         (x : Numeric<'S,'S,RankScalar,'U>)
//         : Numeric<'S,'V,RankVector,'U> =
//         let v = backendV.promote x.Inner 1
//         { Inner = v; Backend = backendV; Access = VectorAccess accessV }

//     let vectorToMatrix
//         (backendM : NumericBackend<'S,'M>)
//         (accessV  : VectorAccessOps<'S,'V>)
//         (accessM  : MatrixAccessOps<'S,'M>)
//         (x : Numeric<'S,'V,RankVector,'U>)
//         : Numeric<'S,'M,RankMatrix,'U> =
//         let arr  = accessV.toArray x.Inner
//         let arr2 = arr |> Array.map (fun v -> [| v |]) |> array2D
//         let m    = accessM.ofArray2d arr2
//         { Inner = m; Backend = backendM; Access = MatrixAccess accessM }


//     // // Test

//     let addScalarVector
//         (s : Numeric<'S,'S,Rank0,'U>)
//         (v : Numeric<'S,'V,Rank1,'U>)
//         : Numeric<'S,'V,Rank1,'U> =
        
//         let b = v.Backend
//         let inner = b.add (b.promote s.Inner (b.length v.Inner)) v.Inner
//         { Inner = inner; Backend = b; Access = v.Access }




//     // let addScalarVector
//     //     (s : Numeric<'S,'S,RankScalar,'U>)
//     //     (v : Numeric<'S,'V,RankVector,'U>)
//     //     : Numeric<'S,'V,RankVector,'U> =
//     //     let b = v.Backend
//     //     let inner = b.add (b.promote s.Inner (b.length v.Inner)) v.Inner
//     //     { Inner = inner; Backend = b; Access = v.Access }

//     // let addVectorMatrix
//     //     backendV accessV backendM accessM
//     //     (v : Numeric<'S,'V,RankVector,'U>)
//     //     (m : Numeric<'S,'M,RankMatrix,'U>)
//     //     : Numeric<'S,'M,RankMatrix,'U> =
//     //     // lift vector → matrix, then reuse addScalarVector‑style logic at Rank2
//     //     let vAsMatrix = vectorToMatrix backendM accessV accessM v
//     //     let b = m.Backend
//     //     let inner = b.add vAsMatrix.Inner m.Inner
//     //     { Inner = inner; Backend = b; Access = m.Access }

// module Tests =

//     let sin (x : Numeric<'S,'C,'R,'U>) : Numeric<'S,'C,'R,'U> =
//         let backend = x.Backend
//         let inner   = backend.sin x.Inner
//         { Inner = inner; Backend = backend; Access = x.Access }

//     let axpy
//         (a : NumericScalar<'S,'U>)
//         (x : Numeric<'S,'C,'R,'U>)
//         (y : Numeric<'S,'C,'R,'U>)
//         : Numeric<'S,'C,'R,'U> =

//         let b = x.Backend
//         let scaled = b.scaleSV a.Inner x.Inner
//         let sum    = b.add scaled y.Inner

//         { Inner = sum; Backend = b; Access = x.Access }




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

        static member inline (*)(k: float, a: TypedScalar<'S,'u>) = { Inner = a.Backend.mul a.Inner (a.Backend.atomic.fromFloatS k); Backend = a.Backend }
        static member inline (*)(a: TypedScalar<'S,'u>, k: float) = { Inner = a.Backend.mul a.Inner (a.Backend.atomic.fromFloatS k); Backend = a.Backend }


    // [<NoEquality; NoComparison>]
    // type TypedScalar<'B,1> =
    //     private { Inner: 'B }

    //     with
    //         member this.AsTensor () : TypedScalar<'S,1> = { Inner = this.Inner }
    //         member this.AsBool () : bool = this.Inner |> float = 1.0


    [<NoEquality; NoComparison; StructuredFormatDisplay("{Inner}")>]
    type TypedVector<'S,'V,[<Measure>] 'u> =
        internal {
            Inner: 'V
            Backend: NumericBackend<'S,'V>
            Access: VectorAccessOps<'S,'V> }

        // Vector–Vector elementwise
        static member inline (+)(a: TypedVector<'S,'V,'u>, b: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,'u> = { Inner = a.Backend.add a.Inner b.Inner; Backend = a.Backend; Access = a.Access }
        static member inline (-)(a: TypedVector<'S,'V,'u>, b: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,'u> = { Inner = a.Backend.sub a.Inner b.Inner; Backend = a.Backend; Access = a.Access }
        static member inline (*)(a: TypedVector<'S,'V,'u>, b: TypedVector<'S,'V,'v>) : TypedVector<'S,'V,'u * 'v> = { Inner = a.Backend.mul a.Inner b.Inner; Backend = a.Backend; Access = a.Access }
        static member inline (/)(a: TypedVector<'S,'V,'u>, b: TypedVector<'S,'V,'v>) : TypedVector<'S,'V,'u / 'v> = { Inner = a.Backend.div a.Inner b.Inner; Backend = a.Backend; Access = a.Access }

        // Vector–Scalar broadcast
        static member (*)(v: TypedVector<'S,'V,'u>, s: TypedScalar<'S,'v>) : TypedVector<'S,'V,'u * 'v> =
            { Inner = v.Backend.scaleVS v.Inner s.Inner
              Backend = v.Backend
              Access = v.Access }

        static member (*)(s: TypedScalar<'S,'u>, v: TypedVector<'S,'V,'v>) : TypedVector<'S,'V,'u * 'v> =
            { Inner = v.Backend.scaleSV s.Inner v.Inner
              Backend = v.Backend
              Access = v.Access }

        static member (+)(v: TypedVector<'S,'V,'u>, s: TypedScalar<'S,'u>) : TypedVector<'S,'V,'u> =
            { Inner = v.Backend.add v.Inner (v.Backend.ofScalar s.Inner)
              Backend = v.Backend
              Access = v.Access }

        static member (/)(v: TypedVector<'S,'V,'u>, s: TypedScalar<'S,'v>) : TypedVector<'S,'V,'u / 'v> =
            { Inner = v.Backend.scaleVS v.Inner s.Inner
              Backend = v.Backend
              Access = v.Access }

        // Vector–float exponent
        static member ( ** )(v: TypedVector<'S,'V,'u>, p: float) : TypedVector<'S,'V,1> =
            { Inner = v.Backend.mul v.Inner (v.Backend.ofScalar (v.Backend.atomic.fromFloatS p))
              Backend = v.Backend
              Access = v.Access }


    [<NoEquality; NoComparison>]
    type TypedMatrix<'S, 'M, [<Measure>] 'u> =
        private { Inner: 'M; Backend: NumericBackend<'S,'M>; Access: MatrixAccessOps<'S,'M> }


    let ofScalar (backend: NumericBackend<'S,'S>) (Inner: float<'u>) : TypedScalar<'S,'u> =
        { Inner = backend.atomic.fromFloatS (float Inner); Backend = backend }

    let ofVector backend accessOps (data: float<'u>[]) : TypedVector<'S,'V,'u> =
        if Array.isEmpty data then
            invalidArg "data" "Cannot create a TypedVector<_> from an empty array."
        let v = data |> Array.map (float >> backend.atomic.fromFloatS) |> accessOps.ofArray
        { Inner = v; Backend = backend; Access = accessOps }

    let ofMatrix backend accessOps (data: float<'u>[,]) : TypedMatrix<'S,'M,'u> =
        if Array2D.length1 data = 0 || Array2D.length2 data = 0 then
            invalidArg "data" "Cannot create a TypedVector<_> from an empty array."
        let v = data |> Array2D.map (float >> backend.atomic.fromFloatS) |> accessOps.ofArray2d
        { Inner = v; Backend = backend; Access = accessOps }


    /// Functions for working with typed scalars.
    module Scalar =

        /// Change the unit-of-measure phantom type of a TypedTensor without altering its Inner.
        /// This is purely a compile-time reinterpretation; the underlying value is unchanged.
        let retype<'S, [<Measure>] 'u, [<Measure>] 'v> (t: TypedScalar<'S,'u>) : TypedScalar<'S,'v> =
            { Inner = t.Inner; Backend = t.Backend }

        let ofBackend backend v : TypedScalar<'S,1> =
            { Inner = v; Backend = backend }

        let toRaw (v:TypedScalar<'S,'u>) : 'S = v.Inner

        let broadcast engine (s: TypedScalar<'S,'u>) (len: int) : TypedVector<'S,'V,'u> =
            engine.scalarBackend.promote s.Inner len
            |> ofVector engine.vectorBackend engine.vectorAccess

        let clamp (s:TypedScalar<'S,'a>) (low: TypedScalar<'S,'a>) (high: TypedScalar<'S,'a>) : TypedScalar<'S,'a> =
            let clamped = s.Backend.min high.Inner (s.Backend.max low.Inner s.Inner)
            { s with Inner = clamped }

        let sign (s:TypedScalar<'S, 'u>) =
            let zero = s.Backend.ofScalar s.Backend.atomic.constants.zero
            let result = s.Backend.sub (s.Backend.gt s.Inner zero) (s.Backend.lt s.Inner zero)
            { s with Inner = result }

        /// Rectified Linear Unit. If s is positive, returns s, otherwise zero.
        let reLU (s:TypedScalar<'S, 'u>) =
            { s with Inner = s.Backend.max s.Backend.atomic.constants.zero s.Inner }

        let exp (a: TypedScalar<'S,1>) : TypedScalar<'S,1> = { a with Inner = a.Backend.exp a.Inner }

        let isNan (s: TypedScalar<'S,'u>) : TypedScalar<'S,1> =
            let absDiff = { s with Inner = s.Backend.abs (s.Inner - s.Inner) }
            let sign = sign absDiff
            reLU sign

        let isInf (s: TypedScalar<'S,'u>) : TypedScalar<'S,1> =
            let absThis = s.Backend.abs s.Inner
            let invAbs = s.Backend.div s.Backend.atomic.constants.one absThis
            let isZero = s.Backend.sub s.Backend.atomic.constants.one { Inner = D.ReLU (D.Sign invAbs.Inner)}
            { Inner = isZero.Inner; Backend = s.Backend }

        // let isFinite (x: TypedScalar<'S,'u>) : TypedScalar<'S,1> =
        //     let nanMask = isNan x
        //     let infMask = isInf x
        //     let badMask = x.Backend.max (nanMask.AsTensor().Inner, infMask.AsTensor().Inner)
        //     { Inner = x.Backend.atomic.constants.one.Inner - badMask; Backend = x.Backend }

        let floor (s: TypedScalar<'S,'u>) : TypedScalar<'S,'u> = { s with Inner = s.Backend.floor s.Inner }

        let log (s: TypedScalar<'S,'u>) : TypedScalar<'S,1> = { Inner = s.Backend.log s.Inner; Backend = s.Backend }

        let max (s1:TypedScalar<'S,'a>) (s2: TypedScalar<'S,'a>) : TypedScalar<'S,'a> =
            { s1 with Inner = s1.Backend.max s2.Inner s2.Inner }

        let min (s1:TypedScalar<'S,'a>) (s2: TypedScalar<'S,'a>) : TypedScalar<'S,'a> =
            { s1 with Inner = s1.Backend.min s2.Inner s2.Inner }

        let pow (a: TypedScalar<'S,'u>) (p: TypedScalar<'S,'u>) : TypedScalar<'S,'u> = { Inner = a.Backend.pow a.Inner p.Inner; Backend = a.Backend }

        let toFloat (t: TypedScalar<'S,'u>) : float<'u> =
            t.Backend.atomic.toFloatS t.Inner |> LanguagePrimitives.FloatWithMeasure<'u>

        // let gt (l: TypedScalar<'S,'u>) (r: TypedScalar<'S,'u>) : TypedScalar<'B,1> =
        //     let r = l.Backend.max l.Backend.atomic.constants.zero (l.Backend.sign)
        //     { Inner = l.Backend.max l.Backend.atomic.constants.zero D.ReLU (D.Sign(l.Inner - r.Inner)) }
            
        // let lt (l: TypedScalar<'S,'u>) (r: TypedScalar<'S,'u>) : TypedScalar<'B,1> =
        //     reLU (sign (r - l))

        let eq (l: TypedScalar<'S,'u>) (r: TypedScalar<'S,'u>) =
            let absDiff = { l with Inner = l.Backend.abs (l.Inner - r.Inner) }
            let diffMask = absDiff |> sign |> reLU
            { Inner = l.Backend.atomic.constants.one - diffMask.Inner }

        // let grad (fn: TypedScalar<'S, 'u> -> TypedScalar<'S,'v>) (x:TypedScalar<'S,'u>) : TypedScalar<'S,'v / 'u> =
        //     let fnRaw (rawX: D) : D =
        //         let boxedX : TypedScalar<'S,'u> = { Inner = rawX }
        //         let boxedY = fn boxedX
        //         boxedY.Inner            
        //     let result = grad fnRaw x.Inner
        //     { Inner = result }

        let square (x: TypedScalar<'S,'u>) : TypedScalar<'S,'u^2> = { Inner = x.Inner ** 2.0; Backend = x.Backend }

        let sqrt (x: TypedScalar<'S,'u^2>) : TypedScalar<'S,'u> =
            { Inner = x.Backend.sqrt x.Inner; Backend = x.Backend }

        let sigmoid (s: TypedScalar<'S,1>) : TypedScalar<'S,1> =
            { Inner = s.Backend.sigmoid s.Inner; Backend = s.Backend }


    module Vector =

        let retype<'S,'V,[<Measure>] 'u, [<Measure>] 'v> (t: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,'v> =
            { Inner = t.Inner; Backend = t.Backend; Access = t.Access }

        let toRaw (v:TypedVector<'S,'V,'u>) : 'V = v.Inner

        let ofBackend backend access v : TypedVector<'S,'V, 1> =
            { Inner = v; Backend = backend; Access = access }

        let length (t: TypedVector<'S,'V,'u>) = t.Backend.length t.Inner

        let itemAt (i: int) (v: TypedVector<'S,'C,'u>) : TypedScalar<'S,'u> =
            { Inner = v.Access.get v.Inner i }

        let itemAtFloat (i: int) (t: TypedVector<'S,'V,'u>) =
            t.Access.get t.Inner i
            // t.Inner.[i] |> float |> LanguagePrimitives.FloatWithMeasure<'u>
            // { Inner = v.Inner.[i] }
            // |> float |> LanguagePrimitives.FloatWithMeasure<'u>


        /// Prepend a scalar to the front of a vector.
        let prepend (head: TypedScalar<'S,'u>) (tail: TypedVector<'S,_,'u>) : TypedVector<'S,_,'u> =
            let headVector = DV.ofArray [| head.Inner |]
            let concatenated = DV.concat [ headVector; tail.Inner ]
            { Inner = concatenated }

        let toArray (v: TypedVector<'S,'V,'u>) : TypedScalar<'S,'u>[] =            
            v.Inner.ToArray() |> Array.map(fun t -> { Inner = t })

        let toArrayFloat (v: TypedVector<'S,'V,'u>) : float<'u>[] =
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

            { Inner = DV.ofArray selectedElements; Backend = v.Backend }


        let dot (a: TypedVector<'S,'V,'u>) (b: TypedVector<'S,'V,'u>) : TypedScalar<'S,'u^2> =
            { Inner = a.Inner * b.Inner }

        let scale (s: TypedScalar<'S,'a>) (v: TypedVector<'S,'V,'b>) : TypedVector<'S,'V,'a * 'b> = s * v

        let square (x: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,'u^2> = { Inner = x.Inner ** 2.0; Backend = x.Backend; Access = x.Access }

        let sum (a: TypedVector<'S,'V,'u>) : TypedScalar<'S,'u> = { Inner = a.Backend.sum a.Inner; Backend = x.Backend; Access = x.Access } }

        let sign (v: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,1> =
            let zero = v.Backend.ofScalar v.Backend.atomic.constants.zero
            let result = v.Backend.sub (v.Backend.gt v.Inner zero) (v.Backend.lt v.Inner zero)
            { Inner = result; Backend = v.Backend; Access = v.Access }

        let tail (v: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,'u> =
            let len = length v

            if len < 2 then
                invalidArg "v" "Vector must have at least two elements to take tail."
            else
                { Inner = v.Inner.[1..]; Backend = v.Backend; Access = v.Access }

        let log (a: TypedVector<'S,'V,'u>) : TypedVector<'S,'V,1> = { Inner = a.Backend.log a.Inner; Backend = a.Backend; Access = a.Access }
        let exp (a: TypedVector<'S,'V,1>) : TypedVector<'S,'V,1> = { Inner = a.Backend.exp a.Inner; Backend = a.Backend; Access = a.Access }


    module Matrix =

        let toRaw (m:TypedMatrix<'S,'M,'u>) : 'M = m.Inner

        let matMul (a: TypedMatrix<'S,'M,'u>) (b: TypedMatrix<'S,'M,'v>) : TypedMatrix<'S,'M,'u * 'v> =
            { Inner = a.Inner * b.Inner; Backend = a.Backend; Access = a.Access }
    

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

    let stack1D vectorBackend (vectorAccess:VectorAccessOps<'S,'V>) (items: TypedScalar<'S,'u>[]) : TypedVector<'S,'V,'u> =
        if Array.isEmpty items then
            invalidArg "items" "Cannot stack an empty array of scalars into a vector."

        let arr = items |> Array.map Scalar.toRaw
        let v   = vectorAccess.ofArray arr
        Vector.ofBackend vectorBackend vectorAccess v
        |> Vector.retype

    let stack2D numEngine (items: TypedVector<'S,'V,'u>[]) : TypedMatrix<'S,'M,'u> =

        if items.Length = 0 then array2D [] |> ofMatrix numEngine.matrixBackend numEngine.matrixAccess
        else
            let cols = numEngine.vectorAccess.length (Vector.toRaw <| items.[0])



    let unstack2D (matrix: TypedMatrix<'S,'M,'u>) : TypedVector<'S,'V,'u>[] =
        failwith "not implemented!"

    /// Squared Euclidean length of a vector.
    let squaredLength (v: TypedVector<'S,'V,'u>) : TypedScalar<'S,'u^2> =
        Scalar.ofBackend (DV.L2NormSq v.Inner)

    let inline call f (x:^T) =
        f (^T : (member Backend : NumericBackend<'S,'V>) x)
            (^T : (member Inner : 'V) x)



module ActivePatterns =

    open Typed

    let (|VectorOfLen|_|) (len: int) (t: TypedVector<'S,'V,'u>) =
        match Vector.length t with
        | l when l = len -> Some t: TypedVector<'S,'V,'u> option
        | _ -> None

    // let (|MatrixOfShape|_|) (rows: int, cols: int) (t: TypedMatrix<'M,'u>) =
    //     match t.Inner.Rows, t.Inner.Cols with
    //     | r, c when r = rows && c = cols -> Some t: TypedMatrix<'M,'u> option
    //     | _ -> None


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
                abs = failwith "Not Implemented"
                exp = failwith "Not Implemented"
                log = failwith "Not Implemented"
                pow = failwith "Not Implemented"
                sqrt = failwith "Not Implemented"
                floor = failwith "Not Implemented"
                gt = failwith "Not Implemented"
                lt = failwith "Not Implemented"
                eq = failwith "Not Implemented"
                isNan = failwith "Not Implemented"
                isInf = failwith "Not Implemented"
                atomic = {
                    addS = fun x y -> x + y
                    subS = fun x y -> x + y
                    mulS = fun x y -> x * y
                    divS = fun x y -> x / y
                    fromFloatS = D
                    toFloatS = fun x -> float x
                    constants = constants
                }
                sigmoid = failwith "Not Implemented"
                sin = failwith "Not Implemented"
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
                reduce  = fun fn x -> fn x
                length  = fun _ -> 1
                promote = fun s _ -> DV.ofArray [| s |]
                initV = failwith "Not Implemented"
                abs = failwith "Not Implemented"
                exp = failwith "Not Implemented"
                log = failwith "Not Implemented"
                pow = failwith "Not Implemented"
                sqrt = failwith "Not Implemented"
                floor = failwith "Not Implemented"
                gt = failwith "Not Implemented"
                lt = failwith "Not Implemented"
                eq = failwith "Not Implemented"
                isNan = failwith "Not Implemented"
                isInf = failwith "Not Implemented"
                atomic = {
                    addS = fun x y -> x + y
                    subS = fun x y -> x + y
                    mulS = fun x y -> x * y
                    divS = fun x y -> x / y
                    fromFloatS = D
                    toFloatS = fun x -> float x
                    constants = constants
                }
                sigmoid = failwith "Not Implemented"
                sin = failwith "Not Implemented"
        }

        let engine = {
            scalarBackend = scalar
            vectorBackend = vector
            matrixBackend = 2
            vectorAccess = 3
            matrixAccess = 4
        }