namespace Bristlecone

/// An F# Domain Specific Language (DSL) for scripting with
/// Bristlecone.
module Language =

    [<NoEquality; NoComparison>]
    type StateId<[<Measure>] 'u> = private StateIdInner of ShortCode.ShortCode
        with member this.Code = this |> fun (StateIdInner c) -> c

    [<NoEquality; NoComparison>]
    type MeasureId<[<Measure>] 'u> = private MeasureIdInner of ShortCode.ShortCode
        with member this.Code = this |> fun (MeasureIdInner c) -> c

    [<NoEquality; NoComparison>]
    type ParamId<[<Measure>] 'u> = private ParamIdInner of ShortCode.ShortCode

    type IncludedParameter<[<Measure>] 'u> = {
        ParamId: ParamId<'u>
        Parameter: Parameter.Parameter<'u>
    }

    /// Define an estimatable parameter for a Bristlecone model.
    let parameter<[<Measure>] 'u> code con lower upper : IncludedParameter<'u> =
        match Parameter.create con lower upper, ShortCode.create code with
        | Some p, Some c -> { ParamId = ParamIdInner c; Parameter = p }
        | _, None -> failwithf "Short-code was not valid for parameter (%s)." code
        | None, _ -> failwithf "Could not configure parameter (%s)." code

    let state<[<Measure>] 'u> name : StateId<'u> =
        match name |> ShortCode.create with
        | Some c -> StateIdInner c
        | None -> failwithf "Short-code was not valid for state (%s)." name

    let environment<[<Measure>] 'u> name : StateId<'u> =
        match name |> ShortCode.create with
        | Some c -> StateIdInner c
        | None -> failwithf "Short-code was not valid for environment variable (%s)." name

    let measure<[<Measure>] 'u> name: MeasureId<'u>  =
        match name |> ShortCode.create with
        | Some c -> MeasureIdInner c
        | None -> failwithf "Short-code was not valid for state (%s)." name

    /// A short code representation of an identifier for a parameter,
    /// model equation, or other model component.
    let code = ShortCode.create

    let private makeResolution f n =
        match PositiveInt.create n with
        | None -> failwithf "%i is not a positive integer" n
        | Some p -> f p

    let years n = makeResolution Time.Resolution.Years n
    let months n = makeResolution Time.Resolution.Months n
    let days n = makeResolution Time.Resolution.Days n

    let noConstraints = Parameter.Constraint.Unconstrained
    let notNegative = Parameter.Constraint.PositiveOnly

    let lookup name (map: CodedMap<float>) =
        match map |> Map.tryFindBy (fun k -> k.Value = name) with
        | Some k -> k
        | None -> invalidOp (sprintf "Could not find %s in the map" name)

    let (|NotEmptyList|_|) list =
        if list |> List.isEmpty then None else Some list

    type ArbitraryRequirement =
        | ArbitraryParameter of string
        | ArbitraryEnvironment of string

    module Untyped =

        /// A model or model fragment that can be interpreted to a mathematical
        /// expression by Bristlecone.
        type ModelExpressionUntyped =
            | This
            | Time
            | Environment of string
            | Parameter of string
            | Constant of float
            | Add of ModelExpressionUntyped list
            | Subtract of ModelExpressionUntyped * ModelExpressionUntyped
            | Multiply of ModelExpressionUntyped list // List must not be an empty list?
            | Divide of ModelExpressionUntyped * ModelExpressionUntyped
            | Mod of ModelExpressionUntyped * ModelExpressionUntyped
            | Power of ModelExpressionUntyped * ModelExpressionUntyped
            | Logarithm of ModelExpressionUntyped
            | Exponential of ModelExpressionUntyped
            | Conditional of cond: ModelExpressionUntyped * ifTrue: ModelExpressionUntyped * ifFalse: ModelExpressionUntyped
            | Label of string * ModelExpressionUntyped
            | StateAt of offset:int<Time.``time index``> * string
            | Invalid
            // Comparisons:
            | GreaterThan of ModelExpressionUntyped * ModelExpressionUntyped
            | LessThan    of ModelExpressionUntyped * ModelExpressionUntyped
            | EqualTo     of ModelExpressionUntyped * ModelExpressionUntyped
            | IsFinite    of ModelExpressionUntyped
            // Functions:
            | Symbol of string
            | Inverse of expr: ModelExpressionUntyped * symbol: string *
                targetValue: ModelExpressionUntyped * lo: ModelExpressionUntyped * hi: ModelExpressionUntyped


    [<NoEquality; NoComparison>]
    type BoolExpression = private BE of Untyped.ModelExpressionUntyped

    [<NoEquality; NoComparison>]
    type ModelExpression<[<Measure>] 'u> = private ME of Untyped.ModelExpressionUntyped
        with

        static member (+) (ME a: ModelExpression<'u>, ME b: ModelExpression<'u>) : ModelExpression<'u> =
            ME (Untyped.Add [a; b])

        static member (-) (ME a: ModelExpression<'u>, ME b: ModelExpression<'u>) : ModelExpression<'u> =
            ME (Untyped.Subtract (a, b))

        static member ( * ) (ME a: ModelExpression<'u1>, ME b: ModelExpression<'u2>) : ModelExpression<'u1 * 'u2> =
            ME (Untyped.Multiply [ a; b ])

        static member ( / ) (ME a: ModelExpression<'u1>, ME b: ModelExpression<'u2>) : ModelExpression<'u1 / 'u2> =
            ME (Untyped.Divide (a, b))

        static member (%) (ME a: ModelExpression<'u>, ME m: ModelExpression<'u>) : ModelExpression<'u> =
            ME (Untyped.Mod (a, m))

        static member (~-) (ME a: ModelExpression<'u>) : ModelExpression<'u> =
            ME (Untyped.Multiply [a; Untyped.Constant -1.0])

        static member (.>) (ME a : ModelExpression<'u>, ME b : ModelExpression<'u>) : BoolExpression =
            BE (Untyped.GreaterThan(a, b))

        static member (.<) (ME a : ModelExpression<'u>, ME b : ModelExpression<'u>) : BoolExpression =
            BE (Untyped.LessThan(a, b))

        static member (=.) (ME a : ModelExpression<'u>, ME b : ModelExpression<'u>) : BoolExpression =
            BE (Untyped.EqualTo(a, b))

        static member Pow (ME a: ModelExpression<'u>, ME p: ModelExpression<1>) : ModelExpression<'u> =
            ME (Untyped.Power (a, p))

        static member Log (ME a: ModelExpression<1>) : ModelExpression<1> =
            ME (Untyped.Logarithm a)

        static member Exp (ME a: ModelExpression<1>) : ModelExpression<1> =
            ME (Untyped.Exponential a)

        static member StateAt(offset:int<Time.``time index``>, sid: StateId<'u>) : ModelExpression<'u> =
            let (StateIdInner sc) = sid
            ME (Untyped.StateAt(offset, sc.Value))

        static member Parameter(pid: IncludedParameter<'u>) : ModelExpression<'u> =
            let (ParamIdInner name) = pid.ParamId
            ME (Untyped.Parameter name.Value)

        static member Environment(sid: StateId<'u>) : ModelExpression<'u> =
            let (StateIdInner name) = sid
            ME (Untyped.Environment name.Value)

    let Constant (x: float<'u>) : ModelExpression<'u> = ME (Untyped.Constant (float x))
    let P<[<Measure>] 'u> name : ModelExpression<'u> = ModelExpression.Parameter name
    let This<[<Measure>] 'u> : ModelExpression<'u> = ME Untyped.This
    let StateAt (offset: int<Time.``time index``>, sid) : ModelExpression<'u> = ModelExpression.StateAt (offset,sid)
    let Time<[<Measure>] 't> : ModelExpression<'t> = ME Untyped.Time
    let Environment<[<Measure>] 'u> sid : ModelExpression<'u> = ModelExpression.Environment sid
    let State<[<Measure>] 'u> sid : ModelExpression<'u> = ModelExpression.Environment sid
    let Invalid<[<Measure>] 'u> : ModelExpression<'u> = ME Untyped.Invalid

    /// Operations for boolean model expressions.
    module Bool =

        let isFinite (ME a : ModelExpression<'u>) : BoolExpression =
            BE (Untyped.IsFinite a)


    /// For conditional statements, all three branches must have the same unit.
    let Conditional<[<Measure>] 'u>
        (cond    : BoolExpression)
        (ifTrue  : ModelExpression<'u>)
        (ifFalse : ModelExpression<'u>)
        : ModelExpression<'u> =
        let (BE c) = cond
        let (ME t) = ifTrue
        let (ME f) = ifFalse
        ME (Untyped.Conditional(c, t, f))

    let Inverse<[<Measure>] 'u, [<Measure>] 'v>
        (forwardFn: ModelExpression<'v> -> ModelExpression<'u>)
        (target: ModelExpression<'u>)
        (lo:ModelExpression<'v>)
        (hi:ModelExpression<'v>) : ModelExpression<'v> =
        let symName = "invVar"
        let arg = ME (Untyped.Symbol symName)
        let (ME targetExpr) = target
        let (ME fExpr) = forwardFn arg
        let ME lo, ME hi = lo, hi
        ME (Untyped.Inverse(
            fExpr, symName,
            targetExpr, lo, hi))

    let Label<[<Measure>] 'u> (label: string) (expr: ModelExpression<'u>) : ModelExpression<'u> =
        let (ME e) = expr
        ME (Untyped.Label(label, e))

    // TODO Units for power.
    let Power<[<Measure>] 'u>
        (baseExpr : ModelExpression<'u>)
        (expExpr  : ModelExpression<1>)
        : ModelExpression<'u> =
        let (ME b) = baseExpr
        let (ME e) = expExpr
        ME (Untyped.Power(b, e))

    // Logarithm: only makes sense for dimensionless inputs
    let Logarithm (expr: ModelExpression<1>) : ModelExpression<1> =
        let (ME e) = expr
        ME (Untyped.Logarithm e)

    // Exponential: exp(x) is dimensionless if x is dimensionless
    let Exponential (expr: ModelExpression<1>) : ModelExpression<1> =
        let (ME e) = expr
        ME (Untyped.Exponential e)


    module ExpressionDescribe =

        open Untyped

        /// Outputs a code-based representation of a model.
        let rec exprCode (expr: ModelExpression<'u>) : string =
            match expr with
            | ME (Constant n) -> sprintf "C[%A]" n
            | ME (Parameter n) -> sprintf "P[%s]" n
            | ME (Environment n) -> sprintf "E[%s]" n
            | ME Time -> "T"
            | ME This -> "X"
            | ME (Add xs) -> "A[" + (xs |> List.map (fun x -> exprCode (ME x)) |> String.concat ";") + "]"
            | ME (Multiply xs) -> "M[" + (xs |> List.map (fun x -> exprCode (ME x)) |> String.concat ";") + "]"
            | ME (Subtract (l,r)) -> sprintf "S[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME (Divide (l,r)) -> sprintf "D[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME (Power (l,r)) -> sprintf "Pow[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME (Conditional (c,t,f)) ->
                sprintf "If[%s;%s;%s]" (exprCode (ME c)) (exprCode (ME t)) (exprCode (ME f))
            | ME (Label (n,m)) -> sprintf "L[%s;%s]" n (exprCode (ME m))
            | ME (StateAt (off,nm)) -> sprintf "St[%A;%s]" off nm
            | ME Invalid -> "Invalid"
            | ME (Symbol s) -> sprintf "Sym[%s]" s
            | ME (Inverse (fn, targetSymbol, targetVal, lo, hi)) ->
                sprintf "Inv[%s;%s;%s;%s;%s]" targetSymbol
                    (exprCode (ME fn)) (exprCode (ME targetVal))
                    (exprCode (ME lo)) (exprCode (ME hi))
            | ME (GreaterThan (l,r)) -> sprintf "GT[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME (LessThan (l,r)) -> sprintf "LT[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME (EqualTo (l,r)) -> sprintf "=[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME (IsFinite e) -> sprintf "IsFin[%s]" (exprCode (ME e))
            | ME (Exponential e) -> sprintf "Exp[%s]" (exprCode (ME e))
            | ME (Logarithm e) -> sprintf "Log[%s]" (exprCode (ME e))
            | ME (Mod (x,y)) -> sprintf "Mod[%s^%s]" (exprCode (ME x)) (exprCode (ME y))


    /// Compile model expressions into functions that take
    /// changing state as Tensors.
    module ExpressionCompiler =

        open DiffSharp
        open Microsoft.FSharp.Quotations
        open Microsoft.FSharp.Linq.RuntimeHelpers
        open Bristlecone.Tensors
        open Untyped

        // ---- Index Builders ----
        let private buildIndex toKey =
            Seq.mapi (fun i k -> toKey k, i) >> Map.ofSeq

        let private paramIndex =
            Parameter.Pool.toList
            >> Seq.map fst
            >> Seq.map (fun s -> s.Value)
            >> buildIndex id

        let private envIndexFromKeys (keys: ShortCode.ShortCode list) : Map<string,int> =
            keys
            |> Seq.map (fun sc -> sc.Value)
            |> Seq.mapi (fun i name -> name, i)
            |> Map.ofSeq

        type Bool<'r> = Bool of 'r

        // ---- Generic AST traversal ----
        type Ops<'r> = {
            constVal    : float -> Expr<'r>
            parameter   : string -> Expr<'r>
            environment : string -> Expr<'r>
            timeVal     : Expr<'r>
            thisVal     : Expr<'r>
            add         : Expr<'r> list -> Expr<'r>
            sub         : Expr<'r> * Expr<'r> -> Expr<'r>
            mul         : Expr<'r> list -> Expr<'r>
            div         : Expr<'r> * Expr<'r> -> Expr<'r>
            pow         : Expr<'r> * Expr<'r> -> Expr<'r>
            log         : Expr<'r> -> Expr<'r>
            exp         : Expr<'r> -> Expr<'r>
            modulo      : Expr<'r> * Expr<'r> -> Expr<'r>
            cond        : Expr<Bool<'r>> * Expr<'r> * Expr<'r> -> Expr<'r>
            label       : string * Expr<'r> -> Expr<'r>
            stateAt     : int<Time.``time index``> * string -> Expr<'r>
            invalid     : unit -> Expr<'r> 
            // Boolean ops
            greaterThan : Expr<'r> * Expr<'r> -> Expr<Bool<'r>>
            lessThan    : Expr<'r> * Expr<'r> -> Expr<Bool<'r>>
            equalTo     : Expr<'r> * Expr<'r> -> Expr<Bool<'r>>
            isFinite    : Expr<'r> -> Expr<Bool<'r>>
            // Functions
            inverse : Expr<'r -> 'r>
              * Expr<'r>  // target
              * Expr<'r>  // lo
              * Expr<'r>  // hi
              * float * int
              -> Expr<'r>
        }

        type QuotationCache<'r, [<Measure>] 'u> =
            { TryGet : ModelExpression<'u> -> Expr<'r> option
              Store  : ModelExpression<'u> -> Expr<'r> -> unit }

        let rec buildBool<'r> buildQuotation (cache: QuotationCache<'r,'u>) (ops: Ops<'r>) (symbols: Map<string, Var>) (BE bexpr: BoolExpression) : Expr<Bool<'r>> =
            match bexpr with
            | GreaterThan (l,r) ->
                ops.greaterThan (buildQuotation cache ops symbols (ME l), buildQuotation cache ops symbols (ME r))
            | LessThan (l,r) ->
                ops.lessThan (buildQuotation cache ops symbols (ME l), buildQuotation cache ops symbols (ME r))
            | EqualTo (l,r) ->
                ops.equalTo (buildQuotation cache ops symbols (ME l), buildQuotation cache ops symbols (ME r))
            | IsFinite ex ->
                ops.isFinite (buildQuotation cache ops symbols (ME ex))
            | _ -> failwith "Unexpected boolean form"

        let rec private buildQuotationCore<'r, [<Measure>] 'u> (cache: QuotationCache<'r, 'u>) (ops: Ops<'r>) (symbols: Map<string, Var>) (expr: ModelExpression<'u>) =
            match expr with
            | ME (Constant n)        -> ops.constVal n
            | ME (Parameter n)       -> ops.parameter n
            | ME (Environment n)     -> ops.environment n
            | ME Time                -> ops.timeVal
            | ME This                -> ops.thisVal
            | ME (Add xs)            -> xs |> List.map (fun x -> buildQuotationCore cache ops symbols (ME x)) |> ops.add
            | ME (Multiply xs)       -> xs |> List.map (fun x -> buildQuotationCore cache ops symbols (ME x)) |> ops.mul
            | ME (Subtract (l,r))    -> ops.sub (buildQuotationCore cache ops symbols (ME l), buildQuotationCore cache ops symbols (ME r))
            | ME (Divide (l,r))      -> ops.div (buildQuotationCore cache ops symbols (ME l), buildQuotationCore cache ops symbols (ME r))
            | ME (Power (l,r))       -> ops.pow (buildQuotationCore cache ops symbols (ME l), buildQuotationCore cache ops symbols (ME r))
            | ME (Logarithm e)       -> ops.log (buildQuotationCore cache ops symbols (ME e))
            | ME (Exponential e)     -> ops.exp (buildQuotationCore cache ops symbols (ME e))
            | ME (Mod (l,r))         -> ops.modulo (buildQuotationCore cache ops symbols (ME l), buildQuotationCore cache ops symbols (ME r))
            | ME (Label (n,m))       -> ops.label (n, buildQuotationCore cache ops symbols (ME m))
            | ME (StateAt (off, nm)) -> ops.stateAt (off, nm)
            | ME Invalid -> ops.invalid()
            | ME (Symbol s) ->
                let v =
                    match symbols.TryFind s with
                    | Some v -> v
                    | None   -> failwithf "Unbound symbol: %s" s
                Expr.Var v |> Expr.Cast<'r>

            | ME (Conditional (c,t,f)) ->
                cache.TryGet expr
                |> Option.defaultWith (fun _ ->
                    let c' = buildBool buildQuotationCore cache ops symbols (BE c)
                    let t' = buildQuotationCore cache ops symbols (ME t)
                    let f' = buildQuotationCore cache ops symbols (ME f)
                    let q = ops.cond (c', t', f')
                    let v  = Var(sprintf "cond_%s" (ExpressionDescribe.exprCode expr), typeof<'r>)
                    let body = Expr.Var v |> Expr.Cast<'r>
                    let lifted: Expr<'r> = Expr.Let(v, q, body) |> Expr.Cast<'r>
                    cache.Store expr lifted
                    lifted )

            | ME (Inverse (fn, targetSymbol, targetVal, lo, hi)) ->
                cache.TryGet expr
                |> Option.defaultWith(fun _ ->
                    let xVar = Var(targetSymbol, typeof<'r>)
                    let symbols'  = Map.empty.Add(targetSymbol, xVar)
                    let fBody = buildQuotationCore cache ops symbols' (ME fn)
                    let fLambda = Expr.Lambda(xVar, fBody) |> Expr.Cast<'r -> 'r>
                    let targetExpr = buildQuotationCore cache ops symbols' (ME targetVal)
                    let loExpr = buildQuotationCore cache ops symbols' (ME lo)
                    let hiExpr = buildQuotationCore cache ops symbols' (ME hi)
                    let tol, maxIter = 1e-6, 50
                    let q = ops.inverse (fLambda, targetExpr, loExpr, hiExpr, tol, maxIter)
                    let v  = Var(sprintf "inverse_%s" (ExpressionDescribe.exprCode expr), typeof<'r>)
                    let body = Expr.Var v |> Expr.Cast<'r>
                    let lifted: Expr<'r> = Expr.Let(v, q, body) |> Expr.Cast<'r>
                    cache.Store expr lifted
                    lifted )

            | ME (GreaterThan _)
            | ME (LessThan _)
            | ME (IsFinite _)
            | ME (EqualTo _) -> failwith "Numeric builder received a boolean node"


        /// Builds a quotation from a model expression tree.
        let rec buildQuotation<'r, [<Measure>] 'u> (ops: Ops<'r>) (symbols: Map<string, Var>) (expr: ModelExpression<'u>) =
            let cache = { TryGet = (fun _ -> None); Store  = fun _ _ -> () }
            buildQuotationCore cache ops symbols expr

        /// Builds a quotation from a model expression tree,
        /// using an internal cache to avoid rebuilding 'heavy'
        /// nodes that have already been built.
        let buildQuotationCached<'r, [<Measure>] 'u> (ops: Ops<'r>) (symbols: Map<string, Var>) (expr: ModelExpression<'u>) : Expr<'r> =
            let cache =
                let dict = System.Collections.Generic.Dictionary<string, Expr<'r>>()
                { TryGet = fun expr ->
                    let key = ExpressionDescribe.exprCode expr
                    match dict.TryGetValue key with
                    | true, q -> Some q
                    | _ -> None
                  Store = fun expr q ->
                    let key = ExpressionDescribe.exprCode expr
                    dict.[key] <- q }
            buildQuotationCore cache ops symbols expr

        let private mkTensor (v:float) = dsharp.tensor(v, dtype = Float64)

        let invalidPenalty = 1e30

        /// If a tensor is infinite or nan, replaces its value with a high
        /// penalty value (1e30).
        let nonFiniteToPenalty (x: Tensor) =
            let nanMask = dsharp.isnan x
            let infMask = dsharp.isinf x
            let nanF = dsharp.cast(nanMask, Dtype.Float64)
            let infF = dsharp.cast(infMask, Dtype.Float64)
            let badMask = nanF + infF
            let goodMask = 1.0 - badMask
            let penalty = dsharp.tensor(invalidPenalty, dtype = Dtype.Float64)
            x * goodMask + penalty * badMask

        /// Blends the true and false cases for AD-safe operation, but eagerly
        /// evaluates both sides. If a NaN is present, it will contaminate the
        /// whole operation despite being in the unused case.
        let private blendConditional c t f =
            <@
                match %c with
                | Bool mask ->
                    let m = dsharp.cast(mask, Dtype.Float64)
                    nonFiniteToPenalty %t * m + nonFiniteToPenalty %f * (1.0 - m)
            @>

        let private tensorSum (xs: Expr<Tensor> list) : Expr<Tensor> =
            match xs with
            | []      -> failwith "Cannot add an empty list"
            | [ one ] -> one
            | _ ->
                // splice the list into an array, then stack into a tensor
                let arrExpr = Expr.NewArray(typeof<Tensor>, xs |> List.map (fun e -> e :> Expr))
                <@ dsharp.sum (dsharp.stack((%%arrExpr : Tensor[]), 0)) @>

        let private tensorProduct (arr: Tensor[]) =
            arr |> Array.reduce (fun acc t -> dsharp.mul(acc, t))

        let mul (xs: Expr<Tensor> list) : Expr<Tensor> =
            match xs with
            | []      -> <@ dsharp.tensor(1.0, dtype=Dtype.Float64) @>
            | [x]     -> x
            | _ ->
                let arrExpr = Expr.NewArray(typeof<Tensor>, xs |> List.map (fun e -> e :> Expr))
                <@ tensorProduct (%%arrExpr : Tensor[]) @>

        // let pVar      = Var("parameters", typeof<TypedTensor<Vector,``parameter``>>)
        // let statesVar = Var("states", typeof<CodedMap<TypedTensor<Scalar,ModelSystem.state>>[]>)
        // let tIdxVar   = Var("timeIndex", typeof<int>)
        // let pIndex    = paramIndex parameters
        let private tensorOps<[<Measure>] 'timeUnit> (pIndex: Map<string,int>) (eIndex: Map<string,int>)
                            (pVar: Var) (eVar: Var) (tVar: Var) (xVar: Var) = {
            constVal    = fun n -> <@ mkTensor n @>
            parameter   = fun name -> <@ (%%Expr.Var pVar : TypedTensor<Vector,``parameter``>).Value.[pIndex.[name]] @>
            environment = fun name ->
                <@
                    (%%Expr.Var eVar : CodedMap<TypedTensor<Scalar,ModelSystem.``environment``>>)
                    |> Map.tryFindBy(fun k -> k.Value = name)
                    |> fun o -> 
                        match o with
                        | Some o -> o.Value
                        | None -> failwithf "Environmental data not available: %s" name
                @>
            timeVal     = <@ (%%Expr.Var tVar : TypedTensor<Scalar,'timeUnit>).Value @>
            thisVal     = <@ (%%Expr.Var xVar : TypedTensor<Scalar,ModelSystem.state>).Value @>
            add         = tensorSum
            sub         = fun (l,r) -> <@ dsharp.sub(%l, %r) @>
            mul         = mul
            div         = fun (l,r) -> <@ dsharp.div(%l, %r) @>
            pow         = fun (l,r) -> <@ dsharp.pow(%l, %r) @>
            log         = fun e -> <@ dsharp.log %e @>
            exp         = fun e -> <@ dsharp.exp %e @>
            modulo      = fun (l,r) -> <@ %l - %r * dsharp.floor(%l / %r) @>
            cond        = fun (c,t,f) -> blendConditional c t f
            label       = fun (_,m) -> m
            stateAt     = fun _ -> failwith "State lookup not supported in equations"
            invalid     = fun () -> <@ mkTensor invalidPenalty @>
            greaterThan = fun (l,r) -> <@ dsharp.gt(%l, %r) |> Bool @>
            lessThan    = fun (l,r) -> <@ dsharp.lt(%l, %r) |> Bool @>
            equalTo     = fun (l,r) -> <@ dsharp.eq(%l, %r) |> Bool @>
            isFinite    = fun ex ->
                <@
                    let nanMask = dsharp.isnan %ex
                    let infMask = dsharp.isinf %ex
                    let bad = dsharp.gt (nanMask + infMask, mkTensor 0.)
                    let m = dsharp.cast(bad, Dtype.Float64)
                    let finiteMask = dsharp.cast(1.0f - m, Dtype.Bool)
                    Bool finiteMask
                @>
            inverse = fun (fLambda, targetExpr, loExpr, hiExpr, tol, maxIter) ->
                <@ Statistics.RootFinding.Tensor.bisect (%%fLambda : Tensor -> Tensor)
                                    %targetExpr %loExpr %hiExpr tol maxIter @>
        }

        let private tensorOpsForMeasure
            (pIndex: Map<string,int>)
            (pVar: Var) (statesVar: Var) (tIdxVar: Var) = {
            constVal    = fun n -> <@ mkTensor n @>
            parameter   = fun name -> <@ (%%Expr.Var pVar : Tensor).[pIndex.[name]] @>
            environment = fun _ -> failwith "Environment not used in measures"
            timeVal     = <@ mkTensor (float (%%Expr.Var tIdxVar : int)) @>
            thisVal     = <@ mkTensor nan @> // not used in measures
            add         = List.reduce (fun l r -> <@ dsharp.add(%l, %r) @>)
            sub         = fun (l,r) -> <@ dsharp.sub(%l, %r) @>
            mul         = List.reduce (fun l r -> <@ %l * %r @>)
            div         = fun (l,r) -> <@ dsharp.div(%l, %r) @>
            pow         = fun (l,r) -> <@ dsharp.pow(%l, %r) @>
            log         = fun e -> <@ dsharp.log %e @>
            exp         = fun e -> <@ dsharp.exp %e @>
            modulo      = fun (l,r) -> <@ %l - %r * dsharp.floor(%l / %r) @>
            cond        = fun (c,t,f) -> blendConditional c t f
            label       = fun (_,m) -> m
            invalid     = fun () -> <@ mkTensor nan @>
            stateAt     = fun (offset,name) ->
                <@
                    let states = %%Expr.Var statesVar : CodedMap<TypedTensor<Vector,ModelSystem.state>>
                    let vec = states |> Map.tryFindBy(fun n -> n.Value = name) |> Option.get
                    (Typed.itemAt ((%%Expr.Var tIdxVar : int) + Units.removeUnitFromInt offset) vec).Value
                @>
            greaterThan = fun (l,r) -> <@ dsharp.gt(%l, %r) |> Bool @>
            lessThan    = fun (l,r) -> <@ dsharp.lt(%l, %r) |> Bool @>
            equalTo     = fun (l,r) -> <@ dsharp.eq(%l, %r) |> Bool @>
            isFinite    = fun ex ->
                <@
                    let nanMask = dsharp.isnan %ex
                    let infMask = dsharp.isinf %ex
                    let bad = dsharp.gt (nanMask + infMask, mkTensor 0.)
                    let m = dsharp.cast(bad, Dtype.Float64)
                    let finiteMask = dsharp.cast(1.0f - m, Dtype.Bool)
                    Bool finiteMask
                @>
            inverse = fun (fLambda, targetExpr, loExpr, hiExpr, tol, maxIter) ->
                <@ Statistics.RootFinding.Tensor.bisect (%%fLambda : Tensor -> Tensor)
                                    %targetExpr %loExpr %hiExpr tol maxIter @>
        }

        // ---- Float ops ----
        let private floatOps (pVar: Var) (eVar: Var) (tVar: Var) (xVar: Var) = {
            constVal    = fun n -> <@ n @>
            parameter = fun name -> <@
                match (%%Expr.Var pVar : Parameter.Pool.ParameterPool)
                    |> Parameter.Pool.tryGetRealValue name with
                | Some est -> float est // strip measure for numeric ops
                | None -> failwithf "The parameter '%s' is missing" name @>
            environment = fun name -> <@
                match (%%Expr.Var eVar : CodedMap<float>)
                        |> Map.tryFindBy (fun n -> n.Value = name) with
                | Some v -> v
                | None   -> failwithf "The environment value '%s' is missing" name @>
            timeVal     = <@ (%%Expr.Var tVar : float<Time.``time index``>)
                            |> Units.removeUnitFromFloat @>
            thisVal     = <@ %%Expr.Var xVar : float @>
            add         = List.reduce (fun l r -> <@ %l + %r @>)
            sub         = fun (l,r) -> <@ %l - %r @>
            mul         = List.reduce (fun l r -> <@ %l * %r @>)
            div         = fun (l,r) -> <@ %l / %r @>
            pow         = fun (l,r) -> <@ %l ** %r @>
            log         = fun e -> <@ log %e @>
            exp         = fun e -> <@ exp %e @>
            modulo      = fun (l,r) -> <@ %l % %r @>
            label       = fun (_,m) -> m
            stateAt     = fun _ -> failwith "State lookup not supported in equations"
            invalid     = fun () -> <@ nan @>
            cond        = fun (c,t,f) -> <@ if %c = Bool 1 then %t else %f @>
            greaterThan = fun (l,r) -> <@ if %l > %r then Bool 1. else Bool 0. @>
            lessThan    = fun (l,r) -> <@ if %l < %r then Bool 1. else Bool 0. @>
            equalTo     = fun (l,r) -> <@ if %l = %r then Bool 1. else Bool 0. @>
            isFinite = fun x ->
                <@
                    let v = %x
                    let b = not (System.Double.IsNaN v || System.Double.IsInfinity v)
                    Bool (if b then 1.0 else 0.0)
                @>
            inverse = fun (fLambda, targetExpr, loExpr, hiExpr, tol, maxIter) ->
                <@ Statistics.RootFinding.bisect 1 maxIter (fun x -> (%%fLambda) x - %targetExpr)
                    %loExpr %hiExpr tol @>
        }

        // ---- Lambda helper ----
        let private toLambda4 v1 v2 v3 v4 body =
            Expr.Lambda(v1, Expr.Lambda(v2, Expr.Lambda(v3, Expr.Lambda(v4, body))))

        // ---- Public compile functions ----

        /// Compile a rate-based (e.g. ODE) model expression into an internal
        /// `RateEquation` for use in model-fitting.
        let compileRate<[<Measure>] 'timeUnit, [<Measure>] 'stateUnit>
            parameters envKeys
            (expr: ModelExpression<'stateUnit / 'timeUnit>)
            : ModelSystem.RateEquation<'timeUnit> =

            let pVar = Var("parameters", typeof<TypedTensor<Vector,``parameter``>>)
            let eVar = Var("environment", typeof<CodedMap<TypedTensor<Scalar,ModelSystem.``environment``>>>)
            let tVar = Var("time", typeof<TypedTensor<Scalar,'timeUnit>>)
            let xVar = Var("state", typeof<TypedTensor<Scalar,'stateUnit>>)

            let pIndex = paramIndex parameters
            let eIndex = envIndexFromKeys envKeys

            let core = buildQuotationCached (tensorOps pIndex eIndex pVar eVar tVar xVar) Map.empty expr

            // Convert from 'stateUnit/'timeUnit to internal state/'timeUnit
            <@ tryAsScalar<ModelSystem.state / 'timeUnit> %core |> Option.get @>
            |> toLambda4 pVar eVar tVar xVar
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<ModelSystem.RateEquation<'timeUnit>>

        let compileDiscrete<[<Measure>] 'timeUnit, [<Measure>] 'stateUnit>
            parameters envKeys
            (expr: ModelExpression<'stateUnit>)
            : ModelSystem.StateEquation<'timeUnit> =

            let pVar = Var("parameters", typeof<TypedTensor<Vector,``parameter``>>)
            let eVar = Var("environment", typeof<CodedMap<TypedTensor<Scalar,ModelSystem.``environment``>>>)
            let tVar = Var("time", typeof<TypedTensor<Scalar,'timeUnit>>)
            let xVar = Var("state", typeof<TypedTensor<Scalar,'stateUnit>>)

            let pIndex = paramIndex parameters
            let eIndex = envIndexFromKeys envKeys

            let core = buildQuotationCached (tensorOps pIndex eIndex pVar eVar tVar xVar) Map.empty expr

            <@ tryAsScalar<ModelSystem.state> %core |> Option.get @>
            |> toLambda4 pVar eVar tVar xVar
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<ModelSystem.StateEquation<'timeUnit>>

        let compileSimple<[<Measure>] 'u> (expr : ModelExpression<'u>) : float<'u> =
            let core =
                buildQuotationCached (tensorOps Map.empty Map.empty (Var("p", typeof<TypedTensor<Vector,``parameter``>>))
                    (Var("e", typeof<CodedMap<TypedTensor<Scalar,ModelSystem.``environment``>>>))
                    (Var("t", typeof<TypedTensor<Scalar,1>>))
                    (Var("x", typeof<TypedTensor<Scalar,1>>)))
                    Map.empty expr
            let q = <@ tryAsScalar<ModelSystem.state> %core |> Option.get @>
            match LeafExpressionConverter.EvaluateQuotation q with
            | :? TypedTensor<Scalar,'u> as t -> Typed.toFloatScalar t
            | _ -> failwith "Expected a scalar tensor result"


        let compileFloat expr =
            let fpVar = Var("parameters", typeof<Parameter.Pool.ParameterPool>)
            let feVar = Var("environment", typeof<CodedMap<float>>)
            let ftVar = Var("time", typeof<float<Time.``time index``>>)
            let fxVar = Var("thisValue", typeof<float>)
            buildQuotationCached (floatOps fpVar feVar ftVar fxVar) Map.empty expr
            |> toLambda4 fpVar feVar ftVar fxVar
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<Parameter.Pool.ParameterPool -> CodedMap<float> -> float<Time.``time index``> -> float -> float>

        let compileMeasure<[<Measure>] 'stateUnit> parameters (expr: ModelExpression<'stateUnit>) =
            let pVar      = Var("parameters", typeof<TypedTensor<Vector,parameter>>)
            let statesVar = Var("states", typeof<CodedMap<TypedTensor<Vector,ModelSystem.state>>>)
            let tIdxVar   = Var("timeIndex", typeof<int>)
            let pIndex    = paramIndex parameters
            let core = buildQuotationCached (tensorOpsForMeasure pIndex pVar statesVar tIdxVar) Map.empty expr
            
            <@ tryAsScalar<ModelSystem.state> %core |> Option.get @>
            |> fun body -> Expr.Lambda(pVar, Expr.Lambda(statesVar, Expr.Lambda(tIdxVar, body)))
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<ModelSystem.Measurement<'stateUnit>>



    /// Computes a `ModelExpression` given the current time, value,
    /// environment, and parameter pool.
    let compute x (t: float<Time.``time index``>) (pool: Parameter.Pool.ParameterPool) (environment: CodedMap<float>) ex : float =
        ExpressionCompiler.compileFloat ex pool environment t x

    let computeT x t pool (environment:CodedMap<Tensors.TypedTensor<Tensors.Scalar,ModelSystem.environment>>) ex =
        let eIndex, eScalar = environment |> Seq.toList |> List.map(fun kv -> kv.Key, kv.Value) |> List.unzip
        // let eVector = eScalar |> List.toArray |> Tensors.Typed.stack1D
        ExpressionCompiler.compileRate pool eIndex ex
        |> fun f -> f (Parameter.Pool.toTensorWithKeysReal pool |> snd) environment t x


    module ExpressionParser =

        open Untyped

        type Requirement =
            | ParameterRequirement of string
            | EnvironmentRequirement of string

        let rec requirements expr =
            Writer.writer {
                match expr with
                | Parameter n   -> do! Writer.tell (ParameterRequirement n)
                | Environment n -> do! Writer.tell (EnvironmentRequirement n)
                | Add xs
                | Multiply xs   ->  for x in xs do
                                        let! () = requirements x
                                        ()
                | Subtract (l,r)
                | Divide (l,r)
                | Power (l,r)
                | Mod (l,r)     -> do! requirements l
                                   do! requirements r
                | Logarithm e
                | Exponential e
                | Label(_, e)   -> do! requirements e
                | _             -> return ()
            }


    /// Scaffolds a `ModelSystem` for fitting with Bristlecone.
    module ModelBuilder =

        // A compiled-at-the-end equation, but with a closure that captures its typed expression
        type EquationThunk<[<Measure>] 'time> =
            | Discrete of ShortCode.ShortCode * (Parameter.Pool.ParameterPool -> ShortCode.ShortCode list -> ModelSystem.StateEquation<'time>)
            | Rate     of ShortCode.ShortCode * (Parameter.Pool.ParameterPool -> ShortCode.ShortCode list -> ModelSystem.RateEquation<'time>)
        
        type MeasureThunk<[<Measure>] 'u> = ShortCode.ShortCode * (Parameter.Pool.ParameterPool -> ModelSystem.Measurement<'u>)
        type LikelihoodThunk = unit -> ModelSystem.Likelihood<ModelSystem.state>

        type ModelFragment<[<Measure>] 'time> =
            | EquationFragment  of ExpressionParser.Requirement list * EquationThunk<'time>
            | ParameterFragment of Parameter.Pool.AnyParameter
            | LikelihoodFragment of LikelihoodThunk
            | MeasureFragment    of ExpressionParser.Requirement list * MeasureThunk<ModelSystem.state>

        type EquationFragment<[<Measure>] 'stateUnit, [<Measure>] 'timeUnit> =
            | DiscreteEq of ModelExpression<'stateUnit>
            | RateEq     of ModelExpression<'stateUnit / 'timeUnit>

        type ModelBuilder<[<Measure>] 'time> = private ModelBuilder of CodedMap<ModelFragment<'time>> * bool

        let create<[<Measure>] 'time> isDiscrete : unit -> ModelBuilder<'time> =
            fun () -> 
                (Map.empty<ShortCode.ShortCode, ModelFragment<'time>>, isDiscrete) |> ModelBuilder

        let internal unwrap (ModelBuilder (m,isDiscrete)) = m, isDiscrete

        module Measurement =
            let adapt<[<Measure>] 'u> (m: ModelSystem.Measurement<'u>) : ModelSystem.Measurement<ModelSystem.state> =
                fun p s i ->
                    (m p s i).Value |> Tensors.tryAsScalar<ModelSystem.state> |> Option.get

        module Likelihood =
            open ModelSystem

            let contramap<[<Measure>] 'u> (l: ModelSystem.Likelihood<'u>) : ModelSystem.Likelihood<ModelSystem.state> =
                fun param series->
                    let s =
                        series |> Map.map(fun _ v ->
                            { Expected = v.Expected |> Tensors.Typed.retype<ModelSystem.state, 'u, Tensors.Vector>
                              Observed = v.Observed |> Tensors.Typed.retype<ModelSystem.state, 'u, Tensors.Vector> }
                        )
                    l param s

        module private Add =
        
            let equationRate<[<Measure>] 'time, [<Measure>] 'state> (name: string) (expr: ModelExpression<'state/'time>) : ModelFragment<'time> =
                let sc = ShortCode.create name |> Option.get
                let (ME eUntyped) = expr
                let envReqs = ExpressionParser.requirements eUntyped |> Writer.run |> snd
                EquationFragment (envReqs, Rate (sc, fun p e -> ExpressionCompiler.compileRate<'time, 'state> p e expr))

            let equationDiscrete<[<Measure>] 'time, [<Measure>] 'state> (name: string) (expr: ModelExpression<'state>) : ModelFragment<'time> =
                let sc = ShortCode.create name |> Option.get
                let (ME eUntyped) = expr
                let envReqs = ExpressionParser.requirements eUntyped |> Writer.run |> snd
                EquationFragment (envReqs, Discrete (sc, fun p e -> ExpressionCompiler.compileDiscrete<'time, 'state> p e expr))

            let measure<[<Measure>] 'time, [<Measure>] 'u> (name: string) (expr: ModelExpression<'state>) : ModelFragment<'time> =
                let sc: ShortCode.ShortCode = ShortCode.create name |> Option.get
                let (ME eUntyped) = expr
                let envReqs = ExpressionParser.requirements eUntyped |> Writer.run |> snd
                MeasureFragment (envReqs, (sc, fun p -> ExpressionCompiler.compileMeasure<'u> p expr |> Measurement.adapt))

            let likelihood<[<Measure>] 'time, [<Measure>] 'u> (l: ModelSystem.Likelihood<'u>) : ModelFragment<'time> =
                let toInternal () = Likelihood.contramap<'u> l
                LikelihoodFragment toInternal

        let add<[<Measure>] 'time> (name: string) (frag: ModelFragment<'time>) (ModelBuilder (m,disc)) =
            let sc = ShortCode.create name |> Option.defaultWith (fun () -> failwithf "Bad short code %s" name)
            if m.ContainsKey sc then failwithf "Duplicate code [%s]" name
            ModelBuilder (m.Add(sc, frag), disc)

        let addEquationRate
            (name: ShortCode.ShortCode)
            (expr: ModelExpression<'u / 'time>)
            (mb: ModelBuilder<'time>) =
            add name.Value (Add.equationRate<'time, 'state> name.Value expr) mb

        let addEquationDiscrete (name: ShortCode.ShortCode) (expr: ModelExpression<'u>) (mb: ModelBuilder<'time>) =
            add name.Value (Add.equationDiscrete name.Value expr) mb

        let includeMeasure<[<Measure>] 'time, [<Measure>] 'u> name (m: ModelExpression<'u>) mb =
            add name (Add.measure<'time, 'u> name m) mb

        let useLikelihood<[<Measure>] 'u,[<Measure>] 'time> (l: ModelSystem.Likelihood<'u>) mb =
            add "likelihood" (Add.likelihood<'time, 'u> l) mb

        let compile builder : ModelSystem.ModelSystem<'time> =
            let m, isDiscrete = unwrap builder

            // Partition fragments
            let likelihoods =
                m |> Map.toSeq |> Seq.choose (function _, LikelihoodFragment f -> Some f | _ -> None) |> Seq.toList
            let parameters =
                m |> Map.toSeq |> Seq.choose (function c, ParameterFragment p -> Some (c, p) | _ -> None)
                |> Map.ofSeq |> Parameter.Pool.Pool
            let measures =
                m |> Map.toSeq |> Seq.choose (function c, MeasureFragment (reqs,m) -> Some (c, m) | _ -> None)
                |> Map.ofSeq
            let equations =
                m |> Map.toSeq |> Seq.choose (function c, EquationFragment (reqs,e) -> Some (c, e) | _ -> None)
                |> Map.ofSeq

            // Validate likelihoods
            let like =
                match likelihoods with
                | [f] -> f()
                | []  -> failwith "You did not specify a likelihood function."
                | _   -> failwith "Multiple likelihoods specified."

            // Collect requirements from your original untyped parser if you still rely on it.
            // Alternatively, compute envKeys directly from typed expressions at add-time and capture them into the thunk.
            let reqs =
                m |> Map.toSeq 
                |> Seq.collect (function kv -> match snd kv with | ModelFragment.EquationFragment (r,_) -> r | ModelFragment.MeasureFragment (r,_) -> r | _ -> [])

            let envKeys =
                reqs |> Seq.choose(fun r ->
                    match r with
                    | ExpressionParser.Requirement.EnvironmentRequirement e -> Some (e |> ShortCode.create |> Option.get)
                    | _ -> None)
                |> Seq.toList

            // Compile equations by invoking thunks
            let eqs =
                if isDiscrete then
                    equations
                    |> Map.map (fun _ -> function Discrete (_sc, k) -> k parameters envKeys | _ -> failwith "Expected discrete eq")
                    |> ModelSystem.DifferenceEqs
                else
                    equations
                    |> Map.map (fun _ -> function Rate (_sc, k) -> k parameters envKeys | _ -> failwith "Expected rate eq")
                    |> ModelSystem.DifferentialEqs

            match eqs with
            | ModelSystem.DifferenceEqs e -> if e.IsEmpty then failwith "No equations added. Models require at least one difference / differential equation."
            | ModelSystem.DifferentialEqs e -> if e.IsEmpty then failwith "No equations added. Models require at least one difference / differential equation."

            // Compile measures by invoking thunks
            let measures =
                measures |> Map.map (fun _ (_,m)-> m parameters)

            { NegLogLikelihood = like
              Parameters       = parameters
              EnvironmentKeys  = envKeys
              Equations        = eqs
              Measures         = measures }


    /// Terms for scaffolding a model system for use with Bristlecone.
    module Model =

        let empty<[<Measure>] 'time> : ModelBuilder.ModelBuilder<'time> = ModelBuilder.create false ()
        let discrete<[<Measure>] 'time> : ModelBuilder.ModelBuilder<'time> = ModelBuilder.create true ()

        let addRateEquation<[<Measure>] 'time, [<Measure>] 'state> (name:StateId<'state>) (expr: ModelExpression<'state/'time>) (mb: ModelBuilder.ModelBuilder<'time>) =
            let (StateIdInner name) = name
            ModelBuilder.addEquationRate name expr mb

        let addDiscreteEquation<[<Measure>] 'time, [<Measure>] 'state> (name:StateId<'state>) (expr: ModelExpression<'state>) (mb: ModelBuilder.ModelBuilder<'time>) : ModelBuilder.ModelBuilder<'time> =
            let (StateIdInner name) = name
            ModelBuilder.addEquationDiscrete name expr mb

        // Units preserved end-to-end: Parameter.create returns Parameter<'u>; we box to AnyParameter
        let estimateParameterOld
            (name: string)
            (constraintMode: Parameter.Constraint)
            (lower: float<'u>)
            (upper: float<'u>)
            (builder: ModelBuilder.ModelBuilder<'time>) =
            match Parameter.create constraintMode lower upper with
            | Some (p: Parameter.Parameter<'u>) ->
                let boxed = Parameter.Pool.boxParam<'u> name p
                ModelBuilder.add name (ModelBuilder.ParameterFragment boxed) builder
            | None -> failwithf "The bounds %A - %A cannot be used to estimate a parameter. See docs." lower upper

        let estimateParameter<[<Measure>] 'time, [<Measure>] 'u> (p:IncludedParameter<'u>) (builder: ModelBuilder.ModelBuilder<'time>) =
            let (ParamIdInner name) = p.ParamId
            let boxed = Parameter.Pool.boxParam<'u> name.Value p.Parameter
            ModelBuilder.add name.Value (ModelBuilder.ParameterFragment boxed) builder

        let internal addParameter<[<Measure>] 'time, [<Measure>] 'u> (name: string) (p:Parameter.Parameter<'u>) (builder: ModelBuilder.ModelBuilder<'time>) =
            let boxed = Parameter.Pool.boxParam<'u> name p
            ModelBuilder.add name (ModelBuilder.ParameterFragment boxed) builder

        let addMeasure<[<Measure>] 'time, [<Measure>] 'u> (name: MeasureId<'u>) (measure: ModelExpression<'u>) (builder: ModelBuilder.ModelBuilder<'time>) : ModelBuilder.ModelBuilder<'time> =
            let (MeasureIdInner name) = name
            ModelBuilder.includeMeasure name.Value measure builder

        let useLikelihoodFunction likelihoodFn builder =
            ModelBuilder.add "likelihood" (ModelBuilder.LikelihoodFragment (fun () -> likelihoodFn)) builder

        let compile = ModelBuilder.compile


    module ModelSystemDsl =

        // Structural flags
        type Missing = Missing
        type Present = Present

        /// Builder state tracks:
        /// - 'time: unit of the time index (e.g., days)
        /// - 'Kind: Discrete | Continuous (marker type)
        /// - 'HasEq / 'HasLike: compile-time presence flags
        type ModelBuilderState<[<Measure>] 'time, 'HasEq, 'HasLike> =
            private { Inner : ModelBuilder.ModelBuilder<'time> }

        let emptyState<[<Measure>] 'time> isDiscrete
            : ModelBuilderState<'time, Missing, Missing> =
            { Inner = ModelBuilder.create isDiscrete () }

        let addRateEq
            (sid: StateId<'u>) (expr: ModelExpression<'stateUnit / 'timeUnit>)
            (mb: ModelBuilderState<'time, 'E, 'L>) =
            let (StateIdInner sc) = sid
            { mb with Inner = ModelBuilder.addEquationRate sc expr mb.Inner }

        let addDiscreteEq
            (sid: StateId<'u>) (expr: ModelExpression<'u>)
            (mb: ModelBuilderState<'time, 'E, 'L>) =
            let (StateIdInner sc) = sid
            { mb with Inner = ModelBuilder.addEquationDiscrete sc expr mb.Inner }

        type ModelSystemBuilder<[<Measure>] 'time>(isDiscrete) =
            member _.Yield(_) = emptyState<'time> isDiscrete
            member _.Delay(f) = f()

            [<CustomOperation("parameter")>]
            member _.Parameter<[<Measure>] 'u, 'E, 'L>
                (state: ModelBuilderState<'time, 'E, 'L>,
                configuredParameter: IncludedParameter<'u>) =
                let (ParamIdInner sc) = configuredParameter.ParamId
                { Inner = Model.addParameter sc.Value configuredParameter.Parameter state.Inner }

            [<CustomOperation("equationDiscrete")>]
            member _.EquationDiscrete(state: ModelBuilderState<'time,'E,'L>, sid: StateId<'u>, expr: ModelExpression<'u>)
                : ModelBuilderState<'time, Present, 'L> =
                let (StateIdInner sc) = sid
                { Inner = ModelBuilder.addEquationDiscrete sc expr state.Inner }

            [<CustomOperation("equationRate")>]
            member _.EquationRate(state: ModelBuilderState<'time,'E,'L>, sid: StateId<'u>, expr: ModelExpression<'u/'time>)
                : ModelBuilderState<'time, Present, 'L> =
                let (StateIdInner sc) = sid
                { Inner = ModelBuilder.addEquationRate sc expr state.Inner }

            [<CustomOperation("measure")>]
            member _.Measure<[<Measure>] 'u, 'E, 'L>
                (state: ModelBuilderState<'time, 'E, 'L>,
                mid: MeasureId<'u>, data) =
                { Inner = Model.addMeasure mid data state.Inner }

            /// Add exactly one likelihood function.
            [<CustomOperation("likelihood")>]
            member _.Likelihood<'E>
                (state: ModelBuilderState<'time, 'E, Missing>, fn)
                : ModelBuilderState<'time, 'E, Present> =
                { Inner = Model.useLikelihoodFunction fn state.Inner }

            member _.Run(state: ModelBuilderState<'time, Present, Present>) =
                ModelBuilder.compile state.Inner


    let discreteModel<[<Measure>] 'time> : ModelSystemDsl.ModelSystemBuilder<'time> =
        ModelSystemDsl.ModelSystemBuilder true

    let continuousModel<[<Measure>] 'time> : ModelSystemDsl.ModelSystemBuilder<'time> =
        ModelSystemDsl.ModelSystemBuilder false


    /// Terms for designing tests for model systems.
    module Test =

        let defaultSettings = Bristlecone.Test.defaultSettings

        /// If the start value has already been set, it will be overwritten with the new value.
        let withStartValue code value (settings: Bristlecone.Test.TestSettings<'state, _, _, _>) =
            match ShortCode.create code with
            | Some c ->
                { settings with
                    StartValues = settings.StartValues |> Map.add c value }
            | None -> failwithf "'%s' is not a valid short code" code

        let run settings = Bristlecone.testModel


    module Components =

        /// A component with its own parameters and an expression generator
        type SubComponent<'a> =
            { Label      : string
              Parameters : CodedMap<Parameter.Pool.AnyParameter>
              Expr       : 'a }

        type ModelComponent<'a> = {
            Label: string
            Implementations: SubComponent<'a> list
        }

        /// Creates a nested component that can be inserted into a base model.
        let subComponent name transform = { Label = name; Parameters = Map.empty; Expr = transform }

        let modelComponent name list = { Label = name; Implementations = list }

        let estimateParameter<[<Measure>] 'u, 'a> (p: IncludedParameter<'u>) (comp: SubComponent<'a>) =
            let (ParamIdInner name) = p.ParamId
            let boxed = Parameter.Pool.boxParam<'u> name.Value p.Parameter
            { comp with Parameters = comp.Parameters |> Map.add name boxed }



        type SubComponentState<[<Measure>] 'u> =
            { Label     : string
              Expr      : ModelExpression<'u> option
              Estimates : (string * Parameter.Constraint * float * float) list }

        type ComponentState<[<Measure>] 'u> =
            { Options : SubComponentState<'u> list }


        type SubComponentBuilder<[<Measure>] 'u>(label: string) =

            member _.Yield(_) =
                { Label = label; Expr = None; Estimates = [] }

            member _.Zero() =
                { Label = label; Expr = None; Estimates = [] }

            member _.Bind(p: Parameter.Parameter<'p>, cont: Parameter.Parameter<'p> -> SubComponentState<'u>) =
                cont p

            member _.Parameter<[<Measure>] 'p>
                (name: string, con, lower: float<'p>, upper: float<'p>, state: SubComponentState<'u>) =
                let p = parameter name con lower upper
                { state with Estimates = (name, con, float lower, float upper) :: state.Estimates }, p

            member _.Expression(expr: ModelExpression<'u>, state: SubComponentState<'u>) =
                { state with Expr = Some expr }

            member _.Run(state: SubComponentState<'u>) =
                match state.Expr with
                | Some _ -> state
                | None   -> failwithf "Subcomponent '%s' has no expression" state.Label


        // type ComponentBuilder<[<Measure>] 'u>(name: string) =

        //     member _.Yield(_) =
        //         { Options = [] }

        //     member _.Yield(sc: SubComponentState<'u>) =
        //         { Options = [sc] }

        //     member _.Zero() =
        //         { Options = [] }

        //     member _.Combine(state: ComponentState<'u>, sc: SubComponentState<'u>) =
        //         { state with Options = sc :: state.Options }

        //     member _.Delay(f: unit -> ComponentState<'u>) = f()

        //     member _.Run(state: ComponentState<'u>) =
        //         let options =
        //             state.Options
        //             |> List.rev
        //             |> List.map (fun sc ->
        //                 let expr = sc.Expr.Value
        //                 (sc.Label, expr)
        //                 |> fun acc ->
        //                     sc.Estimates
        //                     |> List.fold (fun acc (pName, constr, lo, hi) ->
        //                         estimateParameter pName
        //                     ) acc
        //             )
        //         modelComponent<'u> name options


    // let subcomponent<[<Measure>] 'u> label (block: Components.SubComponentBuilder<'u> -> Components.SubComponentState<'u>) =
    //     block (Components.SubComponentBuilder label)
    // let modelComponent label : Components.ComponentBuilder<'u> = Components.ComponentBuilder(label)

    // let nLimitation =
    //     modelComponent "N-limitation" {
    //         subcomponent "Linear" {
    //             let! a = parameter "a" notNegative 0.100 0.400
    //             expression (
    //                 ModelComponents.GrowthLimitation.linear
    //                     (P a / Constant 1000.)
    //                     (Constant 5.00)
    //             )
    //         }
    //     }


    /// <summary>Types to represent a hypothesis, given that a hypothesis
    /// is a model system that contains some alternate formulations of certain
    /// components.</summary>
    [<RequireQualifiedAccess>]
    module Hypotheses =

        // Helper: truncate first N letters of each word and uppercase
        let internal nFirstLetters n (s: string) =
            s.Split(' ')
            |> Seq.map (Seq.truncate n >> Seq.toArray >> System.String)
            |> String.concat ""
            |> fun s -> s.ToUpper()

        /// <summary>Represents the name of a swappable component in a model
        /// and the name of its implementation in a specific case.</summary>
        type ComponentName =
            { Component: string
              Implementation: string }
                /// <summary>A short code that may be used to indicate a particular
                /// model component by its current implementation.</summary>
                /// <returns>A string in the format XX_XXX with the first part
                /// indicating the model component and the second part the implementation.</returns>
                member this.Reference =
                    $"{nFirstLetters 2 this.Component}_{nFirstLetters 3 this.Implementation}"

        /// <summary>A hypothesis consists of a model system and the names of the
        /// swappable components within it, alongside the name of their current implementation.</summary>
        type Hypothesis<[<Measure>] 'timeIndex> =
            | Hypothesis of ModelSystem.ModelSystem<'timeIndex> * list<ComponentName>

            member private this.Unwrap = let (Hypothesis(m, comps)) = this in m, comps
            /// <summary>Compiles a reference code that may be used to identify (although not necessarily uniquely) this hypothesis</summary>
            /// <returns>A string in the format XX_XXX_YY_YYY... where XX_XXX is a singe component with XX the component and XXX the implementation.</returns>

            member this.ReferenceCode =
                this.Unwrap |> snd |> List.map (fun c -> c.Reference) |> String.concat "_"

            member this.Components = this.Unwrap |> snd
            member this.Model = this.Unwrap |> fst
            
        // Start the pipeline: lift the base model into a builder
        let createFromModel (baseModel : 'a -> 'rest)
            : Writer.Writer<'a -> 'rest, ComponentName * CodedMap<Parameter.Pool.AnyParameter>> list =
            [ Writer.return' (baseModel) ]

        /// 
        let apply (comp : Components.ModelComponent<'a>) (builders : Writer.Writer<'a -> 'rest, ComponentName * CodedMap<Parameter.Pool.AnyParameter>> list)
            : Writer.Writer<'rest, ComponentName * CodedMap<Parameter.Pool.AnyParameter>> list =
            [ for sc in comp.Implementations do
                for (Writer.AWriter (f, logs)) in builders do
                    let entry = ({ Component = comp.Label; Implementation = sc.Label }, sc.Parameters)
                    yield Writer.AWriter (f sc.Expr, logs @ [entry]) ]

        /// Compile: run the writer(s), add parameters into the model builder, and wrap in Hypothesis
        let compile (builders : Writer.Writer<ModelBuilder.ModelBuilder<'u>, ComponentName * CodedMap<Parameter.Pool.AnyParameter>> list)
            : Hypothesis<'u> list =
            builders
            |> List.map (fun (Writer.AWriter (mb, logs)) ->
                let withParams =
                    logs
                    |> List.collect (snd >> Map.toList)
                    |> List.fold (fun mb (name,p) ->
                        mb |> ModelBuilder.add name.Value (ModelBuilder.ParameterFragment p)
                    ) mb
                Hypothesis(Model.compile withParams, logs |> List.map fst))



        // type Hypotheses<'subject, 'hypothesis> =
        //     | Hypotheses of Hypothesis<'subject, 'hypothesis> seq

        // open Bristlecone.ModelSelection

        // let private unwrap (ResultSetMany m) = m

        // let many sets = ResultSetMany sets

        // let subject s sets =
        //     sets
        //     |> unwrap
        //     |> Seq.filter(fun s -> s.Subject = s)

        // /// <summary>Map a function over results on a per-subject and per-hypothesis basis</summary>
        // /// <param name="fn">A function to map over the subject-hypothesis groups</param>
        // /// <param name="sets">A result set (many)</param>
        // /// <returns>A transformed results set</returns>
        // let map fn (sets:ResultSetMany<'subject, 'hypothesis>) =
        //     sets
        //     |> unwrap
        //     |> Seq.groupBy(fun s -> s.Subject, s.Hypothesis)
        //     |> Seq.map(fun (s, h) -> fn s h)
        //     |> ResultSetMany
