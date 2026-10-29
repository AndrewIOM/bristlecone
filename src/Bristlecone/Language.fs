namespace Bristlecone

/// An F# Domain Specific Language (DSL) for scripting with
/// Bristlecone.
module Language =

    [<NoEquality; NoComparison>]
    type StateId<[<Measure>] 'u> =
        private
        | StateIdInner of ShortCode.ShortCode

        member this.Code = this |> fun (StateIdInner c) -> c

    [<NoEquality; NoComparison>]
    type MeasureId<[<Measure>] 'u> =
        private
        | MeasureIdInner of ShortCode.ShortCode

        member this.Code = this |> fun (MeasureIdInner c) -> c

    [<NoEquality; NoComparison>]
    type ParamId<[<Measure>] 'u> =
        private
        | ParamIdInner of ShortCode.ShortCode

        member this.Inner = this |> fun (ParamIdInner this) -> this

    type IncludedParameter<[<Measure>] 'u> =
        { ParamId: ParamId<'u>
          Parameter: Parameter.Parameter<'u> }

    /// Define an estimatable parameter for a Bristlecone model.
    let parameter<[<Measure>] 'u> code con lower upper : IncludedParameter<'u> =
        match Parameter.create con lower upper, ShortCode.create code with
        | Some p, Some c ->
            { ParamId = ParamIdInner c
              Parameter = p }
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

    let measure<[<Measure>] 'u> name : MeasureId<'u> =
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

    let Yearly n = makeResolution Time.Resolution.Years n
    let Annually n = makeResolution Time.Resolution.Years n
    let Monthly n = makeResolution Time.Resolution.Months n
    let Daily n = makeResolution Time.Resolution.Days n

    let NoConstraints = Parameter.Constraint.Unconstrained
    let Positive = Parameter.Constraint.PositiveOnly

    let Bounded lo hi =
        if lo = hi then
            failwithf "Parameter bounds were equal: %f -> %f" lo hi

        let lo, hi = min lo hi, max lo hi
        Parameter.Constraint.Bounded(lo, hi)

    module Require =

        type ObsForLikelihood<[<Measure>] 's> =
            | StateObs of StateId<'s>
            | MeasureObs of MeasureId<'s>

        let state (s: StateId<'u>) = StateObs s

        let measure (m: MeasureId<'u>) = MeasureObs m

    let lookup name (map: CodedMap<float>) =
        match map |> Map.tryFindBy (fun k -> k.Value = name) with
        | Some k -> k
        | None -> invalidOp (sprintf "Could not find %s in the map" name)


    type Parameter.Pool.ParameterPool with

        /// Retrieve an parameter value from the parameter pool
        /// in its original units of measure.
        member pool.TryGet<[<Measure>] 'u>(included: IncludedParameter<'u>) : float<'u> option =
            pool |> Parameter.Pool.tryGetRealValue<'u> included.ParamId.Inner.Value

        /// Retrieve an parameter value from the parameter pool
        /// in its original units of measure. Fails if parameter
        /// doesn't exist or is not estimated.
        member pool.Get<[<Measure>] 'u>(included: IncludedParameter<'u>) : float<'u> =
            included |> pool.TryGet |> Option.get


    module Untyped =

        /// A model or model fragment that can be interpreted to a mathematical
        /// expression by Bristlecone.
        type ModelExpressionUntyped =
            | This
            | Time
            | Environment of string
            | State of string
            | StateAt of offset: int<Time.``time index``> * string
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
            | Conditional of
                cond: ModelExpressionUntyped *
                ifTrue: ModelExpressionUntyped *
                ifFalse: ModelExpressionUntyped
            | Label of string * ModelExpressionUntyped
            | Invalid
            // Comparisons:
            | GreaterThan of ModelExpressionUntyped * ModelExpressionUntyped
            | LessThan of ModelExpressionUntyped * ModelExpressionUntyped
            | EqualTo of ModelExpressionUntyped * ModelExpressionUntyped
            | IsFinite of ModelExpressionUntyped
            // Functions:
            | Symbol of string
            | Inverse of
                expr: ModelExpressionUntyped *
                symbol: string *
                targetValue: ModelExpressionUntyped *
                lo: ModelExpressionUntyped *
                hi: ModelExpressionUntyped


    [<NoEquality; NoComparison>]
    type BoolExpression = private BE of Untyped.ModelExpressionUntyped

    [<NoEquality; NoComparison>]
    type ModelExpression<[<Measure>] 'u> =
        private
        | ME of Untyped.ModelExpressionUntyped

        static member (+)(ME a: ModelExpression<'u>, ME b: ModelExpression<'u>) : ModelExpression<'u> =
            ME(Untyped.Add [ a; b ])

        static member (-)(ME a: ModelExpression<'u>, ME b: ModelExpression<'u>) : ModelExpression<'u> =
            ME(Untyped.Subtract(a, b))

        static member (*)(ME a: ModelExpression<'u1>, ME b: ModelExpression<'u2>) : ModelExpression<'u1 * 'u2> =
            ME(Untyped.Multiply [ a; b ])

        static member (/)(ME a: ModelExpression<'u1>, ME b: ModelExpression<'u2>) : ModelExpression<'u1 / 'u2> =
            ME(Untyped.Divide(a, b))

        static member (%)(ME a: ModelExpression<'u>, ME m: ModelExpression<'u>) : ModelExpression<'u> =
            ME(Untyped.Mod(a, m))

        static member (~-)(ME a: ModelExpression<'u>) : ModelExpression<'u> =
            ME(Untyped.Multiply [ a; Untyped.Constant -1.0 ])

        static member (.>)(ME a: ModelExpression<'u>, ME b: ModelExpression<'u>) : BoolExpression =
            BE(Untyped.GreaterThan(a, b))

        static member (.<)(ME a: ModelExpression<'u>, ME b: ModelExpression<'u>) : BoolExpression =
            BE(Untyped.LessThan(a, b))

        static member (=.)(ME a: ModelExpression<'u>, ME b: ModelExpression<'u>) : BoolExpression =
            BE(Untyped.EqualTo(a, b))

        static member Pow(ME a: ModelExpression<'u>, ME p: ModelExpression<1>) : ModelExpression<'u> =
            ME(Untyped.Power(a, p))

        static member Log(ME a: ModelExpression<1>) : ModelExpression<1> = ME(Untyped.Logarithm a)

        static member Exp(ME a: ModelExpression<1>) : ModelExpression<1> = ME(Untyped.Exponential a)

        static member StateAt(offset: int<Time.``time index``>, sid: StateId<'u>) : ModelExpression<'u> =
            let (StateIdInner sc) = sid
            ME(Untyped.StateAt(offset, sc.Value))

        static member State(sid: StateId<'u>) : ModelExpression<'u> =
            let (StateIdInner sc) = sid
            ME(Untyped.State sc.Value)

        static member Measure(sid: MeasureId<'u>) : ModelExpression<'u> =
            let (MeasureIdInner sc) = sid
            ME(Untyped.State sc.Value)

        static member Parameter(pid: IncludedParameter<'u>) : ModelExpression<'u> =
            let (ParamIdInner name) = pid.ParamId
            ME(Untyped.Parameter name.Value)

        static member Environment(sid: StateId<'u>) : ModelExpression<'u> =
            let (StateIdInner name) = sid
            ME(Untyped.Environment name.Value)

    let Constant (x: float<'u>) : ModelExpression<'u> = ME(Untyped.Constant(float x))
    let P<[<Measure>] 'u> name : ModelExpression<'u> = ModelExpression.Parameter name
    let This<[<Measure>] 'u> : ModelExpression<'u> = ME Untyped.This
    let State<[<Measure>] 'u> sId : ModelExpression<'u> = ModelExpression.State sId
    let StateAt (offset: int<Time.``time index``>, sId) : ModelExpression<'u> = ModelExpression.StateAt(offset, sId)
    let Time<[<Measure>] 't> : ModelExpression<'t> = ME Untyped.Time
    let Environment<[<Measure>] 'u> sid : ModelExpression<'u> = ModelExpression.Environment sid
    let Invalid<[<Measure>] 'u> : ModelExpression<'u> = ME Untyped.Invalid
    let Measure<[<Measure>] 'u> sId : ModelExpression<'u> = ModelExpression.Measure sId

    /// Operations for boolean model expressions.
    module Bool =

        let isFinite (ME a: ModelExpression<'u>) : BoolExpression = BE(Untyped.IsFinite a)


    /// For conditional statements, all three branches must have the same unit.
    let Conditional<[<Measure>] 'u>
        (cond: BoolExpression)
        (ifTrue: ModelExpression<'u>)
        (ifFalse: ModelExpression<'u>)
        : ModelExpression<'u> =
        let (BE c) = cond
        let (ME t) = ifTrue
        let (ME f) = ifFalse
        ME(Untyped.Conditional(c, t, f))

    let Inverse<[<Measure>] 'u, [<Measure>] 'v>
        (forwardFn: ModelExpression<'v> -> ModelExpression<'u>)
        (target: ModelExpression<'u>)
        (lo: ModelExpression<'v>)
        (hi: ModelExpression<'v>)
        : ModelExpression<'v> =
        let symName = "invVar"
        let arg = ME(Untyped.Symbol symName)
        let (ME targetExpr) = target
        let (ME fExpr) = forwardFn arg
        let ME lo, ME hi = lo, hi
        ME(Untyped.Inverse(fExpr, symName, targetExpr, lo, hi))

    let Label<[<Measure>] 'u> (label: string) (expr: ModelExpression<'u>) : ModelExpression<'u> =
        let (ME e) = expr
        ME(Untyped.Label(label, e))

    // TODO Units for power.
    let Power<[<Measure>] 'u> (baseExpr: ModelExpression<'u>) (expExpr: ModelExpression<1>) : ModelExpression<'u> =
        let (ME b) = baseExpr
        let (ME e) = expExpr
        ME(Untyped.Power(b, e))

    /// The natural logarithm of an expression.
    let Logarithm (expr: ModelExpression<1>) : ModelExpression<1> =
        let (ME e) = expr
        ME(Untyped.Logarithm e)

    // Exponential: exp(x) is dimensionless if x is dimensionless
    let Exponential (expr: ModelExpression<1>) : ModelExpression<1> =
        let (ME e) = expr
        ME(Untyped.Exponential e)


    module ExpressionDescribe =

        open Untyped

        /// Outputs a code-based representation of a model.
        let rec exprCode (expr: ModelExpression<'u>) : string =
            match expr with
            | ME(Constant n) -> sprintf "C[%A]" n
            | ME(Parameter n) -> sprintf "P[%s]" n
            | ME(Environment n) -> sprintf "E[%s]" n
            | ME(State nm) -> sprintf "St[%s]" nm
            | ME(StateAt(off, nm)) -> sprintf "StOff[%A;%s]" off nm
            | ME Time -> "T"
            | ME This -> "X"
            | ME(Add xs) -> "A[" + (xs |> List.map (fun x -> exprCode (ME x)) |> String.concat ";") + "]"
            | ME(Multiply xs) -> "M[" + (xs |> List.map (fun x -> exprCode (ME x)) |> String.concat ";") + "]"
            | ME(Subtract(l, r)) -> sprintf "S[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME(Divide(l, r)) -> sprintf "D[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME(Power(l, r)) -> sprintf "Pow[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME(Conditional(c, t, f)) -> sprintf "If[%s;%s;%s]" (exprCode (ME c)) (exprCode (ME t)) (exprCode (ME f))
            | ME(Label(n, m)) -> sprintf "L[%s;%s]" n (exprCode (ME m))
            | ME Invalid -> "Invalid"
            | ME(Symbol s) -> sprintf "Sym[%s]" s
            | ME(Inverse(fn, targetSymbol, targetVal, lo, hi)) ->
                sprintf
                    "Inv[%s;%s;%s;%s;%s]"
                    targetSymbol
                    (exprCode (ME fn))
                    (exprCode (ME targetVal))
                    (exprCode (ME lo))
                    (exprCode (ME hi))
            | ME(GreaterThan(l, r)) -> sprintf "GT[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME(LessThan(l, r)) -> sprintf "LT[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME(EqualTo(l, r)) -> sprintf "=[%s;%s]" (exprCode (ME l)) (exprCode (ME r))
            | ME(IsFinite e) -> sprintf "IsFin[%s]" (exprCode (ME e))
            | ME(Exponential e) -> sprintf "Exp[%s]" (exprCode (ME e))
            | ME(Logarithm e) -> sprintf "Log[%s]" (exprCode (ME e))
            | ME(Mod(x, y)) -> sprintf "Mod[%s^%s]" (exprCode (ME x)) (exprCode (ME y))


    /// Compile model expressions into functions that take
    /// changing state as Tensors.
    module ExpressionCompiler =

        open Microsoft.FSharp.Quotations
        open Microsoft.FSharp.Linq.RuntimeHelpers
        open Untyped
        open Bristlecone.Numerics
        open Bristlecone.Numerics.Typed

        let private buildIndex toKey =
            Seq.mapi (fun i k -> toKey k, i) >> Map.ofSeq

        let private paramIndex =
            Parameter.Pool.toList
            >> Seq.map fst
            >> Seq.map (fun s -> s.Value)
            >> buildIndex id

        let private envIndexFromKeys (keys: ShortCode.ShortCode list) : Map<string, int> =
            keys
            |> Seq.map (fun sc -> sc.Value)
            |> Seq.mapi (fun i name -> name, i)
            |> Map.ofSeq


        module Caching =

            type QuotationCache<'r, [<Measure>] 'u> =
                { TryGet: ModelExpression<'u> -> Expr<'r> option
                  Store: ModelExpression<'u> -> Expr<'r> -> Expr<'r> }

            module Policies =
                let noCaching = fun _ -> false
                let cacheEverything = fun _ -> true

                let cacheHeavy =
                    function
                    | ME(Conditional _) -> true
                    | ME(Inverse _) -> true
                    | ME(Power _) -> true
                    | ME(Multiply xs) when xs.Length > 2 -> true
                    | _ -> false

            let makeBindingCache<'r, [<Measure>] 'u>
                ()
                : QuotationCache<'r, 'u> * System.Collections.Generic.List<Var * Expr<'r>> =
                let vars = System.Collections.Generic.Dictionary<string, Var>()
                let bindings = System.Collections.Generic.List<Var * Expr<'r>>()

                let cache =
                    { TryGet =
                        fun expr ->
                            let key = ExpressionDescribe.exprCode expr

                            match vars.TryGetValue key with
                            | true, v -> Some(Expr.Var v |> Expr.Cast<'r>)
                            | _ -> None
                      Store =
                        fun expr rhs ->
                            let key = ExpressionDescribe.exprCode expr
                            let v = Var(sprintf "tmp_%s" key, typeof<'r>)
                            vars.[key] <- v
                            bindings.Add(v, rhs)
                            Expr.Var v |> Expr.Cast<'r> }

                cache, bindings

            /// Use caching if shouldCache returns true for the given expression.
            let apply cache shouldCache (expr: ModelExpression<'u>) (rhs: unit -> Expr<'r>) =
                if shouldCache expr then
                    match cache.TryGet expr with
                    | Some vref -> vref
                    | None -> cache.Store expr (rhs ())
                else
                    rhs ()

        /// A marker to know if a bitmask, which
        /// represents a boolean value as 0 or 1.
        type Bitmask<'S> = Bitmask of 'S

        let rec buildBool<'S,'C>
            (buildQuotation: (ModelExpression<'u> -> Expr<'C>))
            (backend: NumericBackend<'S,'C>)
            (symbols: Map<string, Var>)
            (BE bexpr: BoolExpression)
            : Expr<Bitmask<'C>> =
            match bexpr with
            | GreaterThan(l, r) ->
                let l, r = buildQuotation (ME l), buildQuotation (ME r)
                <@ backend.gt %l %r |> Bitmask @>
            | LessThan(l, r) ->
                let l, r = buildQuotation (ME l), buildQuotation (ME r)
                <@ backend.lt %l %r |> Bitmask @>
            | EqualTo(l, r) ->
                let l, r = buildQuotation (ME l), buildQuotation (ME r)
                <@ backend.eq %l %r |> Bitmask @>
            | IsFinite ex ->
                let ex = buildQuotation (ME ex)
                <@ backend.isInf %ex |> Bitmask @> // TODO Also is nan
             // ops.isFinite (buildQuotation (ME ex))
            | _ -> failwith "Unexpected boolean form"

        /// Different branching techniques for AD-based computation.
        module internal Branching =

            /// Blends the true and false cases for AD-safe operation, but eagerly
            /// evaluates both sides. If a NaN is present, it will contaminate the
            /// whole operation despite being in the unused case.
            let blendConditional (backend: NumericBackend<'S,'C>) (c:Expr<Bitmask<'C>>) t f =
                <@
                    let (Bitmask mask) = %c
                    let batchLength = backend.length (%t)
                    let oneVec = backend.promote backend.atomic.constants.one batchLength
                    let invMask = backend.sub oneVec mask
                    backend.add (backend.mul mask %t) (backend.mul invMask %f)
                @>

            /// For situations where we don't need gradients, use F# control flow.
            /// Only works for simple scalar cases.
            let simple (backend: NumericBackend<'S,'S>) (c:Expr<Bitmask<'S>>) t f =
                <@
                    let (Bitmask mask) = %c
                    if mask = backend.atomic.constants.one then %t else %f
                @>


        /// Plugs in context-dependent values into
        /// the model compilation. Allows parameter values
        /// to be vectorised.
        type OpsContext<'S,[<Measure>] 'u> = {
            GetParameter: string -> Expr<'S>
            // ParameterVar : Var
            ParameterIndex: Map<string, int>
            GetState : string -> int<Time.``time index``> -> Expr<'S>
            Environment : string -> Expr<'S>
            TimeVal : Expr<'S>
            ThisVal : Expr<'S>
        }

        let private listSum backend (xs: Expr<'S> list) : Expr<'S> =
            match xs with
            | [] -> failwith "Cannot add an empty list"
            | x :: rest -> rest |> List.fold (fun acc e -> <@ backend.add %acc %e @>) x

        let private listProduct backend (xs: Expr<'S> list) : Expr<'S> =
            match xs with
            | [] -> failwith "Cannot multiply an empty list"
            | x :: rest -> rest |> List.fold (fun acc e -> <@ backend.mul %acc %e @>) x

        let inverseFn (backend: NumericBackend<'S,'C>) (fLambda: Expr<'C -> 'C>) (targetExpr: Expr<'C>) (loExpr: Expr<'C>) (hiExpr: Expr<'C>) (tolExpr: 'C) (maxIter: int) : Expr<'C> =
            <@
                Statistics.RootFinding.bisect
                    backend
                    %fLambda
                    %targetExpr
                    %loExpr
                    %hiExpr
                    tolExpr
                    maxIter
            @>

        /// Compiles a model expression into a quoted function, which
        /// runs on the dimensions of the container type 'C. For example,
        /// if 'C is a vector, the function runs vectorised within the selected
        /// backend.
        let rec private buildQuotationCore<[<Measure>] 'u, 'S, 'C>
            (shouldCache: ModelExpression<'u> -> bool)
            (cache: Caching.QuotationCache<'C, 'u>)
            (backend: NumericBackend<'S, 'C>)
            (scalarBackend: NumericBackend<'S, 'S>)
            inverseFn
            requiresGradients
            (context:OpsContext<'S,'u>)
            (symbols: Map<string, Var>)
            batchLength
            (expr: ModelExpression<'u>)
            : Expr<'C>
            =
            let build : ModelExpression<'u> -> Expr<'C> = buildQuotationCore shouldCache cache backend scalarBackend inverseFn requiresGradients context symbols batchLength
            let withCache expr mk = Caching.apply cache shouldCache expr mk

            let promote (scalarExpr: Expr<'S>) : Expr<'C> = <@ backend.promote %scalarExpr batchLength @>

            match expr with
            | ME(Constant n) ->
                let staticVal : 'S = backend.atomic.fromFloatS n
                <@ staticVal @> |> promote

            | ME(Environment n) -> context.Environment n |> promote
            | ME(State nm) -> context.GetState nm 0<Time.``time index``> |> promote
            | ME Time -> context.TimeVal |> promote
            | ME This -> context.ThisVal |> promote
            | ME Invalid -> <@ backend.atomic.constants.invalid @> |> promote

            | ME(Parameter name) ->
                let scalarParamExpr = context.GetParameter name
                <@ %scalarParamExpr @> |> promote

            | ME(Add xs) -> withCache expr (fun () -> xs |> List.map (fun x -> build (ME x)) |> listSum backend)

            | ME(Multiply xs) -> withCache expr (fun () -> xs |> List.map (fun x -> build (ME x)) |> listProduct backend)

            | ME(Subtract(l, r)) -> withCache expr (fun () -> (build (ME l), build (ME r)) |> fun (l,r) -> <@ backend.sub %l %r @>)

            | ME(Divide(l, r)) -> withCache expr (fun () -> (build (ME l), build (ME r)) |> fun (l,r) -> <@ backend.div %l %r @>)

            // | ME(Power(l, r)) -> withCache expr (fun () -> (build (ME l), build (ME r)) |> fun (l,r) -> <@ backend.pow %l %r @>)

            | ME(Logarithm e) -> withCache expr (fun () -> (build (ME e)) |> fun e -> <@ backend.log %e @>)

            | ME(Exponential e) -> withCache expr (fun () -> (build (ME e)) |> fun e -> <@ backend.exp %e @>)

            | ME(Mod(l, r)) -> withCache expr (fun () -> (build (ME l), build (ME r)) |> fun (l,r) -> <@ backend.sub %l (backend.mul %r (backend.floor (backend.div %l %r))) @>)

            | ME(Label(n, m)) -> (n, build (ME m)) |> fun (_,m) -> m
            | ME(StateAt(off, n)) -> context.GetState n off |> promote

            | ME(Symbol s) ->
                let v =
                    match symbols.TryFind s with
                    | Some v -> v
                    | None -> failwithf "Unbound symbol: %s" s

                Expr.Var v |> Expr.Cast

            | ME(Conditional(c, t, f)) ->
                withCache expr (fun () ->
                    let c' = buildBool build backend symbols (BE c)
                    let t' = build (ME t)
                    let f' = build (ME f)
                    Branching.blendConditional backend c' t' f'
                    // if requiresGradients then Branching.blendConditional backend c' t' f'
                    // else Branching.simple backend c' t' f'
                    // TODO Simple case can't work on vectorised code.
                )

            | ME(Inverse(fn, targetSymbol, targetVal, lo, hi)) ->
                withCache expr (fun () ->
                    let xVar = Var(targetSymbol, typeof<'C>)
                    let symbolsForFn = Map.empty.Add(targetSymbol, xVar)

                    // Build x and one = x/x for AD lifting
                    let xExpr: Expr<'C> = Expr.Var xVar |> Expr.Cast<'C>
                    let oneVar = Var("one", typeof<'C>)
                    let oneExpr: Expr<'C> = <@ backend.div %xExpr %xExpr @>

                    let innerCache, innerBindings = Caching.makeBindingCache ()

                    let fBody =
                        if requiresGradients
                        then
                            // Make a custom AD lifter to multiply constants by
                            // a one value that is tagged into the AD tape.
                            let adLift (ex: Expr<'C>) : Expr<'C> =
                                <@ 
                                    let one = backend.div %xExpr %xExpr
                                    backend.mul %ex one
                                @>
                            buildQuotationCore shouldCache innerCache backend scalarBackend inverseFn requiresGradients context symbolsForFn batchLength (ME fn) |> adLift
                        else
                            buildQuotationCore shouldCache innerCache backend scalarBackend inverseFn requiresGradients context symbolsForFn batchLength (ME fn)
                    
                    let innerPairs: (Var * Expr<'C>) list = innerBindings |> Seq.toList
                    let fBodyWithLets: Expr<'C> =
                        List.foldBack
                            (fun (v, rhs) (acc: Expr<'C>) -> Expr.Cast<'C>(Expr.Let(v, rhs, acc)))
                            innerPairs
                            fBody

                    let fBodyWithOne: Expr<'C> =
                        Expr.Let(oneVar, oneExpr, fBodyWithLets) |> Expr.Cast<'C>

                    let fLambda = Expr.Lambda(xVar, fBodyWithOne) |> Expr.Cast<'C -> 'C>

                    let targetExpr = build (ME targetVal)
                    let loExpr = build (ME lo)
                    let hiExpr = build (ME hi)
                    let tol = backend.promote backend.atomic.constants.relTol batchLength
                    let maxIter = 50
                    inverseFn backend fLambda targetExpr loExpr hiExpr tol maxIter)

            | ME(GreaterThan _)
            | ME(LessThan _)
            | ME(IsFinite _)
            | ME(EqualTo _) -> failwith "Numeric builder received a boolean node"


        let internal buildQuotation'
            (policy: ModelExpression<'u> -> bool)
            (backend: NumericBackend<'S,'C>)
            scalarBackend
            ctx
            inverseFn
            (symbols: Map<string, Var>)
            (expr: ModelExpression<'u>)
            : Expr<'C> =
            let (bindingCache: Caching.QuotationCache<'C, 'u>), bindings =
                Caching.makeBindingCache ()

            let batchLength = 1
            let reqGrad = false
            let body: Expr<'C> = buildQuotationCore policy bindingCache backend scalarBackend inverseFn reqGrad ctx symbols batchLength expr

            let full: Expr<'C> =
                List.foldBack
                    (fun (v, rhs) (acc: Expr<'C>) -> Expr.Let(v, rhs, acc) |> Expr.Cast<'C>)
                    (bindings |> Seq.toList)
                    body

            full

        /// Builds a quotation from a model expression tree simply,
        /// with no internal caching or let bindings.
        let buildQuotation containerBackend scalarBackend ctx inverseFn symbols expr =
            buildQuotation' Caching.Policies.noCaching containerBackend scalarBackend ctx inverseFn symbols expr

        /// Builds a quotation from a model expression tree,
        /// using an internal cache to avoid rebuilding 'heavy'
        /// nodes that have already been built. 'Heavy' nodes
        /// are introduced as let bindings to minimise repetition
        /// of work in computation.
        let buildQuotationCached
            (backend: NumericBackend<'S,'C>)
            scalarBackend
            ctx
            inverseFn
            (symbols: Map<string, Var>)
            (expr: ModelExpression<1>)
            : Expr<'C> =
            let bindingCache, bindings = Caching.makeBindingCache ()

            let reqGrad = false
            let batchLen = 1
            let body: Expr<'C> =
                buildQuotationCore Caching.Policies.cacheHeavy bindingCache backend scalarBackend inverseFn reqGrad ctx symbols batchLen expr

            let full: Expr<'C> =
                List.foldBack
                    (fun (v, rhs) (acc: Expr<'C>) -> Expr.Let(v, rhs, acc) |> Expr.Cast<'C>)
                    (bindings |> Seq.toList)
                    body

            full


        let getEnvironment eVar name =
            <@
                let map = %%Expr.Var eVar: CodedMap<FsScalar<'S,ModelSystem.``environment``>>

                map
                |> Map.tryFindBy (fun k -> k.Value = name)
                |> fun o ->
                    match o with
                    | Some o -> o |> Scalar.toRaw
                    | None -> failwithf "State / environment %s was not available. Available: %A" name map.Keys
            @>

        // // Initialisers are time-invariant (t = initial), but can access the current exogeneous
        // // environment and observed states at the initial time.

        let private toLambda4 v1 v2 v3 v4 body =
            Expr.Lambda(v1, Expr.Lambda(v2, Expr.Lambda(v3, Expr.Lambda(v4, body))))

        /// Compile a rate-based (e.g. ODE) model expression into an internal
        /// `RateEquation` for use in model-fitting.
        /// Currently hard-coded such that 'S = 'C (i.e. is a degenerate backend / scalar).
        let compileRate<[<Measure>] 'timeUnit, [<Measure>] 'stateUnit, 'S,'V>
            (backend: NumericBackend<'S,'S>)
            parameters
            envKeys
            (expr: ModelExpression<'stateUnit / 'timeUnit>)
            : ModelSystem.RateEquation<'S,'V,'timeUnit> =

            let pVar = Var("parameters", typeof<FsVector<'S,'V,``parameter``>>)
            let eVar = Var("environment", typeof<CodedMap<FsScalar<'S,ModelSystem.``environment``>>>)
            let tVar = Var("time", typeof<FsScalar<'S,'timeUnit>>)
            let xVar = Var("state", typeof<FsScalar<'S,'stateUnit>>)

            let eIndex = envIndexFromKeys envKeys // TODO - USE
            let pIndex = paramIndex parameters
            let ctx = {
                GetParameter = fun name ->
                    let idx = pIndex.[name]
                    <@ 
                        (%%Expr.Var pVar : FsVector<'S,'V,``parameter``>) 
                        |> Vector.itemAt backend idx
                        |> Scalar.toRaw
                    @>
                ParameterIndex = paramIndex parameters
                Environment = fun name -> getEnvironment eVar name
                GetState = fun name _ -> getEnvironment eVar name 
                TimeVal = <@ (%%Expr.Var tVar: FsScalar<'S,'timeUnit>) |> Scalar.toRaw  @>
                ThisVal = <@ (%%Expr.Var xVar: FsScalar<'S,'stateUnit>) |> Scalar.toRaw @>
            }

            // NB The quotation compiler doesn't currently support units. Strip here and reapply later.
            let exprNoUnit = expr |> fun (ME expr) -> expr |> ME
            let core: Expr<'S> = buildQuotationCached backend backend ctx inverseFn Map.empty exprNoUnit

            // Convert from 'stateUnit/'timeUnit to internal state/'timeUnit
            <@ %core |> Scalar.ofRaw backend |> Scalar.retype<'S,1,ModelSystem.state/'timeUnit> @> // Retype unit onto solution here
            |> toLambda4 pVar eVar tVar xVar
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<ModelSystem.RateEquation<'S,'V,'timeUnit>>

        let compileDiscrete<[<Measure>] 'timeUnit, [<Measure>] 'stateUnit, 'S,'V>
            (backend: NumericBackend<'S,'S>)
            parameters
            envKeys
            (expr: ModelExpression<'stateUnit>)
            : ModelSystem.StateEquation<'S,'V,'timeUnit> =

            let pVar = Var("parameters", typeof<FsVector<'S,'V,``parameter``>>)
            let eVar = Var("environment", typeof<CodedMap<FsScalar<'S,ModelSystem.``environment``>>>)
            let tVar = Var("time", typeof<FsScalar<'S,'timeUnit>>)
            let xVar = Var("state", typeof<FsScalar<'S,'stateUnit>>)

            let eIndex = envIndexFromKeys envKeys // TODO - USE
            let pIndex = paramIndex parameters
            let discreteContext = {
                GetParameter = fun name ->
                    let idx = pIndex.[name]
                    <@ 
                        (%%Expr.Var pVar : FsVector<'S,'V,``parameter``>) 
                        |> Vector.itemAt backend idx
                        |> Scalar.toRaw
                    @>
                ParameterIndex = pIndex
                Environment = fun name -> getEnvironment eVar name
                GetState = fun name _ -> getEnvironment eVar name 
                TimeVal = <@ (%%Expr.Var tVar: FsScalar<'S,'timeUnit>) |> Scalar.toRaw  @>
                ThisVal = <@ (%%Expr.Var xVar: FsScalar<'S,'stateUnit>) |> Scalar.toRaw @>
            }

            // NB The quotation compiler doesn't currently support units. Strip here and reapply later.
            let exprNoUnit = expr |> fun (ME expr) -> expr |> ME
            let core: Expr<'S> = buildQuotationCached backend backend discreteContext inverseFn Map.empty exprNoUnit

            <@ %core |> Scalar.ofRaw backend |> Scalar.retype<'S,1, ModelSystem.state> @> // Retype unit back here
            |> toLambda4 pVar eVar tVar xVar
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<ModelSystem.StateEquation<'S,'V,'timeUnit>>

        // // TODO
        // let compileSimple<[<Measure>] 'u, 'S,'V> (backend: NumericBackend<'S,'S>) (expr: ModelExpression<'u>) : float<'u> =
        //     let exprNoUnit = expr |> fun (ME expr) -> expr |> ME
        //     let ctx = {
        //         ParameterVar = ()
        //         ParameterIndex = Map.empty
        //         Environment = fun _ -> failwith "Environment not supported in measures"
        //         GetState = fun _ -> failwith "Environment not supported in measures"
        //         TimeVal = <@ backend.atomic.constants.invalid @>
        //         ThisVal = <@ backend.atomic.constants.invalid @>
        //     }
                       
        //     let qCore : Expr<'S> =
        //         buildQuotationCached
        //             backend
        //             ctx
        //             (inverseFn())
        //             Map.empty
        //             exprNoUnit

        //     match LeafExpressionConverter.EvaluateQuotation qCore with
        //     | :? 'S as raw -> backend.atomic.toFloatS raw |> Units.retype
        //     | _ -> failwith "Expected a scalar tensor result"

        let compileMeasure<[<Measure>] 'stateUnit, 'S,'V> backend parameters (expr: ModelExpression<'stateUnit>) =
            
            let pVar = Var("parameters", typeof<FsVector<'S,'V,parameter>>)
            let statesVar = Var("states", typeof<CodedMap<FsVector<'S,'V,ModelSystem.state>>>)
            let thisVar = Var("thisVal", typeof<FsScalar<'S,ModelSystem.state>>)
            let tIdxVar = Var("timeIndex", typeof<int>)

            let pIndex = paramIndex parameters
            let ctx = {
                GetParameter = fun name ->
                    let idx = pIndex.[name]
                    <@ 
                        (%%Expr.Var pVar : FsVector<'S,'V,``parameter``>) 
                        |> Vector.itemAt backend idx
                        |> Scalar.toRaw
                    @>
                ParameterIndex = pIndex
                Environment = fun _ -> failwith "Environment not supported in measures"
                GetState = fun name t ->
                    <@
                        let states = %%Expr.Var statesVar: CodedMap<FsVector<'S,'V,ModelSystem.state>>
                        let vec = states |> Map.tryFindBy (fun n -> n.Value = name) |> Option.get
                        Vector.itemAt backend ((%%Expr.Var tIdxVar: int) + Units.removeUnitFromInt 0) vec
                        |> Scalar.toRaw
                    @>
                TimeVal = <@ Units.intToFloat (%%Expr.Var tIdxVar: int) |> backend.atomic.fromFloatS @>
                ThisVal = <@ (%%Expr.Var thisVar: FsScalar<'S,ModelSystem.state>) |> Scalar.toRaw @>
            }

            // NB The quotation compiler doesn't currently support units. Strip here and reapply later.
            let exprNoUnit = expr |> fun (ME expr) -> expr |> ME
            let core: Expr<'S> = buildQuotationCached backend backend ctx inverseFn Map.empty exprNoUnit

            <@ %core |> Scalar.ofRaw backend |> Scalar.retype<'S,1, ModelSystem.state> @> // Retype unit on here
            |> fun body -> Expr.Lambda(pVar, Expr.Lambda(statesVar, Expr.Lambda(thisVar, Expr.Lambda(tIdxVar, body))))
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<ModelSystem.Measurement<'S,'V,'stateUnit>>

        let compileInitialiser<[<Measure>] 'stateUnit, 'S,'V> backend parameters (expr: ModelExpression<'stateUnit>) =
            
            let pVar = Var("parameters", typeof<FsVector<'S,'V,``parameter``>>)
            let eVar = Var("environment", typeof<CodedMap<FsScalar<'S,ModelSystem.``environment``>>>)
            let bVar = Var("baselines", typeof<CodedMap<FsScalar<'S,ModelSystem.state>>>)

            let pIndex = paramIndex parameters
            let ctx = {
                GetParameter = fun name ->
                    let idx = pIndex.[name]
                    <@ 
                        (%%Expr.Var pVar : FsVector<'S,'V,``parameter``>) 
                        |> Vector.itemAt backend idx
                        |> Scalar.toRaw
                    @>
                ParameterIndex = paramIndex parameters
                Environment = fun name -> getEnvironment eVar name
                GetState = fun name t ->
                    if t <> 0<Time.``time index``> then failwith "'State at' not supported in initialisers. Use 'State' for baseline."
                    else
                        <@
                            (%%Expr.Var bVar: CodedMap<FsScalar<'S,ModelSystem.``state``>>)
                            |> Map.tryFindBy (fun k -> k.Value = name)
                            |> fun o ->
                                match o with
                                | Some o -> o |> Scalar.toRaw
                                | None -> failwithf "Baseline state not available for: %s" name
                        @>
                TimeVal =
                    let n = backend.atomic.fromFloatS nan
                    <@ n @>
                ThisVal = <@ failwith "This keyword not supported in initialisers" @>
            }

            // NB The quotation compiler doesn't currently support units. Strip here and reapply later.
            let exprNoUnit = expr |> fun (ME expr) -> expr |> ME
            let core: Expr<'S> = buildQuotationCached backend backend ctx inverseFn Map.empty exprNoUnit

            <@ %core |> Scalar.ofRaw backend |> Scalar.retype<'S,1, ModelSystem.state> @> // Retype unit on here.
            |> fun body -> Expr.Lambda(pVar, Expr.Lambda(eVar, Expr.Lambda(bVar, body)))
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<ModelSystem.Initialiser<'S,'V,'stateUnit>>


    /// Computes a `ModelExpression` given the current time, value,
    /// environment, and parameter pool.
    let computeRate (engine: Numerics.NumericEngine<'S,'V,'M>) x t pool (environment: CodedMap<Numerics.Typed.FsScalar<'S,ModelSystem.environment>>) ex =
        ExpressionCompiler.compileRate engine.scalarBackend pool (Seq.toList environment.Keys) ex
        |> fun f -> f (Parameter.Pool.toVectorWithKeysReal engine pool |> snd) environment t x

    let compute (engine: Numerics.NumericEngine<'S,'V,'M>) x t pool (environment: CodedMap<Numerics.Typed.FsScalar<'S,ModelSystem.environment>>) ex =
        ExpressionCompiler.compileDiscrete engine.scalarBackend pool (Seq.toList environment.Keys) ex
        |> fun f -> f (Parameter.Pool.toVectorWithKeysReal engine pool |> snd) environment t x


    module ExpressionParser =

        open Untyped

        type Requirement =
            | ParameterRequirement of string
            | EnvironmentRequirement of string

        let rec requirements expr =
            Writer.writer {
                match expr with
                | Parameter n -> do! Writer.tell (ParameterRequirement n)
                | Environment n -> do! Writer.tell (EnvironmentRequirement n)
                | Add xs
                | Multiply xs ->
                    for x in xs do
                        let! () = requirements x
                        ()
                | Subtract(l, r)
                | Divide(l, r)
                | Power(l, r)
                | Mod(l, r) ->
                    do! requirements l
                    do! requirements r
                | Logarithm e
                | Exponential e
                | Label(_, e) -> do! requirements e
                | _ -> return ()
            }

    /// Scaffolds a `ModelSystem` for fitting with Bristlecone.
    module Model =

        type Fragment = ExpressionParser.Requirement list * Untyped.ModelExpressionUntyped
        type EquationFragment =
            | Discrete of Fragment
            | Rate of Fragment

        // type LikelihoodFragment<'S,'V> =
        //     { RequiredCodes: ModelSystem.LikelihoodRequirement list
        //       RequiredParameters: (ShortCode.ShortCode * Parameter.Pool.ParameterNoUnit) list
        //       Evaluate: ModelSystem.Likelihood<'S,'V,ModelSystem.state> }

        type ModelDescriptionState<'S,'V> = {
            States: Map<ShortCode.ShortCode, (EquationFragment * Fragment option)>
            Measures: Map<ShortCode.ShortCode, Fragment>
            Parameters: Map<ShortCode.ShortCode, Parameter.Pool.ParameterNoUnit>
            Likelihood: ModelSystem.Likelihood<'S,'V,ModelSystem.state> option
        }

        type DiscreteTime = DiscreteTime
        type ContinuousTime = ContinuousTime

        type NoNumEngine = Placeholder

        // type NotReady = NotReady
        // type SimulationOnly = SimulationOnly

        type ModelDescription<[<Measure>] 'time, 'TimeMode,'S,'V> =
            private | ModelBuilder of ModelDescriptionState<'S,'V>

        let private empty'<[<Measure>] 'time, 'TimeMode,'S,'V> () : ModelDescription<'time, 'TimeMode,'S,'V> =
            ModelBuilder {
                States = Map.empty
                Measures = Map.empty
                Parameters = Map.empty
                Likelihood = None }

        let internal unwrap (ModelBuilder m) = m

        let empty<[<Measure>] 'time> : ModelDescription<'time, ContinuousTime,NoNumEngine,NoNumEngine> =
            empty' ()

        let discrete<[<Measure>] 'time> : ModelDescription<'time, DiscreteTime,NoNumEngine,NoNumEngine> =
            empty' ()

        let continuous<[<Measure>] 'time> : ModelDescription<'time, ContinuousTime,NoNumEngine,NoNumEngine> =
            empty' ()

        let private updateState fn (mb: ModelDescription<'u,'TimeMode,'S,'V>) : ModelDescription<'u,'TimeMode,'S,'V> =
            let oldState = unwrap mb
            ModelBuilder (fn oldState)

        let addRateEquation<[<Measure>] 'time, [<Measure>] 'state,'S,'V>
            (name: StateId<'state>)
            (expr: ModelExpression<'state / 'time>)
            (mb: ModelDescription<'time,ContinuousTime,'S,'V>) =
            let (ME eUntyped) = expr
            let reqs = ExpressionParser.requirements eUntyped |> Writer.run |> snd
            let frag = Rate(reqs, eUntyped)

            let existingEq =
                (unwrap mb).States
                |> Map.tryFind name.Code

            match existingEq with
            | Some _ -> failwithf "Cannot add two states with the same name (%s)" name.Code.Value
            | None -> updateState (fun state -> { state with States = state.States |> Map.add name.Code (frag, None) }) mb

        let addDiscreteEquation<[<Measure>] 'time, [<Measure>] 'state,'S,'V>
            (name: StateId<'state>)
            (expr: ModelExpression<'state>)
            (mb: ModelDescription<'time,DiscreteTime,'S,'V>)
            : ModelDescription<'time,DiscreteTime,'S,'V> =
            let (ME eUntyped) = expr
            let reqs = ExpressionParser.requirements eUntyped |> Writer.run |> snd
            let frag = Discrete(reqs, eUntyped)
            
            let existingEq =
                (unwrap mb).States
                |> Map.tryFind name.Code

            match existingEq with
            | Some _ -> failwithf "Cannot add two states with the same name (%s)" name.Code.Value
            | None -> updateState (fun state -> { state with States = state.States |> Map.add name.Code (frag, None) }) mb
            
        /// Used for scaffolding components into hypotheses internally.
        let internal estimateParameter'<[<Measure>] 'time, [<Measure>] 'u,'TimeMode,'S,'V>
            (p: Parameter.Pool.ParameterNoUnit)
            (name: ShortCode.ShortCode)
            (mb: ModelDescription<'time,'TimeMode,'S,'V>) =
            match (unwrap mb).Parameters |> Map.tryFind name with
            | Some _ -> failwithf "Cannot add two states with the same name (%s)" name.Value
            | None -> updateState (fun state -> { state with Parameters = state.Parameters |> Map.add name p }) mb

        let estimateParameter<[<Measure>] 'time, [<Measure>] 'u,'TimeMode,'S,'V>
            (p: IncludedParameter<'u>)
            (mb: ModelDescription<'time,'TimeMode,'S,'V>) =
            let (ParamIdInner name) = p.ParamId
            let boxed = Parameter.Pool.stripUnit<'u> name.Value p.Parameter
            estimateParameter' boxed name mb

        let addMeasure<[<Measure>] 'time, [<Measure>] 'u,'TimeMode,'S,'V>
            (name: MeasureId<'u>)
            (measure: ModelExpression<'u>)
            (mb: ModelDescription<'time,'TimeMode,'S,'V>)
            : ModelDescription<'time,'TimeMode,'S,'V> =
            let (ME eUntyped) = measure
            let envReqs = ExpressionParser.requirements eUntyped |> Writer.run |> snd
            match (unwrap mb).Measures |> Map.tryFind name.Code with
            | Some _ -> failwithf "Cannot add two measures with the same name (%s)" name.Code.Value
            | None -> updateState (fun state -> { state with Measures = state.Measures |> Map.add name.Code (envReqs, eUntyped) }) mb

        let initialiseHiddenStateWith<[<Measure>] 'time, [<Measure>] 'u,'TimeMode,'S,'V>
            (name: StateId<'u>)
            (initialiser: ModelExpression<'u>)
            (mb: ModelDescription<'time,'TimeMode,'S,'V>)
            : ModelDescription<'time,'TimeMode,'S,'V> =
            let (ME eUntyped) = initialiser
            let envReqs = ExpressionParser.requirements eUntyped |> Writer.run |> snd

            let existingEq =
                (unwrap mb).States
                |> Map.tryFind name.Code

            match existingEq with
            | None -> failwithf "You must add the equation before adding an initialiser (%s)" name.Code.Value
            | Some eq ->
                let frag =
                    match eq with
                    | Rate f, _ -> Rate f, Some (envReqs, eUntyped)
                    | Discrete f, _ -> Discrete f, Some (envReqs, eUntyped)
                updateState (fun state -> { state with States = state.States |> Map.add name.Code frag }) mb

        let useLikelihoodFunction<[<Measure>] 'time,'TimeMode,'S1,'V1,'S2,'V2>
            (l: ModelSystem.Likelihood<'S2,'V2,ModelSystem.state>)
            (mb: ModelDescription<'time,'TimeMode,'S1,'V1>)
            : ModelDescription<'time,'TimeMode,'S2,'V2> =
            let oldState = unwrap mb
            { Likelihood = Some l
              States = oldState.States
              Measures = oldState.Measures
              Parameters = oldState.Parameters } |> ModelBuilder


        module Compile =

            open Bristlecone.Numerics

            let internal assertParams parameters requirements likelihoodRequired =
                let present = Map.keys parameters |> Set.ofSeq
                let requiredParams =
                    requirements
                    |> List.choose (fun r ->
                        match r with
                        | ExpressionParser.Requirement.ParameterRequirement e ->
                            Some(e |> ShortCode.create |> Option.get)
                        | _ -> None)
                    |> List.append likelihoodRequired
                    |> Set.ofList
                
                if present <> requiredParams
                then
                    let unnecessary = Set.difference present requiredParams
                    if not unnecessary.IsEmpty
                    then failwithf "The following parameters were specified but are unnecessary: %A" unnecessary

                    let missing = Set.difference requiredParams present
                    if not missing.IsEmpty
                    then failwithf "The following parameters are required, but were not set: %A" missing


            /// Compiles an expression-based model system into lower-level numerical
            /// types, ready for use in Bristlecone inference or simulation.
            let compile (numEngine: NumericEngine<'S,'V,'M>) (mb:ModelDescription<'time,'TimeMode,'S,'V>) : ModelSystem.ModelSystem<'S,'V,'time> =
                
                let (ModelBuilder builder) = mb

                let likelihoodReqs = builder.Likelihood |> Option.map(fun l -> l.RequiredParameters |> List.map fst) |> Option.defaultValue []
                let requirements =
                    [
                        builder.States |> Seq.choose(fun kv -> kv.Value |> snd |> Option.map fst)
                        builder.Measures |> Seq.map(fun kv -> kv.Value |> fst)
                    ] |> Seq.concat |> Seq.concat |> Seq.toList |> List.distinct

                assertParams builder.Parameters requirements likelihoodReqs

                // Add likelihood parameters? Is this needed now?
                let parameters = builder.Parameters |> Parameter.Pool.Pool

                let requiredEnvVars =
                    requirements
                    |> List.choose (fun r ->
                        match r with
                        | ExpressionParser.Requirement.EnvironmentRequirement e ->
                            Some(e |> ShortCode.create |> Option.get)
                        | _ -> None)

                let intialisers =
                    builder.States
                    |> Seq.choose(fun kv -> kv.Value |> snd |> Option.map(fun initi -> kv.Key, snd initi))
                    |> Map.ofSeq
                    |> Map.map(fun _ initi -> ExpressionCompiler.compileInitialiser numEngine.scalarBackend parameters (ME initi))

                let diffEqs =
                    builder.States
                    |> Seq.choose(fun kv ->
                        match fst kv.Value with
                        | Discrete (_,expr) -> Some (kv.Key, expr)
                        | _ -> None) |> Seq.toList
                
                let continEqs =
                    builder.States
                    |> Seq.choose(fun kv ->
                        match fst kv.Value with
                        | Discrete (_,expr) -> Some (kv.Key, expr)
                        | _ -> None) |> Seq.toList

                let stateEqs : ModelSystem.ModelForm<'S,'V,'time> =
                    match diffEqs.Length, continEqs.Length with
                    | _, 0 ->
                        diffEqs
                        |> List.map(fun (c,expr) ->
                            c, ExpressionCompiler.compileDiscrete numEngine.scalarBackend parameters requiredEnvVars (ME expr))
                        |> Map.ofList
                        |> ModelSystem.DifferenceEqs
                    | 0, _ ->
                        continEqs
                        |> List.map(fun (c,expr) ->
                            c, ExpressionCompiler.compileRate numEngine.scalarBackend parameters requiredEnvVars (ME expr))
                        |> Map.ofList
                        |> ModelSystem.DifferentialEqs
                    | _ -> failwith "Mixture of discrete and continuous-time equations specified."

                let measures =
                    builder.Measures
                    |> Map.map(fun _ (_,expr) -> ExpressionCompiler.compileMeasure numEngine.scalarBackend parameters (ME expr))

                let likelihood =
                    builder.Likelihood
                    |> Option.defaultWith(fun _ -> failwith "No likelihood function specified.")

                { NegLogLikelihood = likelihood // TODO
                  Parameters = parameters
                  EnvironmentKeys = requiredEnvVars
                  Initialisers = intialisers
                  Equations = stateEqs
                  Measures = measures }

        /// Pre-compile a model system for use in Bristlecone.
        /// Uses the default numeric engine (DiffSharp).
        let compile mb =
            Compile.compile Numerics.Backends.DiffSharp.engine mb

        /// Pre-compile a model system for use in Bristlecone.
        /// Specify a custom numeric engine that matches the estimation
        /// engine that you will use.
        let compileWith (numEng: Numerics.NumericEngine<'S,'V,'M>) mb =
            Compile.compile numEng mb


        module ComputationExpression =

            // Phantom types for tracking whether model has parts or not:
            type Missing = Missing
            type Present = Present

            type CeState<[<Measure>] 'time, 'TimeMode, 'HasEq, 'HasLike,'S,'V> =
                private
                    { Inner: ModelDescription<'time,'TimeMode,'S,'V> }

            let emptyState<[<Measure>] 'time> : CeState<'time, ContinuousTime, Missing, Missing,NoNumEngine,NoNumEngine> =
                { Inner = continuous }

            let emptyDiscrete<[<Measure>] 'time> : CeState<'time, DiscreteTime, Missing, Missing,NoNumEngine,NoNumEngine> =
                { Inner = discrete }

            type ModelSystemBuilder<[<Measure>] 'time> () =
                member _.Yield(_) = emptyState<'time>
                member _.Delay(f) = f ()
                member _.Zero() =
                    fun (state: CeState<'time, ContinuousTime, Present, Present,'S,'V>) ->
                        state.Inner

                // member _.Return(backend: Numerics.NumericEngine<'S,'V,'M>) =
                //     fun (state: CeState<'time, ContinuousTime, Present, Present>) ->
                //         Compile.compile backend state.Inner

                [<CustomOperation("estimate")>]
                member _.Parameter<[<Measure>] 'u, 'E, 'L,'S,'V>
                    (state: CeState<'time,ContinuousTime, 'E, 'L,'S,'V>, p: IncludedParameter<'u>)
                    =
                    { Inner = estimateParameter p state.Inner }

                // [<CustomOperation("equation")>]
                // member _.Equation
                //     (state: CeState<'time, ContinuousTime, 'E, 'L>, sid: StateId<'u>, expr: ModelExpression<'u>)
                //     : CeState<'time,ContinuousTime, Present, 'L> =
                //     { Inner = addRateEquation sid expr state.Inner }

                [<CustomOperation("measure")>]
                member _.Measure<[<Measure>] 'u, 'E, 'L,'S,'V>
                    (state: CeState<'time, ContinuousTime, 'E, 'L,'S,'V>, mid: MeasureId<'u>, data)
                    =
                    { Inner = addMeasure mid data state.Inner }

                [<CustomOperation("initialise")>]
                member _.Init<[<Measure>] 'u, 'E, 'L,'S,'V>
                    (state: CeState<'time, ContinuousTime, 'E, 'L,'S,'V>, sid: StateId<'u>, initFn)
                    =
                    { Inner = initialiseHiddenStateWith sid initFn state.Inner }

                /// Add exactly one likelihood function.
                [<CustomOperation("likelihood")>]
                member _.Likelihood<'E,'S,'V,'S2,'V2>
                    (state: CeState<'time, ContinuousTime, 'E, Missing,'S,'V>, fn: ModelSystem.Likelihood<'S2,'V2,ModelSystem.state>)
                    : CeState<'time, ContinuousTime, 'E, Present,'S2,'V2> =
                    { Inner = useLikelihoodFunction fn state.Inner }

                member _.Run(state: CeState<'time, ContinuousTime, Present, Present,'S,'V>) = state.Inner



    /// A computation expression for writing a bristlecone
    /// model system.
    /// TODO Not fully functional.
    let model<[<Measure>] 'time> : Model.ComputationExpression.ModelSystemBuilder<'time> =
        Model.ComputationExpression.ModelSystemBuilder ()


    /// Terms for designing tests for model systems.
    module Test =

        open Bristlecone.Test

        /// Start defining conditions for a new test procedure
        /// for a model + estimation engine combination. This default
        /// uses modern calendar time as the basis. For other date/time systems,
        /// use `createWithTimeMode` instead.
        let create = Test.defaultSettings

        /// Start defining conditions for a new test procedure
        /// for a model + estimation engine combination. Specify
        /// a `dateMode` and associated temporal resolution and start
        /// date that are in compatible time systems.
        let createWithTimeMode dateMode resolution startDate : TestSettings<'state, 'date, 'year, 'timespan> =
            { Resolution = resolution
              TimeSeriesLength = defaultSettings.TimeSeriesLength
              StartValues = Map.empty
              GenerationRules = List.empty
              ObservationErrorFn = fun _ _ ts -> ts
              EnvironmentalData = Map.empty
              StartDate = startDate
              RetryDataGen = 100
              DateMode = dateMode }

        let private obsToCode =
            function
            | Require.MeasureObs m -> m.Code
            | Require.StateObs s -> s.Code

        let private getParam (p: IncludedParameter<'s>) pool : float<'s> =
            match Parameter.Pool.tryGetRealValue<'s> p.ParamId.Inner.Value pool with
            | Some p -> p
            | None -> failwithf "parameter %s not available." p.ParamId.Inner.Value

        module Error =

            let normal sigma =
                fun rnd pool ->
                    let sigma = getParam sigma pool
                    ObservationError.normal rnd sigma

            let logNormal sigma =
                fun rnd pool ->
                    let sigma = getParam sigma pool
                    ObservationError.logNormal rnd sigma


        /// <summary>Add observation error ('noise') to a particular time-series when
        /// generating fake time-series. Built-in noise functions are in the `Noise` module.</summary>
        /// <param name="obs">The state or measure to add to (use Require.state or Require.measure)</param>
        /// <param name="genErrorFn">A function that generates error for any point based on its value.</param>
        /// <param name="settings">Current test settings</param>
        /// <returns>Updated test settings</returns>
        let withObservationError (obs: Require.ObsForLikelihood<'s>) genErrorFn settings =
            let code = obsToCode obs

            let fn rnd pool ts =
                let addNoise = genErrorFn rnd pool
                Test.ObservationError.addNoise code addNoise ts

            let stackNoise rnd pool ts =
                settings.ObservationErrorFn rnd pool ts |> fn rnd pool

            { settings with
                ObservationErrorFn = stackNoise }

        let withEnvironmentGen
            (obs: StateId<'s>)
            (genFn: 'date -> float<'s>)
            (settings: TestSettings<'state, 'date, 'a, 'b>)
            =
            let envTs =
                Bristlecone.Time.TimeSeries.fromGen
                    settings.DateMode
                    settings.StartDate
                    settings.Resolution
                    settings.TimeSeriesLength
                    genFn
                |> Bristlecone.Time.TimeSeries.map (fun (v, _) -> v |> Units.retype)

            { settings with
                EnvironmentalData = settings.EnvironmentalData |> Map.add obs.Code envTs }

        let withEnvironmentGenBySpan
            (obs: StateId<'s>)
            (genFn: float<'fnTime> -> float<'s>)
            (gen2: 'timespan -> float<'fnTime>)
            (settings: TestSettings<'state, 'date, 'yearUnit, 'timespan>)
            =

            let startDate =
                match settings.Resolution with
                | Time.Resolution.Years y -> settings.DateMode.AddYears settings.StartDate -y.Value
                | Time.Resolution.Months m -> settings.DateMode.AddMonths settings.StartDate -m.Value
                | Time.Resolution.Days d -> settings.DateMode.AddDays settings.StartDate -d.Value
                | Time.Resolution.CustomEpoch c -> settings.DateMode.SubtractTime settings.StartDate c

            let envTs =
                Bristlecone.Time.TimeSeries.fromGenBaseline
                    settings.DateMode
                    startDate
                    settings.Resolution
                    (settings.TimeSeriesLength + 1)
                    genFn
                    gen2
                |> Bristlecone.Time.TimeSeries.map (fun (v, _) -> v |> Units.retype)

            { settings with
                EnvironmentalData = settings.EnvironmentalData |> Map.add obs.Code envTs }

        let internal withObservationErrorNormal obs sigma settings =
            withObservationError obs (Error.normal sigma) settings

        /// Set a start value (at t=1). If a value has already been set, it
        /// will be overwritten.
        let t1<[<Measure>] 's, [<Measure>] 'stateUnit, 'date, 'yearUnit, 'timespan>
            (obs: Require.ObsForLikelihood<'s>)
            (value: float<'s>)
            (settings: Test.TestSettings<'stateUnit, 'date, 'yearUnit, 'timespan>)
            =
            let code = obsToCode obs

            { settings with
                StartValues = settings.StartValues |> Map.add code (Units.retype value) }

        let rule (obs: Require.ObsForLikelihood<'s>) (rule: GenerationRule<'u>) settings =
            let code = obsToCode obs

            { settings with
                GenerationRules = (code, rule) :: settings.GenerationRules }

        let seriesLength n (settings: TestSettings<'stateUnit, 'date, 'yearUnit, 'timespan>) =
            { settings with TimeSeriesLength = n }

        let resolution res settings = { settings with Resolution = res }
        let startTime time settings = { settings with StartDate = time }


    module Components =

        /// A component with its own parameters and an expression generator
        type SubComponent<'a> =
            { Label: string
              Parameters: CodedMap<Parameter.Pool.ParameterNoUnit>
              Expr: 'a }

        type ModelComponent<'a> =
            { Label: string
              Implementations: SubComponent<'a> list }

        /// Creates a nested component that can be inserted into a base model.
        let subComponent name transform =
            { Label = name
              Parameters = Map.empty
              Expr = transform }

        let modelComponent name list =
            { Label = name; Implementations = list }

        let estimateParameter<[<Measure>] 'u, 'a> (p: IncludedParameter<'u>) (comp: SubComponent<'a>) =
            let (ParamIdInner name) = p.ParamId
            let boxed = Parameter.Pool.stripUnit<'u> name.Value p.Parameter

            { comp with
                Parameters = comp.Parameters |> Map.add name boxed }



        type SubComponentState<[<Measure>] 'u> =
            { Label: string
              Expr: ModelExpression<'u> option
              Estimates: (string * Parameter.Constraint<'u> * float * float) list }

        type ComponentState<[<Measure>] 'u> = { Options: SubComponentState<'u> list }


        type SubComponentBuilder<[<Measure>] 'u>(label: string) =

            member _.Yield(_) =
                { Label = label
                  Expr = None
                  Estimates = [] }

            member _.Zero() =
                { Label = label
                  Expr = None
                  Estimates = [] }

            member _.Bind(p: Parameter.Parameter<'p>, cont: Parameter.Parameter<'p> -> SubComponentState<'u>) = cont p

            member _.Parameter<[<Measure>] 'p>
                (name: string, con, lower: float<'p>, upper: float<'p>, state: SubComponentState<'p>)
                =
                let p = parameter name con lower upper

                { state with
                    Estimates = (name, con, float lower, float upper) :: state.Estimates },
                p

            member _.Expression(expr: ModelExpression<'u>, state: SubComponentState<'u>) =
                { state with Expr = Some expr }

            member _.Run(state: SubComponentState<'u>) =
                match state.Expr with
                | Some _ -> state
                | None -> failwithf "Subcomponent '%s' has no expression" state.Label


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
            |> fun s -> System.Text.RegularExpressions.Regex.Replace(s, "[^A-Z0-9_]", "")

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
        type Hypothesis<'S,'V,[<Measure>] 'timeIndex> =
            | Hypothesis of ModelSystem.ModelSystem<'S,'V,'timeIndex> * list<ComponentName>

            member private this.Unwrap = let (Hypothesis(m, comps)) = this in m, comps

            /// <summary>Compiles a reference code that may be used to identify (although not necessarily uniquely) this hypothesis</summary>
            /// <returns>A string in the format XX_XXX_YY_YYY... where XX_XXX is a singe component with XX the component and XXX the implementation.</returns>

            member this.ReferenceCode =
                this.Unwrap |> snd |> List.map (fun c -> c.Reference) |> String.concat "_"

            member this.Components = this.Unwrap |> snd
            member this.Model = this.Unwrap |> fst

        // Start the pipeline: lift the base model into a builder
        let createFromModel
            (baseModel: 'a -> 'rest)
            : Writer.Writer<'a -> 'rest, ComponentName * CodedMap<Parameter.Pool.ParameterNoUnit>> list =
            [ Writer.return' baseModel ]

        ///
        let apply
            (comp: Components.ModelComponent<'a>)
            (builders: Writer.Writer<'a -> 'rest, ComponentName * CodedMap<Parameter.Pool.ParameterNoUnit>> list)
            : Writer.Writer<'rest, ComponentName * CodedMap<Parameter.Pool.ParameterNoUnit>> list =
            if comp.Implementations.IsEmpty then
                failwith "Components must have at least one implementation."

            [ for sc in comp.Implementations do
                  for (Writer.AWriter(f, logs)) in builders do
                      let entry =
                          ({ Component = comp.Label
                             Implementation = sc.Label },
                           sc.Parameters)

                      yield Writer.AWriter(f sc.Expr, logs @ [ entry ]) ]

        /// Compile: run the writer(s), add parameters into the model builder, and wrap in Hypothesis
        let compileWith
            numBackend // TODO Changed API, with no doc.
            (builders:
                Writer.Writer<Model.ModelDescription<'time, 'timeMode,'S,'V>, ComponentName * CodedMap<Parameter.Pool.ParameterNoUnit>> list)
            : Hypothesis<'S,'V,'u> list =
            builders
            |> List.map (fun (Writer.AWriter(mb, logs)) ->
                let withParams =
                    logs
                    |> List.collect (snd >> Map.toList)
                    |> List.fold
                        (fun mb (name, p) -> mb |> Model.estimateParameter' p name)
                        mb

                Hypothesis(Model.compileWith numBackend withParams, logs |> List.map fst))

        let compile numBackend builders = compileWith numBackend builders
