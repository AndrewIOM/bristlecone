namespace Bristlecone

/// An F# Domain Specific Language (DSL) for scripting with
/// Bristlecone.
module Language =

    [<NoEquality; NoComparison>]
    type StateId<[<Measure>] 'u> = private StateIdInner of ShortCode.ShortCode

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
            | Arbitrary of
                (float -> float<Time.``time index``> -> Parameter.Pool.ParameterPool -> ModelSystem.ExternalEnvironment -> float) *
                list<ArbitraryRequirement>
            | Mod of ModelExpressionUntyped * ModelExpressionUntyped
            | Power of ModelExpressionUntyped * ModelExpressionUntyped
            | Logarithm of ModelExpressionUntyped
            | Exponential of ModelExpressionUntyped
            | Conditional of cond: ModelExpressionUntyped * ifTrue: ModelExpressionUntyped * ifFalse: ModelExpressionUntyped
            | Label of string * ModelExpressionUntyped
            | StateAt of offset:int<Time.``time index``> * string
            | Invalid

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
    let StateAt (offset: int<Time.``time index``>, name: string) : ModelExpression<'u> = ME (Untyped.StateAt (offset, name))
    let Time<[<Measure>] 't> () : ModelExpression<'t> = ME Untyped.Time
    let Environment<[<Measure>] 'u> sid : ModelExpression<'u> = ModelExpression.Environment sid

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
            cond        : Expr<'r> * Expr<'r> * Expr<'r> -> Expr<'r>
            label       : string * Expr<'r> -> Expr<'r>
            stateAt     : int<Time.``time index``> * string -> Expr<'r>
            invalid     : unit -> Expr<'r> }

        let rec private buildQuotation<'r, [<Measure>] 'u> (ops: Ops<'r>) (expr: ModelExpression<'u>) =
            match expr with
            | ME (Constant n)        -> ops.constVal n
            | ME (Parameter n)       -> ops.parameter n
            | ME (Environment n)     -> ops.environment n
            | ME Time                -> ops.timeVal
            | ME This                -> ops.thisVal
            | ME (Add xs)            -> xs |> List.map (fun x -> buildQuotation ops (ME x)) |> ops.add
            | ME (Multiply xs)       -> xs |> List.map (fun x -> buildQuotation ops (ME x)) |> ops.mul
            | ME (Subtract (l,r))    -> ops.sub (buildQuotation ops (ME l), buildQuotation ops (ME r))
            | ME (Divide (l,r))      -> ops.div (buildQuotation ops (ME l), buildQuotation ops (ME r))
            | ME (Power (l,r))       -> ops.pow (buildQuotation ops (ME l), buildQuotation ops (ME r))
            | ME (Logarithm e)       -> ops.log (buildQuotation ops (ME e))
            | ME (Exponential e)     -> ops.exp (buildQuotation ops (ME e))
            | ME (Mod (l,r))         -> ops.modulo (buildQuotation ops (ME l), buildQuotation ops (ME r))
            | ME (Conditional (c,t,f)) ->
                ops.cond (buildQuotation ops (ME c), buildQuotation ops (ME t), buildQuotation ops (ME f))
            | ME (Label (n,m))       -> ops.label (n, buildQuotation ops (ME m))
            | ME (StateAt (off, nm)) -> ops.stateAt (off, nm)
            | ME Invalid             -> ops.invalid()
            | ME (Arbitrary _)       -> failwith "Arbitrary not handled"


        // ---- Tensor ops ----
        module DslHelpers =
            let ifThenElse (cond: Tensor) (thenVal: Tensor) (elseVal: Tensor) =
                //dsharp.where(cond, thenVal, elseVal)
                failwith "not finished"

        // let pVar      = Var("parameters", typeof<TypedTensor<Vector,``parameter``>>)
        // let statesVar = Var("states", typeof<CodedMap<TypedTensor<Scalar,ModelSystem.state>>[]>)
        // let tIdxVar   = Var("timeIndex", typeof<int>)
        // let pIndex    = paramIndex parameters
        let private tensorOps<[<Measure>] 'timeUnit> (pIndex: Map<string,int>) (eIndex: Map<string,int>)
                            (pVar: Var) (eVar: Var) (tVar: Var) (xVar: Var) = {
            constVal    = fun n -> <@ dsharp.tensor n @>
            parameter   = fun name -> <@ (%%Expr.Var pVar : TypedTensor<Vector,``parameter``>).Value.[pIndex.[name]] @>
            environment = fun name ->
                <@
                    (%%Expr.Var eVar : CodedMap<TypedTensor<Scalar,ModelSystem.``environment``>>)
                    |> Map.tryFindBy(fun k -> k.Value = name)
                    |> Option.get
                    |> fun t -> t.Value
                @>
            timeVal     = <@ (%%Expr.Var tVar : TypedTensor<Scalar,'timeUnit>).Value @>
            thisVal     = <@ (%%Expr.Var xVar : TypedTensor<Scalar,ModelSystem.state>).Value @>
            add         = List.reduce (fun l r -> <@ dsharp.add(%l, %r) @>)
            sub         = fun (l,r) -> <@ dsharp.sub(%l, %r) @>
            mul         = List.reduce (fun l r -> <@ %l * %r @>)
            div         = fun (l,r) -> <@ dsharp.div(%l, %r) @>
            pow         = fun (l,r) -> <@ dsharp.pow(%l, %r) @>
            log         = fun e -> <@ dsharp.log %e @>
            exp         = fun e -> <@ dsharp.exp %e @>
            modulo      = fun (l,r) -> <@ %l - %r * dsharp.floor(%l / %r) @>
            cond        = fun (c,t,f) -> <@ DslHelpers.ifThenElse %c %t %f @>
            label       = fun (_,m) -> m
            stateAt     = fun _ -> failwith "State lookup not supported in equations"
            invalid     = fun () -> <@ dsharp.tensor nan @> }

        let private tensorOpsForMeasure
            (pIndex: Map<string,int>)
            (pVar: Var) (statesVar: Var) (tIdxVar: Var) = {
            constVal    = fun n -> <@ dsharp.tensor n @>
            parameter   = fun name -> <@ (%%Expr.Var pVar : Tensor).[pIndex.[name]] @>
            environment = fun _ -> failwith "Environment not used in measures"
            timeVal     = <@ dsharp.tensor (float (%%Expr.Var tIdxVar : int)) @>
            thisVal     = failwith "Not used in measures"
            add         = List.reduce (fun l r -> <@ dsharp.add(%l, %r) @>)
            sub         = fun (l,r) -> <@ dsharp.sub(%l, %r) @>
            mul         = List.reduce (fun l r -> <@ %l * %r @>)
            div         = fun (l,r) -> <@ dsharp.div(%l, %r) @>
            pow         = fun (l,r) -> <@ dsharp.pow(%l, %r) @>
            log         = fun e -> <@ dsharp.log %e @>
            exp         = fun e -> <@ dsharp.exp %e @>
            modulo      = fun (l,r) -> <@ %l - %r * dsharp.floor(%l / %r) @>
            cond        = fun (c,t,f) -> <@ DslHelpers.ifThenElse %c %t %f @>
            label       = fun (_,m) -> m
            invalid     = fun () -> <@ dsharp.tensor nan @>
            stateAt     = fun (offset,name) ->
                <@
                    let states = %%Expr.Var statesVar : CodedMap<TypedTensor<Vector,ModelSystem.state>>
                    let vec = states |> Map.tryFindBy(fun n -> n.Value = name) |> Option.get
                    (Typed.itemAt ((%%Expr.Var tIdxVar : int) + Units.removeUnitFromInt offset) vec).Value
                @>
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
            cond        = fun (c,t,f) -> <@ if %c <> 0.0 then %t else %f @>
            label       = fun (_,m) -> m
            stateAt     = fun _ -> failwith "State lookup not supported in equations"
            invalid     = fun () -> <@ nan @> }

        // ---- Lambda helper ----
        let private toLambda4 v1 v2 v3 v4 body =
            Expr.Lambda(v1, Expr.Lambda(v2, Expr.Lambda(v3, Expr.Lambda(v4, body))))

        // ---- Public compile functions ----
        // let compile<[<Measure>] 'timeUnit> parameters envKeys expr =
        //     let pVar = Var("parameters", typeof<TypedTensor<Vector,``parameter``>>)
        //     let eVar = Var("environment", typeof<CodedMap<TypedTensor<Scalar,ModelSystem.``environment``>>>)
        //     let tVar = Var("time", typeof<TypedTensor<Scalar,'timeUnit>>)
        //     let xVar = Var("thisValue", typeof<TypedTensor<Scalar,ModelSystem.state>>)
        //     let pIndex = paramIndex parameters
        //     let eIndex = envIndexFromKeys envKeys
        //     let core = buildQuotation (tensorOps pIndex eIndex pVar eVar tVar xVar) expr
        //     <@ tryAsScalar<ModelSystem.state> %core |> Option.get @>
        //     |> toLambda4 pVar eVar tVar xVar
        //     |> LeafExpressionConverter.EvaluateQuotation
        //     |> unbox<ModelSystem.GenericModelEquation<'timeUnit>>

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

            let core = buildQuotation (tensorOps pIndex eIndex pVar eVar tVar xVar) expr

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

            let core = buildQuotation (tensorOps pIndex eIndex pVar eVar tVar xVar) expr

            <@ tryAsScalar<ModelSystem.state> %core |> Option.get @>
            |> toLambda4 pVar eVar tVar xVar
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<ModelSystem.StateEquation<'timeUnit>>


        let compileFloat expr =
            let fpVar = Var("parameters", typeof<Parameter.Pool.ParameterPool>)
            let feVar = Var("environment", typeof<CodedMap<float>>)
            let ftVar = Var("time", typeof<float<Time.``time index``>>)
            let fxVar = Var("thisValue", typeof<float>)
            buildQuotation (floatOps fpVar feVar ftVar fxVar) expr
            |> toLambda4 fpVar feVar ftVar fxVar
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<Parameter.Pool.ParameterPool -> CodedMap<float> -> float<Time.``time index``> -> float -> float>

        let compileMeasure parameters stateKeys (expr: ModelExpression<'stateUnit>) =
            let pVar      = Var("parameters", typeof<TypedTensor<Vector,``parameter``>>)
            let statesVar = Var("states", typeof<CodedMap<TypedTensor<Scalar,ModelSystem.state>>[]>)
            let tIdxVar   = Var("timeIndex", typeof<int>)
            let pIndex    = paramIndex parameters
            buildQuotation (tensorOpsForMeasure pIndex pVar statesVar tIdxVar) expr
            |> fun body -> Expr.Lambda(pVar, Expr.Lambda(statesVar, Expr.Lambda(tIdxVar, body)))
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<ModelSystem.Measurement<'u>>



    /// Computes a `ModelExpression` given the current time, value,
    /// environment, and parameter pool.
    let compute x (t: float<Time.``time index``>) (pool: Parameter.Pool.ParameterPool) (environment: CodedMap<float>) ex : float =
        ExpressionCompiler.compileFloat ex pool environment t x

    let computeT x t pool (environment:CodedMap<Tensors.TypedTensor<Tensors.Scalar,ModelSystem.environment>>) ex =
        let eIndex, eScalar = environment |> Seq.toList |> List.map(fun kv -> kv.Key, kv.Value) |> List.unzip
        let eVector = eScalar |> List.toArray |> Tensors.Typed.stack1D
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


    // /// Allows common F# functions to use Bristlecone model expressions.
    // module ComputableFragment =

    //     open Writer

    //     // Takes the standard arguments and computes
    //     type ComputableFragment<'a> =
    //         { compute : float -> float -> CodedMap<float> -> CodedMap<float> -> 'a
    //           expr    : ModelExpression }

    //     /// Apply a Bristlecone model expression to a custom arbitrary function.
    //     let apply ex fn =
    //         { compute = fun x t p e -> fn (compute x t p e ex)
    //           expr    = ex }

    //     let applyAgain ex frag =
    //         { compute = (fun x t p e ->
    //             frag.compute x t p e (compute x t p e ex))
    //           expr    = ex }

    //     let asBristleconeFunction frag =
    //         Arbitrary(frag.compute, [])


    /// Scaffolds a `ModelSystem` for fitting with Bristlecone.
    module ModelBuilder =

        // A compiled-at-the-end equation, but with a closure that captures its typed expression
        type EquationThunk<[<Measure>] 'time> =
            | Discrete of ShortCode.ShortCode * (Parameter.Pool.ParameterPool -> ShortCode.ShortCode list -> ModelSystem.StateEquation<'time>)
            | Rate     of ShortCode.ShortCode * (Parameter.Pool.ParameterPool -> ShortCode.ShortCode list -> ModelSystem.RateEquation<'time>)
        
        type MeasureThunk = ShortCode.ShortCode * (unit -> ModelSystem.Measurement<ModelSystem.state>)
        type LikelihoodThunk = unit -> ModelSystem.Likelihood<ModelSystem.state>

        type ModelFragment<[<Measure>] 'time> =
            | EquationFragment  of EquationThunk<'time>
            | ParameterFragment of Parameter.Pool.AnyParameter
            | LikelihoodFragment of LikelihoodThunk
            | MeasureFragment    of MeasureThunk

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
                EquationFragment (Rate (sc, fun p e -> ExpressionCompiler.compileRate<'time, 'state> p e expr))

            let equationDiscrete<[<Measure>] 'time, [<Measure>] 'state> (name: string) (expr: ModelExpression<'state>) : ModelFragment<'time> =
                let sc = ShortCode.create name |> Option.get
                EquationFragment (Discrete (sc, fun p e -> ExpressionCompiler.compileDiscrete<'time, 'state> p e expr))

            // Unify measure/likelihood to internal <state> at add-time via adapters
            let measure<[<Measure>] 'u> (sc: ShortCode.ShortCode) (m: ModelSystem.Measurement<'u>) : ModelFragment<'time> =
                let toInternal () = Measurement.adapt<'u> m
                MeasureFragment (sc, toInternal)

            let likelihood<[<Measure>] 'u> (l: ModelSystem.Likelihood<'u>) : ModelFragment<'time> =
                let toInternal () = Likelihood.contramap<'u> l
                LikelihoodFragment toInternal

        let add<[<Measure>] 'time> (name: string) (frag: ModelFragment<'time>) (ModelBuilder (m,disc)) =
            let sc = ShortCode.create name |> Option.defaultWith (fun () -> failwithf "Bad short code %s" name)
            if m.ContainsKey sc then failwithf "Duplicate code [%s]" name
            ModelBuilder (m.Add(sc, frag), disc)

        let addEquationRate
            (name: string)
            (expr: ModelExpression<'u / 'time>)
            (mb: ModelBuilder<'time>) =
            let sc = ShortCode.create name |> Option.get
            add name (Add.equationRate<'time, 'state> sc.Value expr) mb

        let addEquationDiscrete name (expr: ModelExpression<'u>) (mb: ModelBuilder<'time>) =
            let sc = ShortCode.create name |> Option.get
            add name (Add.equationDiscrete sc.Value expr) mb

        let includeMeasure name (m: ModelSystem.Measurement<'u>) mb =
            let sc = ShortCode.create name |> Option.get
            add name (Add.measure sc m) mb

        let useLikelihood<[<Measure>] 'u,[<Measure>] 'time> (l: ModelSystem.Likelihood<'u>) mb =
            add "likelihood" (Add.likelihood<'u> l) mb

        let compile builder : ModelSystem.ModelSystem<'time> =
            let m, isDiscrete = unwrap builder

            // Partition fragments
            let likelihoods =
                m |> Map.toSeq |> Seq.choose (function _, LikelihoodFragment f -> Some f | _ -> None) |> Seq.toList
            let parameters =
                m |> Map.toSeq |> Seq.choose (function c, ParameterFragment p -> Some (c, p) | _ -> None)
                |> Map.ofSeq |> Parameter.Pool.Pool
            let measures =
                m |> Map.toSeq |> Seq.choose (function c, MeasureFragment (sc, mk) -> Some (c, mk()) | _ -> None)
                |> Map.ofSeq
            let equations =
                m |> Map.toSeq |> Seq.choose (function c, EquationFragment e -> Some (c, e) | _ -> None)
                |> Map.ofSeq

            // Validate likelihoods
            let like =
                match likelihoods with
                | [f] -> f()
                | []  -> failwith "You did not specify a likelihood function."
                | _   -> failwith "Multiple likelihoods specified."

            // Collect requirements from your original untyped parser if you still rely on it.
            // Alternatively, compute envKeys directly from typed expressions at add-time and capture them into the thunk.
            let envKeys =
                // existing requirement discovery or pre-collected keys
                []

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

            { NegLogLikelihood = like
              Parameters       = parameters
              EnvironmentKeys  = envKeys
              Equations        = eqs
              Measures         = measures }


    /// Terms for scaffolding a model system for use with Bristlecone.
    module Model =

        let empty<[<Measure>] 'time> : ModelBuilder.ModelBuilder<'time> = ModelBuilder.create false ()
        let discrete<'time> = ModelBuilder.create true ()

        let addRateEquation<[<Measure>] 'time, [<Measure>] 'state> name (expr: ModelExpression<'state/'time>) (mb: ModelBuilder.ModelBuilder<'time>) =
            ModelBuilder.addEquationRate name expr mb

        let addDiscreteEquation name expr mb =
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

        let estimateParameter (p:IncludedParameter<'u>) (builder: ModelBuilder.ModelBuilder<'time>) =
            let (ParamIdInner name) = p.ParamId
            let boxed = Parameter.Pool.boxParam<'u> name.Value p.Parameter
            ModelBuilder.add name.Value (ModelBuilder.ParameterFragment boxed) builder

        let addParameter (name: string) (p:Parameter.Parameter<'u>) (builder: ModelBuilder.ModelBuilder<'time>) =
            let boxed = Parameter.Pool.boxParam<'u> name p
            ModelBuilder.add name (ModelBuilder.ParameterFragment boxed) builder

        let includeMeasure name measure builder =
            ModelBuilder.add name (ModelBuilder.MeasureFragment measure) builder

        let useLikelihoodFunction likelihoodFn builder =
            ModelBuilder.add "likelihood" (ModelBuilder.LikelihoodFragment (fun () -> likelihoodFn)) builder

        let compile = ModelBuilder.compile


    module ModelSystemDsl =

        open ModelBuilder

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

        let state<[<Measure>] 'u> c : StateId<'u> = StateIdInner c
        let param<[<Measure>] 'u> c : ParamId<'u> = ParamIdInner c

        // Register a state with its unit and return a typed handle
        let declareState<[<Measure>] 'u> name (mb: ModelBuilderState<'time, 'E, 'L>)
            : StateId<'u> * ModelBuilderState<'time, 'E, 'L> =
            let sc = ShortCode.create name |> Option.defaultWith (fun () -> failwithf "Bad name %s" name)
            let sid = state sc
            // store the state name → box sid in the builder catalog for cross-reference validation
            sid, mb

        // // Register a parameter with unit and return a typed handle
        // let declareParam<[<Measure>] 'u> name lower upper constraintMode (mb: ModelBuilderState<'time, 'E, 'L>)
        //     : ParamId<'u> * ModelBuilderState<'time, 'E, 'L> =
        //     match Parameter.create constraintMode lower upper with
        //     | Some p ->
        //         let pid = declareParam<'u> name
        //         let boxed = Parameter.Pool.boxParam<'u> name p
        //         // add to internal pool
        //         pid, { mb with Inner = ModelBuilder.add name (ModelBuilder.ParameterFragment boxed) mb.Inner }
        //     | None -> failwith "Invalid bounds"

        let addRateEq
            (sid: StateId<'u>) (expr: ModelExpression<'stateUnit / 'timeUnit>)
            (mb: ModelBuilderState<'time, 'E, 'L>) =
            let (StateIdInner sc) = sid
            { mb with Inner = ModelBuilder.addEquationRate sc.Value expr mb.Inner }

        let addDiscreteEq
            (sid: StateId<'u>) (expr: ModelExpression<'u>)
            (mb: ModelBuilderState<'time, 'E, 'L>) =
            let (StateIdInner sc) = sid
            { mb with Inner = ModelBuilder.addEquationDiscrete sc.Value expr mb.Inner }

        // let addMeasure<[<Measure>] 'u, [<Measure>] 'time>
        //     (sid: StateId<'u>) (meas: ModelSystem.Measurement<'u>) (mb: ModelBuilderState<'time, 'E, 'L>) =
        //     let (StateIdInner sc) = sid
        //     { mb with Inner = ModelBuilder.includeMeasure sc.Value meas mb.Inner }


        type ModelSystemBuilder<[<Measure>] 'time>(isDiscrete) =
            member _.Yield(_) = emptyState<'time> isDiscrete
            member _.Delay(f) = f()

            // member _.Bind
            //     ((handle, state): 'handle * ModelBuilderState<'time,'E,'L>, cont: 'handle -> ModelBuilderState<'time,'E,'L>) =
            //     cont handle

            // member _.Param<[<Measure>] 'u>(name: string, lower: float<'u>, upper: float<'u>, c: Parameter.Constraint,
            //                             st: ModelBuilderState<'time,'E,'L>)
            //     : ParamId<'u> * ModelBuilderState<'time,'E,'L> =
            //     declareParam<'u> name lower upper c st

            // member _.State(state: ModelBuilderState<'time,'E,'L>, name: string)
            //     : StateId<'u> * ModelBuilderState<'time,'E,'L> = declareState<'u> name state

            // // [<CustomOperation("parameter")>]
            // // member _.Param(state: ModelBuilderState<'time,'E,'L>, name: string, lower: float<'u>, upper: float<'u>, c: Parameter.Constraint)
            // //     : ParamId<'u> * ModelBuilderState<'time,'E,'L> = declareParam<'u> name lower upper c state

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
                { Inner = ModelBuilder.addEquationDiscrete sc.Value expr state.Inner }

            [<CustomOperation("equationRate")>]
            member _.EquationRate(state: ModelBuilderState<'time,'E,'L>, sid: StateId<'u>, expr: ModelExpression<'u/'time>)
                : ModelBuilderState<'time, Present, 'L> =
                let (StateIdInner sc) = sid
                { Inner = ModelBuilder.addEquationRate sc.Value expr state.Inner }

            [<CustomOperation("measure")>]
            member _.Measure<'E, 'L>
                (state: ModelBuilderState<'time, 'E, 'L>,
                name: string, data) =
                { Inner = Model.includeMeasure name data state.Inner }

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

    // TODO delete this module:
    module Example =

        [<Measure>] type day
        [<Measure>] type year
        [<Measure>] type g
        [<Measure>] type m
        [<Measure>] type mg
        [<Measure>] type kg
        [<Measure>] type biomass = g / m^2
        [<Measure>] type soilN = mg / kg
        [<Measure>] type perDay = 1 / day

        // State handles:
        let B = state<biomass> "biomass"
        let N = state<kg> "soilN"

        // Parameter handles:
        let K  = parameter "K" notNegative 20.<biomass> 30.<biomass>
        let r  = parameter "r" notNegative 0.1</day> 1.0</day>
        
        // Build expressions using typed handles
        let one = Constant 1.0
        let biomass   = ModelExpression.StateAt(0<Time.``time index``>, N)

        let ``dB/dt`` = ModelExpression.Parameter r * This<biomass> * (one - This<biomass> / ModelExpression.Parameter K)

        let model =
            continuousModel<day> {
                parameter r
                equationRate B ``dB/dt``
                likelihood (ModelLibrary.Likelihood.sumOfSquares [])
            }


    /// Terms for designing tests for model systems.
    module Test =

        let defaultSettings = Bristlecone.Test.TestSettings.Default

        /// If the start value has already been set, it will be overwritten with the new value.
        let withStartValue code value (settings: Bristlecone.Test.TestSettings<'state, _, _, _>) =
            match ShortCode.create code with
            | Some c ->
                { settings with
                    StartValues = settings.StartValues |> Map.add c value }
            | None -> failwithf "'%s' is not a valid short code" code

        let run settings = Bristlecone.testModel

    /// A component with its own parameters and an expression generator
    type PluggableComponent =
        { Parameters : CodedMap<Parameter.Pool.AnyParameter>
          Transform  : Untyped.ModelExpressionUntyped -> Untyped.ModelExpressionUntyped }

    /// Creates a nested component that can be inserted into a base model.
    let subComponent name transform = name, { Parameters = Map.empty; Transform = transform }

    let modelComponent name list = name, list

    let estimateParameter<[<Measure>] 'u>
        name constraintMode (lower: float<'u>) (upper: float<'u>)
        (compName, comp: PluggableComponent) =
        match Parameter.create constraintMode lower upper with
        | Some p ->
            match code name with
            | Some c ->
                let boxed = Parameter.Pool.boxParam<'u> name p
                compName, { comp with Parameters = comp.Parameters |> Map.add c boxed }
            | None -> failwithf "Invalid code '%s'" name
        | None -> failwithf "Invalid bounds %A - %A" lower upper


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
            
        // // The pluggable function you want to pass through the pipeline
        // type Transform = ModelExpression -> ModelExpression

        // // Builder that expects a single Transform and returns the next staged builder or a finished model.
        // // Keep it generic so it matches whatever you already have downstream.
        // type Builder<'next> = Transform -> Writer.Writer<'next, ComponentName * CodedMap<Parameter.Parameter>>

        // // Some builders take a named transform (e.g., pick which hole to fill by name)
        // type NamedBuilder<'next> = (string * Transform) -> Writer.Writer<'next, ComponentName * CodedMap<Parameter.Parameter>>

        // let private makeName comp impl =
        //     { Component = fst comp; Implementation = impl }

        // /// <summary>Implement a component where a model system requires one. A component is a part of a model that may be varied, for example between competing hypotheses.</summary>
        // /// <param name="comp">A tuple representing a component scaffolded with the `modelComponent` and `subComponent` functions.</param>
        // /// <param name="builder">A builder started with `createFromComponent`</param>
        // /// <typeparam name="'a"></typeparam>
        // /// <typeparam name="'b"></typeparam>
        // /// <returns>A builder to add further components or compile with `Hypothesis.compile`</returns>
        // let createFromComponent
        //     (compName: string, impls: (string * PluggableComponent) list)
        //     (builder: Builder<'next>) =
        //     if List.isEmpty impls then
        //         failwithf "You must specify at least one implementation for the '%s' component" compName
        //     impls
        //     |> List.map (fun (implName, c) ->
        //         Writer.bind (builder c.Transform, (nameOf compName implName, c.Parameters)))

        // /// <summary>Add a second or further component to a model system where one is still required.</summary>
        // /// <param name="comp">A tuple representing a component scaffolded with the `modelComponent` and `subComponent` functions.</param>
        // /// <param name="builder">A builder started with `createFromComponent`</param>
        // /// <typeparam name="'a"></typeparam>
        // /// <typeparam name="'b"></typeparam>
        // /// <returns>A builder to add further components or compile with `Hypothesis.compile`</returns>
        // let useAnother
        //     (compName: string, impls: (string * PluggableComponent) list)
        //     (builders: Writer.Writer<Builder<'next>, ComponentName * CodedMap<Parameter.Parameter>> list) =
        //     List.allPairs impls builders
        //     |> List.map (fun ((implName, c), model) ->
        //         model
        //         |> Writer.flatMap (fun nextBuilder ->
        //             Writer.bind (nextBuilder c.Transform, (nameOf compName implName, c.Parameters))))
        
        // /// <summary>Add a second or further component to a model system where one is still required.</summary>
        // /// <param name="comp">A tuple representing a component scaffolded with the `modelComponent` and `subComponent` functions.</param>
        // /// <param name="builder">A builder started with `createFromComponent`</param>
        // /// <typeparam name="'a"></typeparam>
        // /// <typeparam name="'b"></typeparam>
        // /// <returns>A builder to add further components or compile with `Hypothesis.compile`</returns>
        // let useAnotherWithName
        //     (compName: string, impls: (string * PluggableComponent) list)
        //     (builders: Writer.Writer<NamedBuilder<'next>, ComponentName * CodedMap<Parameter.Parameter>> list) =
        //     List.allPairs impls builders
        //     |> List.map (fun ((implName, c), model) ->
        //         model
        //         |> Writer.flatMap (fun nextNamedBuilder ->
        //             Writer.bind (nextNamedBuilder (implName, c.Transform), (nameOf compName implName, c.Parameters))))
        
        // /// <summary>Adds parameters from model components into the base model builder.</summary>
        // let internal addParameters
        //     (modelBuilder: ModelBuilder.ModelBuilder<'state>)
        //     (newParams: (ComponentName * CodedMap<Parameter.Pool.AnyParameter>) list) =
        //     newParams
        //     |> List.collect (snd >> Map.toList)
        //     |> List.fold (fun mb ((name: ShortCode.ShortCode), p) ->
        //         mb |> ModelBuilder.add name.Value (ModelBuilder.ParameterFragment p)
        //     ) modelBuilder
        
        // /// <summary>Compiles a suite of competing model hypotheses based on the given components.
        // /// The compilation includes only the required parameters in each model hypothesis,
        // /// and combines all labels into a single model identifier.</summary>
        // /// <param name="builder">A builder started with `createFromComponent`</param>
        // /// <returns>A list of compiled hypotheses for this model system and specified components.</returns>
        // let compile (builder: Writer.Writer<ModelBuilder.ModelBuilder<'state>, ComponentName * CodedMap<Parameter.Parameter>> list) =
        //     if builder |> List.isEmpty then
        //         failwith "No hypotheses specified"

        //     builder
        //     |> List.map (fun h ->
        //         let names = run h

        //         (names |> addParameters |> Model.compile, names |> snd |> List.map fst)
        //         |> Hypothesis)



// type Hypotheses<'subject, 'hypothesis> =
//     | Hypotheses of Language.Hypotheses.Hypothesis<'subject, 'hypothesis> seq

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
