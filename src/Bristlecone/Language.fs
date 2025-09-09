namespace Bristlecone

/// An F# Domain Specific Language (DSL) for scripting with
/// Bristlecone.
module Language =

    /// Define an estimatable parameter for a Bristlecone model.
    let parameter = Parameter.create

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

    /// A model or model fragment that can be interpreted to a mathematical
    /// expression by Bristlecone.
    type ModelExpression =
        | This
        | Time
        | Environment of string
        | Parameter of string
        | Constant of float
        | Add of ModelExpression list
        | Subtract of ModelExpression * ModelExpression
        | Multiply of ModelExpression list // List must not be an empty list?
        | Divide of ModelExpression * ModelExpression
        | Arbitrary of
            (float -> float<Time.``time index``> -> Parameter.Pool.ParameterPool -> ModelSystem.ExternalEnvironment -> float) *
            list<ArbitraryRequirement>
        | Mod of ModelExpression * ModelExpression
        | Power of ModelExpression * ModelExpression
        | Logarithm of ModelExpression
        | Exponential of ModelExpression
        | Conditional of cond: ModelExpression * ifTrue: ModelExpression * ifFalse: ModelExpression
        | Label of string * ModelExpression
        | StateAt of offset:int<Time.``time index``> * string
        | Invalid

        static member (*)(e1, e2) =
            Multiply[e1
                     e2]

        static member (/)(e1, e2) = Divide(e1, e2)

        static member (+)(e1, e2) =
            Add[e1
                e2]

        static member (-)(e1, e2) = Subtract(e1, e2)
        static member (%)(e1, remainder) = Mod(e1, remainder)
        static member Pow(e1, pow) = Power(e1, pow)
        static member Log(e) = Logarithm(e)
        static member Exp(e) = Exponential(e)

        static member (~-) e =
            Multiply[e
                     Constant -1.]


    /// Compile model expressions into functions that take
    /// changing state as Tensors.
    module ExpressionCompiler =

        open DiffSharp
        open Microsoft.FSharp.Quotations
        open Microsoft.FSharp.Linq.RuntimeHelpers
        open Bristlecone.Tensors

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

        let rec private buildQuotation (ops: Ops<'r>) = function
            | Constant n        -> ops.constVal n
            | Parameter n       -> ops.parameter n
            | Environment n     -> ops.environment n
            | Time              -> ops.timeVal
            | This              -> ops.thisVal
            | Add xs            -> xs |> List.map (buildQuotation ops) |> ops.add
            | Multiply xs       -> xs |> List.map (buildQuotation ops) |> ops.mul
            | Subtract (l,r)    -> ops.sub (buildQuotation ops l, buildQuotation ops r)
            | Divide   (l,r)    -> ops.div (buildQuotation ops l, buildQuotation ops r)
            | Power    (l,r)    -> ops.pow (buildQuotation ops l, buildQuotation ops r)
            | Logarithm e       -> ops.log (buildQuotation ops e)
            | Exponential e     -> ops.exp (buildQuotation ops e)
            | Mod (l,r)         -> ops.modulo (buildQuotation ops l, buildQuotation ops r)
            | Conditional (c,t,f) ->
                ops.cond (buildQuotation ops c, buildQuotation ops t, buildQuotation ops f)
            | Label (n,m)       -> ops.label (n, buildQuotation ops m)
            | StateAt (off, nm) -> ops.stateAt (off, nm)
            | Invalid           -> ops.invalid()
            | Arbitrary _       -> failwith "Arbitrary not handled"

        // ---- Tensor ops ----
        module DslHelpers =
            let ifThenElse (cond: Tensor) (thenVal: Tensor) (elseVal: Tensor) =
                //dsharp.where(cond, thenVal, elseVal)
                failwith "not finished"

        let private tensorOps (pIndex: Map<string,int>) (eIndex: Map<string,int>)
                            (pVar: Var) (eVar: Var) (tVar: Var) (xVar: Var) = {
            constVal    = fun n -> <@ dsharp.tensor n @>
            parameter   = fun name -> <@ (%%Expr.Var pVar : Tensor).[pIndex.[name]] @>
            environment = fun name -> <@ (%%Expr.Var eVar : Tensor).[eIndex.[name]] @>
            timeVal     = <@ %%Expr.Var tVar : Tensor @>
            thisVal     = <@ %%Expr.Var xVar : Tensor @>
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
        let compile<[<Measure>] 'timeUnit> parameters envKeys expr =
            let pVar = Var("parameters", typeof<TypedTensor<Vector,``parameter``>>)
            let eVar = Var("environment", typeof<CodedMap<TypedTensor<Scalar,ModelSystem.``environment``>>>)
            let tVar = Var("time", typeof<TypedTensor<Scalar,'timeUnit>>)
            let xVar = Var("thisValue", typeof<TypedTensor<Scalar,ModelSystem.state>>)
            let pIndex = paramIndex parameters
            let eIndex = envIndexFromKeys envKeys
            buildQuotation (tensorOps pIndex eIndex pVar eVar tVar xVar) expr
            |> toLambda4 pVar eVar tVar xVar
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<ModelSystem.GenericModelEquation<'timeUnit>>

        let compileFloat expr =
            let fpVar = Var("parameters", typeof<Parameter.Pool.ParameterPool>)
            let feVar = Var("environment", typeof<CodedMap<float>>)
            let ftVar = Var("time", typeof<float<Time.``time index``>>)
            let fxVar = Var("thisValue", typeof<float>)
            buildQuotation (floatOps fpVar feVar ftVar fxVar) expr
            |> toLambda4 fpVar feVar ftVar fxVar
            |> LeafExpressionConverter.EvaluateQuotation
            |> unbox<Parameter.Pool.ParameterPool -> CodedMap<float> -> float<Time.``time index``> -> float -> float>

        let compileMeasure parameters stateKeys expr =
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
        ExpressionCompiler.compile pool (Map.keys environment |> Seq.toList) ex
        |> fun f -> f (Parameter.Pool.toTensorWithKeysReal pool |> snd) environment t x


    module ExpressionParser =

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

        type ModelFragment<[<Measure>] 'state> =
            | EquationFragment of ModelExpression
            | ParameterFragment of Parameter.Pool.AnyParameter
            | LikelihoodFragment of ModelSystem.Likelihood<'state>
            | MeasureFragment of ModelSystem.Measurement<'state>

        type ModelBuilder<[<Measure>] 'state> = private ModelBuilder of CodedMap<ModelFragment<'state>> * bool

        let create<[<Measure>] 'state> isDiscrete : ModelBuilder<'state> =
            (Map.empty<ShortCode.ShortCode, ModelFragment<'state>>, isDiscrete) |> ModelBuilder

        let internal unwrap (ModelBuilder (m,isDiscrete)) = m, isDiscrete

        let add name comp builder =
            let map, isDiscrete = builder |> unwrap

            match map |> Map.tryFindBy (fun n -> n.Value = name) with
            | Some _ -> failwithf "You specified a duplicate code [%s] in your model system." name
            | None ->
                match code name with
                | Some c -> (map |> Map.add c comp, isDiscrete) |> ModelBuilder
                | None -> failwithf "The text '%s' cannot be used to make a short code identifier." name

        let compile builder : ModelSystem.ModelSystem<'state, 'timeUnit> =
            let map, isDiscrete = builder |> unwrap

            let likelihoods =
                map
                |> Map.toSeq
                |> Seq.choose (function
                    | _, LikelihoodFragment l -> Some l
                    | _ -> None)
                |> Seq.toList

            match likelihoods with
            | [l] -> ()
            | [] -> failwith "You did not specify a likelihood function. The likelihood function is used to assess model fit."
            | _ -> failwith "Multiple likelihood functions specified. Only one is allowed."

            let parameters =
                map
                |> Map.toSeq
                |> Seq.choose (function
                    | c, ParameterFragment p -> Some (c, p)
                    | _ -> None)
                |> Map.ofSeq
                |> Parameter.Pool.Pool

            let measures =
                map
                |> Map.toSeq
                |> Seq.choose (function
                    | c, MeasureFragment m -> Some (c, m)
                    | _ -> None)
                |> Map.ofSeq

            let equations =
                map
                |> Map.toSeq
                |> Seq.choose (function
                    | c, EquationFragment e -> Some (c, e)
                    | _ -> None)
                |> Map.ofSeq

            let allKeys = Seq.append (Map.keys measures) (Map.keys equations)
            if Seq.hasDuplicates allKeys then
                failwith "Duplicate keys were used within equation and measures. These must be unique."

            if Map.isEmpty equations then
                failwith "No equations specified. You must state at least one model equation."

            let reqs =
                equations
                |> Seq.collect (fun kv -> ExpressionParser.requirements kv.Value |> Writer.run |> snd)
                |> Seq.toList

            reqs
            |> List.iter (function
                | ExpressionParser.ParameterRequirement p ->
                    if not (parameters |> Parameter.Pool.toTensorWithKeysReal |> fst |> Array.exists (fun p2 -> p2.Value = p)) then
                        failwithf "The specified model requires the parameter '%s' but this has not been set up." p
                | ExpressionParser.EnvironmentRequirement _ -> () )

            let envKeys =
                reqs
                |> List.choose (function
                    | ExpressionParser.ParameterRequirement _ -> None
                    | ExpressionParser.EnvironmentRequirement e -> e |> ShortCode.create )

            let eqs =
                equations
                |> Map.map (fun _ ex -> ExpressionCompiler.compile parameters envKeys ex)
                |> fun e ->
                    if isDiscrete 
                    then ModelSystem.DifferenceEqs e
                    else ModelSystem.DifferentialEqs e

            { NegLogLikelihood = List.head likelihoods
              Parameters = parameters
              EnvironmentKeys = envKeys
              Equations = eqs
              Measures = measures }


    /// Terms for scaffolding a model system for use with Bristlecone.
    module Model =

        let empty = ModelBuilder.create false
        let discrete = ModelBuilder.create true

        let addEquation name eq builder =
            ModelBuilder.add name (ModelBuilder.EquationFragment eq) builder

        // Units preserved end-to-end: Parameter.create returns Parameter<'u>; we box to AnyParameter
        let estimateParameter<[<Measure>] 'u>
            (name: string)
            (constraintMode: Parameter.Constraint)
            (lower: float<'u>)
            (upper: float<'u>)
            (builder: ModelBuilder.ModelBuilder<'state>) =
            match Parameter.create constraintMode lower upper with
            | Some (p: Parameter.Parameter<'u>) ->
                let boxed = Parameter.Pool.boxParam<'u> name p
                ModelBuilder.add name (ModelBuilder.ParameterFragment boxed) builder
            | None -> failwithf "The bounds %A - %A cannot be used to estimate a parameter. See docs." lower upper
        
        let includeMeasure name measure builder =
            ModelBuilder.add name (ModelBuilder.MeasureFragment measure) builder

        let useLikelihoodFunction likelihoodFn builder =
            ModelBuilder.add "likelihood" (ModelBuilder.LikelihoodFragment likelihoodFn) builder

        let compile = ModelBuilder.compile


    module ModelSystemDsl =

        // Structural flags
        type Missing = Missing
        type Present = Present

        /// Builder state tracks:
        /// - 'state: unit of the state variable(s) (e.g., biomass)
        /// - 'time: unit of the time index (e.g., days)
        /// - 'Kind: Discrete | Continuous (marker type)
        /// - 'HasEq / 'HasLike: compile-time presence flags
        type ModelBuilderState<[<Measure>] 'state, [<Measure>] 'time, 'HasEq, 'HasLike> =
            private { Inner : ModelBuilder.ModelBuilder<'state> }

        let emptyState<[<Measure>] 'state, [<Measure>] 'time> isDiscrete
            : ModelBuilderState<'state, 'time, Missing, Missing> =
            { Inner = ModelBuilder.create isDiscrete }

        type ModelSystemBuilder<[<Measure>] 'state, [<Measure>] 'time>(isDiscrete) =
            member _.Yield(_) = emptyState<'state, 'time> isDiscrete
            member _.Delay(f: unit -> ModelBuilderState<'state, 'time, 'E, 'L>) = f()

            /// Add one or more equations.
            [<CustomOperation("equation")>]
            member _.Equation<'HasEq, 'L>
                (state: ModelBuilderState<'state, 'time, 'HasEq, 'L>,
                name: string, expr: ModelExpression)
                : ModelBuilderState<'state, 'time, Present, 'L> =
                { Inner = Model.addEquation name expr state.Inner }

            [<CustomOperation("parameter")>]
            member _.Parameter<[<Measure>] 'u, 'E, 'L>
                (state: ModelBuilderState<'state, 'time, 'E, 'L>,
                name: string, lower: float<'u>, upper: float<'u>, constraintMode: Parameter.Constraint) =
                { Inner = Model.estimateParameter name constraintMode lower upper state.Inner }

            [<CustomOperation("measure")>]
            member _.Measure<'E, 'L>
                (state: ModelBuilderState<'state, 'time, 'E, 'L>,
                name: string, data) =
                { Inner = Model.includeMeasure name data state.Inner }

            /// Add exactly one likelihood function.
            [<CustomOperation("likelihood")>]
            member _.Likelihood<'E>
                (state: ModelBuilderState<'state, 'time, 'E, Missing>, fn)
                : ModelBuilderState<'state, 'time, 'E, Present> =
                { Inner = Model.useLikelihoodFunction fn state.Inner }

            member _.Run(state: ModelBuilderState<'state, 'time, Present, Present>) =
                ModelBuilder.compile state.Inner


    let discreteModel<[<Measure>] 'time> : ModelSystemDsl.ModelSystemBuilder<'state, 'time> =
        ModelSystemDsl.ModelSystemBuilder true

    let continuousModel<[<Measure>] 'time> : ModelSystemDsl.ModelSystemBuilder<'state, 'time> =
        ModelSystemDsl.ModelSystemBuilder false

    // TODO delete this module:
    module Example =

        [<Measure>] type mm
        [<Measure>] type day

        let example growthLimit lossRate =
            continuousModel {
                equation "m" (Parameter "r" * growthLimit This - lossRate This)
                parameter "r" 0.01<mm> 1.00<mm> Parameter.Constraint.Unconstrained
                likelihood (ModelLibrary.Likelihood.sumOfSquares ["x"])
            }

        let growthLimit s = This
        let lossRate s = This

        let discreteForm =
            discreteModel<day> {
                equation "N" (Parameter "r" * growthLimit This - lossRate This)
                parameter "r" 0.01<mm/day> 1.00<mm/day> Parameter.Constraint.Unconstrained
                likelihood (ModelLibrary.Likelihood.sumOfSquares ["N"])
            }

        let continuousForm =
            continuousModel<day> {
                equation "N" (Parameter "r" * growthLimit This - lossRate This)
                parameter "r" 0.01<mm/day> 1.00<mm/day> Parameter.Constraint.Unconstrained
                likelihood (ModelLibrary.Likelihood.sumOfSquares ["N"])
            }


    /// Terms for designing tests for model systems.
    module Test =

        let defaultSettings = Bristlecone.Test.TestSettings<_, _, _, _>.Default

        /// If the start value has already been set, it will be overwritten with the new value.
        let withStartValue code value (settings: Bristlecone.Test.TestSettings<float, _, _, _>) =
            match ShortCode.create code with
            | Some c ->
                { settings with
                    StartValues = settings.StartValues |> Map.add c value }
            | None -> failwithf "'%s' is not a valid short code" code

        let run settings = Bristlecone.testModel

    /// A component with its own parameters and an expression generator
    type PluggableComponent =
        { Parameters : CodedMap<Parameter.Pool.AnyParameter>
          Transform  : ModelExpression -> ModelExpression }

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
        type Hypothesis<[<Measure>] 'data, [<Measure>] 'timeIndex> =
            | Hypothesis of ModelSystem.ModelSystem<'data, 'timeIndex> * list<ComponentName>

            member private this.Unwrap = let (Hypothesis(m, comps)) = this in m, comps
            /// <summary>Compiles a reference code that may be used to identify (although not necessarily uniquely) this hypothesis</summary>
            /// <returns>A string in the format XX_XXX_YY_YYY... where XX_XXX is a singe component with XX the component and XXX the implementation.</returns>

            member this.ReferenceCode =
                this.Unwrap |> snd |> List.map (fun c -> c.Reference) |> String.concat "_"

            member this.Components = this.Unwrap |> snd
            member this.Model = this.Unwrap |> fst
            

        let private makeName comp impl =
            { Component = fst comp; Implementation = impl }

        /// <summary>Implement a component where a model system requires one. A component is a part of a model that may be varied, for example between competing hypotheses.</summary>
        /// <param name="comp">A tuple representing a component scaffolded with the `modelComponent` and `subComponent` functions.</param>
        /// <param name="builder">A builder started with `createFromComponent`</param>
        /// <typeparam name="'a"></typeparam>
        /// <typeparam name="'b"></typeparam>
        /// <returns>A builder to add further components or compile with `Hypothesis.compile`</returns>
        let createFromComponent comp builder =
            match snd comp with
            | [] -> failwithf "You must specify at least one implementation for '%s'" (fst comp)
            | impls ->
                impls
                |> List.map (fun (n, c) ->
                    // builder takes the expression and returns a Writer with the base model and log
                    let w = builder c.Transform
                    Writer.bind (fun mb ->
                        Writer.tell (makeName comp n, c.Parameters)
                        |> Writer.map (fun () -> mb)
                    ) w
                )

        /// <summary>Add a second or further component to a model system where one is still required.</summary>
        /// <param name="comp">A tuple representing a component scaffolded with the `modelComponent` and `subComponent` functions.</param>
        /// <param name="builder">A builder started with `createFromComponent`</param>
        /// <typeparam name="'a"></typeparam>
        /// <typeparam name="'b"></typeparam>
        /// <returns>A builder to add further components or compile with `Hypothesis.compile`</returns>
        let useAnother
            (comp: string * (string * PluggableComponent) list)
            (builders: Writer.Writer<ModelBuilder.ModelBuilder<'state>, ComponentName * CodedMap<AnyParameter>> list) =
            List.allPairs (snd comp) builders
            |> List.map (fun ((implName, c), model) ->
                model
                |> Writer.map (fun mb -> mb |> Model.addEquation "m" c.Transform)
                |> Writer.bind (fun mb ->
                    Writer.tell (makeName comp implName, c.Parameters)
                    |> Writer.map (fun () -> mb)
                )
            )

        /// <summary>Add a second or further component to a model system where one is still required.</summary>
        /// <param name="comp">A tuple representing a component scaffolded with the `modelComponent` and `subComponent` functions.</param>
        /// <param name="builder">A builder started with `createFromComponent`</param>
        /// <typeparam name="'a"></typeparam>
        /// <typeparam name="'b"></typeparam>
        /// <returns>A builder to add further components or compile with `Hypothesis.compile`</returns>
        let useAnotherWithName comp builders =
            List.allPairs (snd comp) builders
            |> List.map (fun ((n,c), model) ->
                model
                |> Writer.map (fun m -> m |> Model.addEquation "m" c.Expression)
                |> Writer.tell (makeName comp n, c.Parameters)
            )
        
        /// <summary>Adds parameters from model components into the base model builder.</summary>
        let internal addParameters
            (modelBuilder: ModelBuilder.ModelBuilder<'state>)
            (newParams: (ComponentName * CodedMap<Parameter.Pool.AnyParameter>) list) =
            newParams
            |> List.collect (snd >> Map.toList)
            |> List.fold (fun mb ((name: ShortCode.ShortCode), p) ->
                mb |> ModelBuilder.add name.Value (ModelBuilder.ParameterFragment p)
            ) modelBuilder
        
        /// <summary>Compiles a suite of competing model hypotheses based on the given components.
        /// The compilation includes only the required parameters in each model hypothesis,
        /// and combines all labels into a single model identifier.</summary>
        /// <param name="builder">A builder started with `createFromComponent`</param>
        /// <returns>A list of compiled hypotheses for this model system and specified components.</returns>
        let compile
            (builders: Writer.Writer<ModelBuilder.ModelBuilder<'state>, ComponentName * CodedMap<Parameter.Pool.AnyParameter>> list) =
            match builders with
            | [] -> failwith "No hypotheses specified"
            | bs ->
                bs
                |> List.map (fun h ->
                    let (mb, logs) = Writer.run h
                    Hypothesis (addParameters mb logs |> Model.compile,
                                logs |> List.map fst))



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
