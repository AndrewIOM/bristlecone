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
        | Arbitrary of (float -> float -> Parameter.Pool -> CodedMap<float> -> float) * list<ArbitraryRequirement>
        | Mod of ModelExpression * ModelExpression
        | Power of ModelExpression * ModelExpression
        | Logarithm of ModelExpression
        | Exponential of ModelExpression
        | Conditional of ((ModelExpression -> float) -> ModelExpression)
        | Label of string * ModelExpression
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

    /// Computes a `ModelExpression` given the current time, value,
    /// environment, and parameter pool.
    let rec compute x t (pool: Parameter.Pool) (environment: CodedMap<float>) ex : float =
        match ex with
        | This -> x
        | Time -> t
        | Environment name ->
            match environment |> Map.tryFindBy (fun n -> n.Value = name) with
            | Some i -> i
            | None ->
                failwithf
                    "The equation could not be calculated. The environmental data '%s' was not available at time-step %f."
                    name
                    t
        | Parameter name ->
            match pool |> Parameter.Pool.tryGetRealValue name with
            | Some est -> est
            | None ->
                failwithf
                    "The equation could not be calculated. The parameter '%s' has not been set up (or has yet to be estimated)."
                    name
        | Constant n -> n
        | Add list ->
            match list with
            | NotEmptyList l -> l |> List.sumBy (compute x t pool environment)
            | _ -> failwith "List was empty"
        | Subtract(l, r) -> compute x t pool environment l - compute x t pool environment r
        | Multiply list ->
            match list with
            | NotEmptyList l -> l |> List.map (compute x t pool environment) |> List.fold (*) 1.
            | _ -> failwith "List was empty"
        | Divide(l, r) -> compute x t pool environment l / compute x t pool environment r
        | Arbitrary(fn, _) -> fn x t pool environment
        | Mod(e, m) -> (compute x t pool environment e) % (compute x t pool environment m)
        | Power(e, m) -> (compute x t pool environment e) ** (compute x t pool environment m)
        | Logarithm e -> log (compute x t pool environment e)
        | Exponential e -> exp (compute x t pool environment e)
        | Conditional m -> m (compute x t pool environment) |> compute x t pool environment
        | Label(_, m) -> m |> compute x t pool environment
        | Invalid -> nan


    module Writer =

        type Writer<'a, 'L> = AWriter of 'a * List<'L>

        let bind =
            function
            | (v, itemLog) -> AWriter(v, [ itemLog ])

        let map fx =
            function
            | AWriter(a, log) -> AWriter(fx a, log)

        let run =
            function
            | AWriter(a, log) -> (a, log)

        let flatMap fx =
            function
            | AWriter(a, log) ->
                let (v, newLog) = fx a |> run
                AWriter(v, List.append log newLog)


    module ExpressionParser =

        open Writer

        /// Characterises a Bristlecone model expression by the components used within.
        let rec internal describe ex : Writer<unit, string> =
            match ex with
            | This -> bind ((), "current_state")
            | Time -> bind ((), "time")
            | Environment name -> bind ((), "environment: " + name)
            | Parameter name -> bind ((), "parameter: " + name)
            | Constant _ -> bind ((), "nothing")
            | Add list -> bind ((), "add") // TODO
            | Subtract(l, r) -> describe l |> flatMap (fun () -> describe r)
            | Multiply list ->
                match list with
                | NotEmptyList l -> bind ((), "multiply") // TODO
                | _ -> failwith "List was empty"
            | Divide(l, r) -> describe l |> flatMap (fun () -> describe r)
            | Arbitrary(fn, reqs) -> bind ((), "custom component - TODO may use additional parameters?")
            | Logarithm(e) -> describe e
            | Exponential(e) -> describe e
            | Mod(e, _) -> describe e
            | Power(e, _) -> describe e
            | Invalid -> bind ((), "invalid model")
            | Conditional _ -> bind ((), "conditional element") // TODO
            | Label(_, e) -> describe e

        type Requirement =
            | ParameterRequirement of string
            | EnvironmentRequirement of string

        /// Determines the parameter and environmental data requirements of the defined model expression.
        let rec requirements ex reqs =
            match ex with
            | This -> reqs
            | Time -> reqs
            | Environment name -> EnvironmentRequirement name :: reqs
            | Parameter name -> ParameterRequirement name :: reqs
            | Constant _ -> reqs
            | Add list
            | Multiply list -> list |> List.collect (fun l -> requirements l reqs) |> List.append reqs
            | Divide(l, r)
            | Subtract(l, r) -> [ requirements l reqs; requirements r reqs; reqs ] |> List.concat
            | Arbitrary(fn, r) ->
                r
                |> List.map (fun r ->
                    match r with
                    | ArbitraryEnvironment e -> EnvironmentRequirement e
                    | ArbitraryParameter p -> ParameterRequirement p)
                |> List.append reqs
            | Mod(e, _) -> requirements e reqs
            | Power(e, _) -> requirements e reqs
            | Logarithm e -> requirements e reqs
            | Exponential e -> requirements e reqs
            | Invalid -> reqs
            | Conditional _ -> reqs
            | Label(_, e) -> requirements e reqs


    /// Allows common F# functions to use Bristlecone model expressions.
    module ComputableFragment =

        open Writer

        // Takes the standard arguments and computes
        type ComputableFragment = float -> float -> CodedMap<float> -> CodedMap<float> -> ModelExpression

        // Represents a compute function for model expressions
        type Compute = float -> float -> CodedMap<float> -> CodedMap<float> -> ModelExpression -> float

        /// Apply a Bristlecone model expression to a custom arbitrary function.
        let apply (ex: ModelExpression) fn =
            bind ((fun x t p e -> fn (compute x t p e ex)), ex)

        /// Apply additional parameters as Bristlecone expressions to an arbitrary function.
        let applyAgain ex fn =
            let fx expr =
                bind ((fun x t p e -> expr x t p e (compute x t p e ex)), ex)

            flatMap fx fn

        let asBristleconeFunction fragment = run fragment |> Arbitrary


    /// Scaffolds a `ModelSystem` for fitting with Bristlecone.
    module ModelBuilder =

        type ModelFragment =
            | EquationFragment of ModelExpression
            | ParameterFragment of Parameter.Parameter
            | LikelihoodFragment of ModelSystem.LikelihoodFn
            | MeasureFragment of ModelSystem.MeasureEquation

        type ModelBuilder = private ModelBuilder of Map<ShortCode.ShortCode, ModelFragment>

        let create = Map.empty<ShortCode.ShortCode, ModelFragment> |> ModelBuilder

        let private unwrap (ModelBuilder m) = m

        let add name comp builder =
            let map = builder |> unwrap

            match map |> Map.tryFindBy (fun n -> n.Value = name) with
            | Some _ -> failwithf "You specified a duplicate code [%s] in your model system." name
            | None ->
                match code name with
                | Some c -> map |> Map.add c comp |> ModelBuilder
                | None -> failwithf "The text '%s' cannot be used to make a short code identifier." name

        let compile builder : ModelSystem.ModelSystem =
            // Ensure only single likelihood function
            // Find all parameters

            // Check each dynamic function that it has needed parameters.
            // Check each measure has needed parameters.
            // Check if any parameters are unused.

            // 2. Environment required.
            // Compile down to a [float -> float -> Pool -> Env -> float] function, with a list of requirements.

            let map = builder |> unwrap

            let likelihoods =
                map
                |> Map.toSeq
                |> Seq.map (fun (c, f) ->
                    match f with
                    | LikelihoodFragment l -> Some l
                    | _ -> None)
                |> Seq.choose id

            if likelihoods |> Seq.length <> 1 then
                failwith
                    "You did not specify a likelihood function. The likelihood function is used to assess model fit."

            let parameters =
                map
                |> Map.toSeq
                |> Seq.map (fun (c, f) ->
                    match f with
                    | ParameterFragment p -> Some(c, p)
                    | _ -> None)
                |> Seq.choose id
                |> Map.ofSeq
                |> Parameter.Pool.Pool

            let measures =
                map
                |> Map.toSeq
                |> Seq.map (fun (c, f) ->
                    match f with
                    | MeasureFragment m -> Some(c, m)
                    | _ -> None)
                |> Seq.choose id
                |> Map.ofSeq

            let equations =
                map
                |> Map.toSeq
                |> Seq.map (fun (c, f) ->
                    match f with
                    | EquationFragment e -> Some(c, e)
                    | _ -> None)
                |> Seq.choose id
                |> Map.ofSeq

            if Seq.hasDuplicates (Seq.concat [ Map.keys measures; Map.keys equations ]) then
                failwith "Duplicate keys were used within equation and measures. These must be unique."

            if equations.IsEmpty then
                failwith "No equations specified. You must state at least one model equation."

            equations
            |> Map.map (fun _ v -> ExpressionParser.requirements v [])
            |> Map.toList
            |> List.map snd
            |> List.collect id
            |> List.distinct
            |> List.iter (fun req ->
                match req with
                | ExpressionParser.ParameterRequirement p ->
                    match parameters |> Parameter.Pool.hasParameter p with
                    | Some p -> ()
                    | None ->
                        failwithf "The specified model requires the parameter '%s' but this has not been set up." p
                | ExpressionParser.EnvironmentRequirement _ -> ())

            { NegLogLikelihood = likelihoods |> Seq.head
              Parameters = parameters
              Equations = equations |> Map.map (fun _ v -> (fun pool t x env -> compute x t pool env v))
              Measures = measures }


    /// Terms for scaffolding a model system for use with Bristlecone.
    module Model =

        let empty = ModelBuilder.create

        let addEquation name eq builder =
            ModelBuilder.add name (ModelBuilder.EquationFragment eq) builder

        let estimateParameter name constraintMode lower upper builder =
            match Parameter.create constraintMode lower upper with
            | Some p -> ModelBuilder.add name (ModelBuilder.ParameterFragment p) builder
            | None -> failwithf "The bounds %f - %f cannot be used to estimate a parameter. See docs." lower upper

        let includeMeasure name measure builder =
            ModelBuilder.add name (ModelBuilder.MeasureFragment measure) builder

        let useLikelihoodFunction likelihoodFn builder =
            ModelBuilder.add "likelihood" (ModelBuilder.LikelihoodFragment likelihoodFn) builder

        let compile = ModelBuilder.compile


    /// Terms for designing tests for model systems.
    module Test =

        let defaultSettings = Bristlecone.Test.TestSettings<float>.Default

        /// If the start value has already been set, it will be overwritten with the new value.
        let withStartValue code value (settings: Bristlecone.Test.TestSettings<float>) =
            match ShortCode.create code with
            | Some c ->
                { settings with
                    StartValues = settings.StartValues |> Map.add c value }
            | None -> failwithf "'%s' is not a valid short code" code

        let run settings = Bristlecone.testModel


    let noConstraints = Parameter.Constraint.Unconstrained
    let notNegative = Parameter.Constraint.PositiveOnly

    type PluggableComponent<'a> =
        { Parameters: CodedMap<Parameter.Parameter>
          Expression: 'a -> ModelExpression }

    /// Creates a nested component that can be inserted into a base model.
    let subComponent name expression =
        name,
        { Parameters = Map.empty
          Expression = expression }

    let modelComponent name list = name, list

    let estimateParameter name constraintMode lower upper comp =
        match Parameter.create constraintMode lower upper with
        | None -> failwithf "The bounds %f - %f cannot be used to estimate a parameter. See docs." lower upper
        | Some p ->
            match code name with
            | Some c ->
                fst comp,
                { snd comp with
                    Parameters = (snd comp).Parameters |> Map.add c p }
            | None -> failwithf "The code '%s' cannot be used as an identifier. See docs." name

    /// <summary>Types to represent a hypothesis, given that a hypothesis
    /// is a model system that contains some alternate formulations of certain
    /// components.</summary>
    [<RequireQualifiedAccess>]
    module Hypotheses =

        open Writer

        /// Takes n first letters from a string. If there are n or more words,
        /// takes the first letters of each word, else takes the letters from the
        /// first word.
        let internal nFirstLetters n (s: string) =
            let s = System.Text.RegularExpressions.Regex.Replace(s, @"[^A-Za-z0-9 ]+", "")

            if s.Split(' ').Length >= n then
                ((s.Split(' ') |> Array.map (fun s -> s |> Seq.head) |> Seq.truncate n)
                 |> System.String.Concat)
                    .ToUpper()
            else
                (s |> Seq.truncate n |> System.String.Concat).ToUpper()

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
                sprintf "%s_%s" (nFirstLetters 2 this.Component) (nFirstLetters 3 this.Implementation)

        /// <summary>A hypothesis consists of a model system and the names of the
        /// swappable components within it, alongside the name of their current implementation.</summary>
        type Hypothesis =
            | Hypothesis of ModelSystem.ModelSystem * list<ComponentName>

            member private this.unwrap = this |> fun (Hypothesis(h1, h2)) -> (h1, h2)

            /// <summary>Compiles a reference code that may be used to identify (although not necessarily uniquely) this hypothesis</summary>
            /// <returns>A string in the format XX_XXX_YY_YYY... where XX_XXX is a singe component with XX the component and XXX the implementation.</returns>
            member this.ReferenceCode =
                this.unwrap |> snd |> List.map (fun c -> c.Reference) |> String.concat "_"

            member this.Components = this.unwrap |> snd
            member this.Model = this.unwrap |> fst

        /// <summary>Implement a component where a model system requires one. A component is a part of a model that may be varied, for example between competing hypotheses.</summary>
        /// <param name="comp">A tuple representing a component scaffolded with the `modelComponent` and `subComponent` functions.</param>
        /// <param name="builder">A builder started with `createFromComponent`</param>
        /// <typeparam name="'a"></typeparam>
        /// <typeparam name="'b"></typeparam>
        /// <returns>A builder to add further components or compile with `Hypothesis.compile`</returns>
        let createFromComponent comp builder =
            if snd comp |> List.isEmpty then
                failwithf "You must specify at least one implementation for the '%s' component" (fst comp)

            let name i =
                { Component = fst comp
                  Implementation = i }

            comp
            |> snd
            |> List.map (fun (n, c) -> bind (builder c.Expression, (name n, c.Parameters)))

        /// <summary>Add a second or further component to a model system where one is still required.</summary>
        /// <param name="comp">A tuple representing a component scaffolded with the `modelComponent` and `subComponent` functions.</param>
        /// <param name="builder">A builder started with `createFromComponent`</param>
        /// <typeparam name="'a"></typeparam>
        /// <typeparam name="'b"></typeparam>
        /// <returns>A builder to add further components or compile with `Hypothesis.compile`</returns>
        let useAnother comp builder =
            let name i =
                { Component = fst comp
                  Implementation = i }

            List.allPairs (snd comp) builder
            |> List.map (fun ((n, c), model) ->
                model |> flatMap (fun m -> bind (m c.Expression, (name n, c.Parameters))))

        /// <summary>Add a second or further component to a model system where one is still required.</summary>
        /// <param name="comp">A tuple representing a component scaffolded with the `modelComponent` and `subComponent` functions.</param>
        /// <param name="builder">A builder started with `createFromComponent`</param>
        /// <typeparam name="'a"></typeparam>
        /// <typeparam name="'b"></typeparam>
        /// <returns>A builder to add further components or compile with `Hypothesis.compile`</returns>
        let useAnotherWithName comp builder =
            let name i =
                { Component = fst comp
                  Implementation = i }

            List.allPairs (snd comp) builder
            |> List.map (fun ((n, c), model) ->
                model |> flatMap (fun m -> bind (m (n, c.Expression), (name n, c.Parameters))))

        /// <summary>Adds parameters from model components into the base model builder.</summary>
        let internal addParameters
            ((modelBuilder, newParams): ModelBuilder.ModelBuilder * List<ComponentName * CodedMap<Parameter.Parameter>>)
            =
            newParams
            |> List.collect (snd >> Map.toList)
            |> List.fold
                (fun mb (name, p) -> mb |> ModelBuilder.add name.Value (ModelBuilder.ParameterFragment p))
                modelBuilder

        /// <summary>Compiles a suite of competing model hypotheses based on the given components.
        /// The compilation includes only the required parameters in each model hypothesis,
        /// and combines all labels into a single model identifier.</summary>
        /// <param name="builder">A builder started with `createFromComponent`</param>
        /// <returns>A list of compiled hypotheses for this model system and specified components.</returns>
        let compile (builder: Writer<ModelBuilder.ModelBuilder, ComponentName * CodedMap<Parameter.Parameter>> list) =
            if builder |> List.isEmpty then
                failwith "No hypotheses specified"

            builder
            |> List.map (fun h ->
                let names = run h

                (names |> addParameters |> Model.compile, names |> snd |> List.map fst)
                |> Hypothesis)


// type Hypotheses<'subject, 'hypothesis> =
//     | Hypotheses of Hypothesis<'subject, 'hypothesis> seq

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
