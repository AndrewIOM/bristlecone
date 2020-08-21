namespace Bristlecone

/// An F# Domain Specific Language (DSL) for scripting with
/// Bristlecone.
module Language =

    /// Define an estimatable parameter for a Bristlecone model.
    let parameter = Parameter.create

    /// A short code representation of an identifier for a parameter,
    /// model equation, or other model component.
    let code = ShortCode.create

    let lookup name (map:CodedMap<float>) = 
        match map |> Map.tryFindBy (fun k -> k.Value = name) with
        | Some k -> k
        | None -> invalidOp (sprintf "Could not find %s in the map" name)

    let (|NotEmptyList|_|) list =
        if list |> List.isEmpty then None else Some list

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
    | Arbitrary of (float -> float -> Parameter.Pool -> CodedMap<float> -> float)
    | Mod of ModelExpression * float
    | Exponent of ModelExpression * float

    with
        static member (*) (e1, e2) = Multiply[e1; e2]
        static member (/) (e1, e2) = Divide(e1, e2)
        static member (+) (e1, e2) = Add[e1; e2]
        static member (-) (e1, e2) = Subtract(e1, e2)
        static member (%) (e1, remainder) = Mod(e1, remainder)
        static member ``(**)`` (e1, pow) = Exponent(e1, pow)
        static member (~-) e = -e

    /// Computes a `ModelExpression` given the current time, value,
    /// environment, and parameter pool.
    let rec compute x t (pool:Parameter.Pool) (environment:CodedMap<float>) ex : float =
        match ex with
        | This -> x
        | Time -> t
        | Environment name ->
            match environment |> Map.tryFindBy (fun n -> n.Value = name) with
            | Some i -> i
            | None -> failwithf "The equation could not be calculated. The environmental data '%s' has not been configured." name
        | Parameter name ->
            match pool |> Parameter.Pool.tryGetEstimate name with
            | Some est -> est
            | None -> failwithf "The equation could not be calculated. The parameter '%s' has not been set up." name
        | Constant n -> n
        | Add list ->
            match list with
            | NotEmptyList l -> l |> List.sumBy (compute x t pool environment)
            | _ -> failwith "List was empty"
        | Subtract (l,r) -> compute x t pool environment l - compute x t pool environment r
        | Multiply list ->
            match list with
            | NotEmptyList l -> l |> List.map (compute x t pool environment) |> List.fold (*) 1.
            | _ -> failwith "List was empty"
        | Divide (l,r) -> compute x t pool environment l / compute x t pool environment r
        | Arbitrary fn -> fn x t pool environment
        | Mod (e, m) -> (compute x t pool environment e) % m
        | Exponent (e, m) -> (compute x t pool environment e) ** m

    module Writer =

        type Writer<'a, 'L> = AWriter of 'a * List<'L>

        let bind = function
            | (v, itemLog) -> AWriter(v, [itemLog])

        let map fx = function
            | AWriter(a, log) -> AWriter(fx a, log)

        let run = function
            | AWriter(a, log) -> (a, log)

        let flatMap fx = function
            | AWriter(a, log) ->
                let (v, newLog) = fx a |> run
                AWriter (v, List.append log newLog)
            

    module ExpressionParser =

        open Writer

        /// Characterises a Bristlecone model expression by the components used within.
        let rec internal describe ex : Writer<unit,string> =
            match ex with
            | This -> bind((),"current_state")
            | Time -> bind((),"time")
            | Environment name -> bind((), "environment: " + name)
            | Parameter name -> bind((), "parameter: " + name)
            | Constant _ -> bind((),"nothing")
            | Add list -> bind((), "add") // TODO
            | Subtract (l,r) -> describe l |> flatMap (fun () -> describe r)
            | Multiply list ->
                match list with
                | NotEmptyList l -> bind((), "multiply") // TODO
                | _ -> failwith "List was empty"
            | Divide (l,r) -> describe l |> flatMap (fun () -> describe r)
            | Arbitrary fn -> bind((), "custom component - TODO may use additional parameters?")
            | Mod (e, _) -> describe e
            | Exponent (e, _) -> describe e

    /// Allows common F# functions to use Bristlecone model expressions.
    module ComputableFragment =

        // Takes the standard arguments and computes
        type ComputableFragment = float -> float -> CodedMap<float> -> CodedMap<float> -> ModelExpression

        // Represents a compute function for model expressions
        type Compute = float -> float -> CodedMap<float> -> CodedMap<float> -> ModelExpression -> float

        let apply (ex:ModelExpression) fn =
            fun x t p e -> fn (compute x t p e ex)

        let applyAgain b (fn:float -> float -> Parameter.Pool -> CodedMap<float> ->(float -> 'b)) =
            fun x t p e -> fn x t p e (compute x t p e b)

        let asBristleconeFunction = Arbitrary

    /// Scaffolds a `ModelSystem` for fitting with Bristlecone.
    module ModelBuilder =

        type ModelFragment =
            | EquationFragment of ModelExpression
            | ParameterFragment of Parameter.Parameter
            | LikelihoodFragment of ModelSystem.Likelihood
            | MeasureFragment of ModelSystem.MeasureEquation

        type ModelBuilder = private ModelBuilder of Map<ShortCode.ShortCode, ModelFragment>

        let create = Map.empty |> ModelBuilder

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
                map |> Map.toSeq 
                |> Seq.map(fun (c,f) -> match f with | LikelihoodFragment l -> Some l | _ -> None )
                |> Seq.choose id
            if likelihoods |> Seq.length <> 1 then failwith "You did not specify a likelihood function. The likelihood function is used to assess model fit."
            
            let parameters = 
                map |> Map.toSeq 
                |> Seq.map(fun (c,f) -> match f with | ParameterFragment p -> Some (c,p) | _ -> None )
                |> Seq.choose id |> Map.ofSeq |> Parameter.Pool.Pool
            let measures = 
                map |> Map.toSeq 
                |> Seq.map(fun (c,f) -> match f with | MeasureFragment m -> Some (c,m) | _ -> None )
                |> Seq.choose id |> Map.ofSeq
            let equations = 
                map |> Map.toSeq 
                |> Seq.map(fun (c,f) -> match f with | EquationFragment e -> Some (c,e) | _ -> None )
                |> Seq.choose id |> Map.ofSeq

            let requirements =
                equations
                |> Map.map (fun k v -> ExpressionParser.describe v |> Writer.run |> snd)

            // 1. Check all requirements are met (equations, measures, likelihood fn)

            // 2. Check that all estimatable parameters are used.


            // 3. Summarise each equation:
            // > Print if mass / time is required.

            { Likelihood = likelihoods |> Seq.head; 
              Parameters = parameters
              Equations = equations |> Map.map(fun _ v -> (fun pool t x env -> compute x t pool env v))
              Measures = measures }


    /// Terms for scaffolding a model system for use with Bristlecone.
    module Model =

        let empty = ModelBuilder.create
        let addEquation name eq builder = ModelBuilder.add name (ModelBuilder.EquationFragment eq) builder
        let estimateParameter name constraintMode lower upper builder = 
            match Parameter.create constraintMode lower upper with
            | Some p -> ModelBuilder.add name (ModelBuilder.ParameterFragment p) builder
            | None -> failwithf "The bounds %f - %f cannot be used to estimate a parameter. See docs." lower upper
        let includeMeasure name measure builder = ModelBuilder.add name (ModelBuilder.MeasureFragment measure) builder
        let useLikelihoodFunction likelihoodFn builder = ModelBuilder.add "likelihood" (ModelBuilder.LikelihoodFragment likelihoodFn) builder
        let compile = ModelBuilder.compile

    let noConstraints = Parameter.Constraint.Unconstrained
    let notNegative = Parameter.Constraint.PositiveOnly
    
    type PluggableComponent<'a> = {
        Parameters: CodedMap<Parameter.Parameter>
        Expression: 'a -> ModelExpression
    }

    /// Creates a nested component that can be inserted into a base model.
    let subComponent name expression = name, { Parameters = Map.empty; Expression = expression }
    let modelComponent name list = name, list
    let estimateParameter name constraintMode lower upper comp = 
        match Parameter.create constraintMode lower upper with
        | None -> failwithf "The bounds %f - %f cannot be used to estimate a parameter. See docs." lower upper
        | Some p ->
            match code name with
            | Some c -> fst comp, { snd comp with Parameters = (snd comp).Parameters |> Map.add c p }
            | None -> failwithf "The code '%s' cannot be used as an identifier. See docs." name

    module Hypotheses =

        open Writer

        type ComponentName = {
            Component: string
            Implementation: string
        }

        /// Implement a component where a model system requires one. A component is
        /// a part of a model that may be varied, for example between competing
        /// hypotheses.
        let createFromComponent comp x =
            if snd comp |> List.isEmpty then failwithf "You must specify at least one implementation for the '%s' component" (fst comp)
            let name i = { Component = fst comp; Implementation = i }
            comp |> snd |> List.map(fun (n,c) -> bind(x c.Expression, name n))

        /// Implement a second or further component on a model system where one is
        /// still required. 
        let useAnother comp x =
            let name i = { Component = fst comp; Implementation = i }
            List.allPairs (snd comp) x
            |> List.map(fun ((n,c), model) -> model |> flatMap(fun m -> bind(m c.Expression, name n)))

        /// with name
        let useAnotherWithName comp (x:list<Writer<string * (ModelExpression -> ModelExpression) -> ModelBuilder.ModelBuilder,ComponentName>>) =
            let name i = { Component = fst comp; Implementation = i }
            List.allPairs (snd comp) x
            |> List.map(fun ((n,c), model) -> model |> flatMap(fun m -> bind(m (n,c.Expression), name n)))

        let internal compileNames (names:ComponentName list) =
            names.ToString()

        /// Compiles a suite of competing model hypotheses based on the given components.
        /// The compilation includes only the required parameters in each model hypothesis, 
        /// and combines all labels into a single model identifier.
        let compile (x:Writer<ModelBuilder.ModelBuilder, ComponentName> list) =
            if x |> List.isEmpty then failwith "No hypotheses specified"
            x |> List.map (map Model.compile >> run)


    let ``predator-prey`` =

        let ``dh/dt`` = Parameter "α" * This - Parameter "β" * This * Environment "lynx"
        let ``dl/dt`` = - Parameter "γ" * This + Parameter "Δ" * Environment "hare" * This

        Model.empty
        |> Model.addEquation       "hare"   ``dh/dt``
        |> Model.addEquation       "lynx"   ``dl/dt``
        |> Model.estimateParameter "α"      noConstraints 0.10 0.60    // Natural growth rate of hares in absence of predation
        |> Model.estimateParameter "β"      noConstraints 0.10 0.60    // Death rate per encounter of hares due to predation
        |> Model.estimateParameter "Δ"      noConstraints 0.10 0.60    // Efficiency of turning predated hares into lynx
        |> Model.estimateParameter "γ"      noConstraints 0.10 0.60    // Natural death rate of lynx in the absence of food
        |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.sumOfSquares [ "hare"; "lynx" ])
        |> Model.compile


    // An example shrub model using new setup
    module TreeRing =

        /// Transform δ15N to N availability.
        let nAvailability =
            (Constant 100. * Environment "N" + Constant 309.) / Constant 359.

        /// Plant uptake of N from soil, which may be turned on or off
        let uptake f geom =
            (geom This) * This * (f nAvailability)

        /// Cumulative stem biomass
        let biomass geom nLimitation =
            Parameter "r" * (uptake nLimitation geom) - Parameter "γ[b]" * This

        let soilNitrogen geom feedback limitationName nLimitation =          
            let dndt = Parameter "λ" - Parameter "γ[N]" * nAvailability + feedback This
            if limitationName = "None" then dndt else dndt - (uptake nLimitation geom)
        
        let toRadiusMM = invalidOp "Cool" // TODO

        /// Measurement (Size) variable: stem radius
        let stemRadius lastRadius lastEnv env =
            let oldCumulativeMass = lastEnv |> lookup "bs"
            let newCumulativeMass = env |> lookup "bs"
            if (newCumulativeMass - oldCumulativeMass) > 0.
            then newCumulativeMass |> toRadiusMM
            else lastRadius

        let baseModel geom feedback (nLimitMode,nLimitation) =
            Model.empty
            |> Model.addEquation        "bs"    (biomass geom nLimitation)
            |> Model.addEquation        "N"     (soilNitrogen geom feedback nLimitMode nLimitation)
            |> Model.includeMeasure     "x"     stemRadius
            |> Model.estimateParameter  "λ"     notNegative 0.001 0.500
            |> Model.estimateParameter  "γ[N]"  notNegative 0.001 0.200
            |> Model.estimateParameter  "γ[b]"  notNegative 0.001 0.200
            |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.bivariateGaussian "x" "N")
            |> Model.estimateParameter  "ρ"     noConstraints -0.500 0.500
            |> Model.estimateParameter  "σ[x]"  notNegative 0.001 0.100
            |> Model.estimateParameter  "σ[y]"  notNegative 0.001 0.100


        // Our components:

        let chapmanRichards mass = Constant 1. - (mass / (Parameter "k" * Constant 1000.))
        let biomassLoss biomass = (Parameter "ɑ" / Constant 100.) * biomass * Parameter "γ[b]"
        let hollingDiscModelDual' a b h min =
            fun r -> 
                if (a * min) / (1. + (a * b * h * min)) < 1e-12 then nan
                else (a * r) / (1. + (a * b * h * r))
        let hollingDiscModelDual n =
            hollingDiscModelDual'
            |> ComputableFragment.apply (Parameter "a")
            |> ComputableFragment.applyAgain (Parameter "b")
            |> ComputableFragment.applyAgain (Parameter "h")
            |> ComputableFragment.applyAgain (Parameter "min")
            |> ComputableFragment.applyAgain n
            |> ComputableFragment.asBristleconeFunction

        let linear' (a:float) min =
            fun r -> if a * min < 1e-12 then nan else a * r
        let linear n =
            linear'
            |> ComputableFragment.apply (Parameter "a")
            |> ComputableFragment.applyAgain (Parameter "min")
            |> ComputableFragment.applyAgain n
            |> ComputableFragment.asBristleconeFunction

        let geometricModes = modelComponent "Geometric constraint" [
            subComponent "None" (Constant 1. |> (*))
            subComponent "Chapman-Richards" chapmanRichards
            |> estimateParameter "k" notNegative 3.00 5.00  // Asymptotic biomass (in kilograms)
        ]

        let feedbackModes = modelComponent "Plant-Soil Feedback" [
            subComponent "None" (Constant 1. |> (*))
            subComponent "Biomass Loss" biomassLoss
            |> estimateParameter "ɑ" notNegative 0.01 1.00  // N-recycling efficiency
        ]

        let limitationModes = modelComponent "N-limitation" [
            subComponent "Saturating" hollingDiscModelDual
            |> estimateParameter "a" notNegative 0.100 0.400
            |> estimateParameter "h" notNegative 0.100 0.400
            |> estimateParameter "r" notNegative 0.500 1.000
            subComponent "Linear" linear
            |> estimateParameter "a" notNegative 0.100 0.400
            |> estimateParameter "r" notNegative 0.500 1.000
            subComponent "None" (Constant 1. |> (*))
            |> estimateParameter "r" notNegative 0.500 1.000
        ]

        let hypotheses =
            baseModel
            |> Hypotheses.createFromComponent geometricModes
            |> Hypotheses.useAnother feedbackModes
            |> Hypotheses.useAnotherWithName limitationModes
            |> Hypotheses.compile

        // What to do with the model hypotheses once we have them?

        