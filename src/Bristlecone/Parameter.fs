namespace Bristlecone

open System

[<RequireQualifiedAccess>]
module Parameter =

    /// The mode in which constraints are handled by the parameter.
    /// In `Transform` mode, the parameter value is transformed
    /// when requested in 'optimisation space'. Alternatively,
    /// in `Detached` mode, the real parameter value is simply
    /// returned when requested in 'optimisation space'.
    type ConstraintMode =
        | Transform
        | Detached

    /// Limits a `Parameter` to certain value ranges by
    /// applying mathematical transformations when requested
    /// for 'optimisation space'.
    type Constraint =
        | Unconstrained
        | PositiveOnly

    type Estimation =
        | NotEstimated of lowStartingBound: float * highStartingBound: float
        | Estimated of estimate: float

    type Parameter = private Parameter of Constraint * ConstraintMode * Estimation

    /// Converts the parameter value in optimisation space
    /// into its 'real' value for storage in the parameter.
    let internal transformIn con value =
        match con with
        | Unconstrained -> value
        | PositiveOnly -> exp value

    /// Converts the 'real' parameter value into that required
    /// in optimisation space, for when optimisation algorithms
    /// have no knowledge of constraints.
    let internal transformOut con value =
        match con with
        | Unconstrained -> value
        | PositiveOnly -> log value

    let private unwrap (Parameter(c, m, e)) = c, m, e

    let internal isValidParamValue con num =
        not (System.Double.IsInfinity num)
        && not (System.Double.IsNaN num)
        && if (con = PositiveOnly) then num > 0. else true

    let internal validBounds con bound1 bound2 =
        match con with
        | PositiveOnly -> bound1 > 0. && bound2 > 0.
        | Unconstrained -> true

    /// Create an estimatable `Parameter` that may be constrained or
    /// unconstrained to a certain value range. Bristlecone draws an
    /// initial value from the given bounds. To retrieve the estimated
    /// parameter value, use `Parameter.finalise`.
    let create con bound1 bound2 =
        if
            isValidParamValue con bound1
            && isValidParamValue con bound2
            && validBounds con bound1 bound2
        then
            let min = [ bound1; bound2 ] |> Seq.min
            let max = [ bound1; bound2 ] |> Seq.max
            Parameter(con, Transform, (NotEstimated(min, max))) |> Some
        else
            None

    /// Determines if the parameter has been estimated or not.
    let isEstimated parameter =
        let _, _, estimate = parameter |> unwrap

        match estimate with
        | NotEstimated _ -> false
        | Estimated v -> true

    /// Retrieve the estimated parameter value for further analysis.
    /// Will only be `Ok` if parameter has been estimated.
    let getEstimate parameter =
        let _, _, estimate = parameter |> unwrap

        match estimate with
        | NotEstimated _ -> Error "Has not been estimated yet"
        | Estimated v -> Ok v

    /// Bounds are always stored in a parameter in raw form.
    let bounds (p: Parameter) : (float * float) option =
        let c, m, estimate = p |> unwrap

        match estimate with
        | Estimated _ -> None
        | NotEstimated(s, e) ->
            match m with
            | Detached -> Some(s, e)
            | Transform -> Some(s |> transformOut c, e |> transformOut c)

    /// Gets the estimated value in transformed parameter space.
    let internal getTransformedValue (p: Parameter) : float =
        let c, m, estimate = p |> unwrap

        match estimate with
        | NotEstimated _ -> invalidOp "Parameter has not been estimated"
        | Estimated v ->
            match m with
            | Detached -> v
            | Transform -> v |> transformOut c

    /// Set the parameter's value, where `value` is in
    /// transformed parameter space.
    let internal setTransformedValue parameter value =
        let con, m, _ = parameter |> unwrap

        match m with
        | Detached ->
            match isValidParamValue con value with
            | true -> Parameter(con, m, Estimated value) |> Ok
            | false -> Error <| sprintf "Cannot set parameter value as %f" value
        | Transform ->
            match transformIn con value with
            | v when isValidParamValue con v -> Parameter(con, m, value |> transformIn con |> Estimated) |> Ok
            | _ -> Error <| sprintf "Cannot set parameter value as %f" value

    /// Detaches any constraints such that the parameter's
    /// transformed space equals normal space.
    let detatchConstraint (p: Parameter) : Parameter * Constraint =
        let c, _, estimate = p |> unwrap

        match estimate with
        | NotEstimated(x, y) -> (Parameter(c, Detached, NotEstimated(x, y)), c)
        | Estimated v -> (Parameter(c, Detached, Estimated v), c)

    /// Contains the `ParameterPool` type, which represents the set of parameters
    /// to be estimated within an analysis.
    [<RequireQualifiedAccess>]
    module Pool =

        type ParameterPool = Pool of CodedMap<Parameter>

        let private unwrap (Pool p) = p

        let toList pool = (pool |> unwrap) |> Map.toList

        let hasParameter name pool =
            pool |> unwrap |> Map.tryFindBy (fun k -> k.Value = name)

        /// Returns Some value if a parameter with the `key`
        /// exists in the Pool. The value returned is transformed
        /// for an unconstrained parameter space.
        let internal tryGetTransformedValue key (pool: ParameterPool) : float option =
            pool
            |> toList
            |> List.tryFind (fun (x, _) -> x.Value = key)
            |> Option.map snd
            |> Option.map getTransformedValue

        /// Gets the 'real' / non-transformed value for use in model
        /// calculation.
        let internal tryGetRealValue key (pool: ParameterPool) : float option =
            pool
            |> unwrap
            |> Map.tryFindBy (fun k -> k.Value = key)
            |> Option.bind (getEstimate >> Result.toOption)

        /// Returns the starting bounds in transformed parameter space if
        /// the parameter has not been estimated. If the parameter has already
        /// been estimated, returns None.
        let tryGetBoundsForEstimation (pool: ParameterPool) key : (float * float) option =
            match pool |> unwrap |> Map.tryFindBy (fun k -> k.Value = key) with
            | Some p -> p |> bounds
            | None -> None

        /// The number of parameters in the Pool.
        let count pool = (pool |> unwrap).Count

        let fromList list = list |> Map.ofList |> Pool

        let map f pool = pool |> unwrap |> Map.map f |> Pool

        /// Retrieves the bounds for un-estimated parameters in the `Pool`
        /// in the form required by optimisation functions. If one or more
        /// of the parameters have been estimated, an expection will be thrown.
        let internal toDomain (optimisationConstraints: Constraint list) pool =
            let x, y = pool |> toList |> List.choose (snd >> bounds) |> List.unzip

            if x.Length <> y.Length || y.Length <> count pool then
                failwith "Could not convert pool to domain"

            List.zip3 x y optimisationConstraints |> List.toArray

        let private validateLength x y =
            if count x = count y then
                y
            else
                failwith "Cannot set parameters as infinite or NaN values"

        /// Creates a `Pool` from a point in the transformed parameter
        /// space, for example from optimisation.
        let internal fromPointInTransformedSpace pool point : ParameterPool =
            if count pool = (point |> Array.length) then
                pool
                |> toList
                |> List.mapi (fun i (sc, p) -> sc, setTransformedValue p point.[i])
                |> List.choose (fun (c, r) ->
                    match r with
                    | Ok x -> Some(c, x)
                    | Error _ ->
                        printfn "Error in (%A, %A)" c r
                        None)
                |> fromList
                |> validateLength pool
            else
                invalidOp "The number of parameters estimated differs from those in the parameter pool"

        /// Create a Pool where all parameters are not estimated. The upper and
        /// lower bounds are set as the estimate from `pool`.
        let fromEstimated pool =
            let result =
                pool
                |> toList
                |> List.choose (fun (k, v) ->
                    match v |> getEstimate with
                    | Ok estimate ->
                        create (detatchConstraint v |> snd) estimate estimate
                        |> Option.map (fun v -> k, v)
                    | Error _ -> failwithf "Could not get estimate for %A" v)
                |> fromList

            if count result <> count pool then
                failwith "Parameter pools were of different lengths"
            else
                result

        /// Flips all parameters in the pool to work in `Detached` mode rather
        /// than `Transformed` mode.
        let detatchConstraints pool =
            pool
            |> toList
            |> List.map (fun (k, v) ->
                let x, y = detatchConstraint v
                (k, x), y)
            |> List.unzip
            |> fun (a, b) -> a |> fromList, b

    type Pool = Pool.ParameterPool
