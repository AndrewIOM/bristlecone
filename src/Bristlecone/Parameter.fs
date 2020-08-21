namespace Bristlecone

[<RequireQualifiedAccess>]
module Parameter =

    type ConstraintMode =
        | Transform
        | Detached

    /// Limits a `Parameter` to certain value ranges within
    /// Bristlecone.
    type Constraint =
        | Unconstrained
        | PositiveOnly

    type EstimationStartingBounds = float * float

    type Estimation =
        | NotEstimated of EstimationStartingBounds
        | Estimated of float

    type Parameter = private Parameter of Constraint * ConstraintMode * Estimation

    let internal transformOut con value =
        match con with
        | Unconstrained -> value
        | PositiveOnly -> exp value

    let internal transformIn con value =
        match con with
        | Unconstrained -> value
        | PositiveOnly -> log value

    let private unwrap (Parameter (c,m,e)) = c,m,e

    let internal isValidParamValue num =
        not (System.Double.IsInfinity num) &&
        not (System.Double.IsNaN num)

    let internal validBounds con bound1 bound2 =
        match con with
        | PositiveOnly -> bound1 > 0. && bound2 > 0.
        | Unconstrained -> true

    /// Create an estimatable `Parameter` that may be constrained or
    /// unconstrained to a certain value range. Bristlecone draws an
    /// initial value from the given bounds. To retrieve the estimated
    /// parameter value, use `Parameter.finalise`.
    let create con bound1 bound2 =
        if isValidParamValue bound1 && isValidParamValue bound2 && 
            validBounds con bound1 bound2
        then
            let min = [bound1; bound2] |> Seq.min
            let max = [bound1; bound2] |> Seq.max
            Parameter (con, Transform, (NotEstimated (min, max))) |> Some
        else None

    /// Retrieve the estimated parameter value for further analysis.
    /// Will only be `Ok` if parameter has been estimated.
    let getEstimate parameter =
        let c,m,estimate = parameter |> unwrap
        match estimate with
        | NotEstimated _ -> Error "Has not been estimated yet"
        | Estimated v -> Ok v

    /// Bounds are always stored in a parameter in raw form.
    let bounds (p:Parameter) : (float * float) option =
        let c,m,estimate = p |> unwrap
        match estimate with
        | Estimated _ -> None
        | NotEstimated (s,e) ->
            match m with
            | Detached -> Some (s, e)
            | Transform -> Some (s |> transformOut c, e |> transformOut c)

    /// Gets the estimated value in transformed parameter space.
    let internal getTransformedValue (p:Parameter) : float =
        let c,m,estimate = p |> unwrap
        match estimate with
        | NotEstimated _ -> invalidOp "Parameter has not been estimated"
        | Estimated v ->
            match m with
            | Detached -> v
            | Transform -> v |> transformOut c

    /// Set the parameter's value, where `value` is in
    /// transformed parameter space.
    let internal setTransformedValue parameter value =
        if isValidParamValue value then 
            let c,m,_ = parameter |> unwrap
            match m with
            | Detached -> Parameter (c, m, Estimated value) |> Ok
            | Transform -> Parameter (c, m, value |> transformIn c |> Estimated) |> Ok
        else Error <| sprintf "Cannot set parameter value as %f" value

    /// Detaches any constraints such that the parameter's
    /// transformed space equals normal space.
    let detatchConstraint (p:Parameter) : Parameter * Constraint =
        let c,_,estimate = p |> unwrap
        match estimate with
        | NotEstimated x -> (Parameter (c, Detached, NotEstimated x), c)
        | Estimated v -> (Parameter (c, Detached, Estimated v), c)

    /// Contains the `ParameterPool` type, which represents the set of parameters
    /// to be estimated within an analysis. 
    [<RequireQualifiedAccess>]
    module Pool =

        type ParameterPool = Pool of CodedMap<Parameter>

        let private unwrap (Pool p) = p

        let toList pool = (pool |> unwrap) |> Map.toList

        let tryGetEstimate key (pool:ParameterPool) : float option =
            pool |> toList |> List.tryFind (fun (x,_) -> x.Value = key) |> Option.map snd |> Option.map getTransformedValue
            // TODO Make more efficient
            // match pool |> unwrap |> Map.tryFind (ShortCode.create key) with
            // | Some p -> p |> getEstimate
            // | None -> invalidOp (sprintf "[Parameter] The parameter %s has not been added to the parameter pool" key)

        let tryGetBoundsForEstimation (pool:ParameterPool) key : (float * float) option =
            match pool |> unwrap |> Map.tryFindBy (fun k -> k.Value = key) with
            | Some p -> p |> bounds
            | None -> None

        let count pool = (pool |> unwrap).Count

        let fromList list = list |> Map.ofList |> Pool

        let toDomain (optimisationConstraints:Constraint list) pool =
            let x,y = 
                pool
                |> toList
                |> List.choose (snd >> bounds)
                |> List.unzip
            if x.Length <> y.Length || y.Length <> count pool then failwith "Could not convert pool to domain"
            List.zip3 x y optimisationConstraints |> List.toArray

        let private validateLength x y =
            if count x = count y then y
            else failwith "Cannot set parameters as infinite or NaN values"

        let fromPoint pool point : ParameterPool =
            if count pool = (point |> Array.length)
            then
                pool 
                |> toList
                |> List.mapi (fun i (sc,p) -> sc, setTransformedValue p point.[i] )
                |> List.choose(fun (c,r) -> match r with | Ok x -> Some (c,x) | Error _ -> None)
                |> fromList
                |> validateLength pool
            else
                invalidOp "The number of parameters estimated differs from those in the parameter pool"

        let fromEstimated pool =
            let result =
                pool
                |> toList 
                |> List.choose(fun (k,v) -> create (detatchConstraint v |> snd) (v |> getTransformedValue) (v |> getTransformedValue) |> Option.map (fun v -> k,v))
                |> fromList
            if count result <> count pool 
            then failwith "Parameter pools were of different lengths"
            else result

    type Pool = Pool.ParameterPool