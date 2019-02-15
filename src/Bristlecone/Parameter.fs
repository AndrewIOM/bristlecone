namespace Bristlecone

[<AutoOpen>]
module Parameter =

    type ConstraintMode =
        | Transform
        | Detached

    type Constraint =
        | Unconstrained
        | PositiveOnly

    type EstimationStartingBounds = float * float

    type Estimation =
        | NotEstimated of EstimationStartingBounds
        | Estimated of float

    type Parameter = private Parameter of Constraint * ConstraintMode * Estimation

    let transformOut con value =
        match con with
        | Unconstrained -> value
        | PositiveOnly -> exp value

    let transformIn con value =
        match con with
        | Unconstrained -> value
        | PositiveOnly -> log value

    let private unwrap (Parameter (c,m,e)) = c,m,e

    let create con bound1 bound2 =
        let min = [bound1; bound2] |> Seq.min
        let max = [bound1; bound2] |> Seq.max
        Parameter (con, Transform, (NotEstimated (min, max)))

    /// Bounds are always stored in a parameter in raw form.
    let bounds (p:Parameter) : float * float =
        let c,m,estimate = p |> unwrap
        match estimate with
        | Estimated _ -> invalidOp "Already estimated"
        | NotEstimated (s,e) ->
            match m with
            | Detached -> (s, e)
            | Transform -> (s |> transformOut c, e |> transformOut c)

    let getEstimate (p:Parameter) : float =
        let c,m,estimate = p |> unwrap
        match estimate with
        | NotEstimated _ -> invalidOp "Parameter has not been estimated"
        | Estimated v ->
            match m with
            | Detached -> v
            | Transform -> v |> transformOut c

    let detatchConstraint (p:Parameter) : Parameter * Constraint =
        let c,_,estimate = p |> unwrap
        match estimate with
        | NotEstimated x -> (Parameter (c, Detached, NotEstimated x), c)
        | Estimated v -> (Parameter (c, Detached, Estimated v), c)

    let setEstimate parameter value =
        let c,m,_ = parameter |> unwrap
        match m with
        | Detached -> Parameter (c, m, Estimated value)
        | Transform -> Parameter (c, m, value |> transformIn c |> Estimated)


    [<AutoOpen>]
    module Pool =

        type ParameterPool = CodedMap<Parameter>

        let getEstimate key (pool:ParameterPool) : float =
            match pool  |> Map.tryFind (ShortCode.create key) with
            | Some p -> p |> getEstimate
            | None -> invalidOp (sprintf "[Parameter] The parameter %s has not been added to the parameter pool" key)

        let getBoundsForEstimation (pool:ParameterPool) key : float * float =
            match pool  |> Map.tryFind (ShortCode.create key) with
            | Some p -> p |> bounds
            | None -> invalidOp (sprintf "[Parameter] The parameter %s has not been added to the parameter pool" key)
