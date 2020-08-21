namespace Bristlecone

[<RequireQualifiedAccess>]
module Parameter =

    type ConstraintMode =
        | Transform
        | Detached

    type Constraint =
        | Unconstrained
        | PositiveOnly

    // TODO Change starting bounds to draw from a distribution.
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

    let internal isValidBound num =
        num <> infinity && num <> -infinity &&
        not (System.Double.IsNaN num)

    let create con bound1 bound2 =
        if isValidBound bound1 && isValidBound bound2
        then
            let min = [bound1; bound2] |> Seq.min
            let max = [bound1; bound2] |> Seq.max
            Parameter (con, Transform, (NotEstimated (min, max))) |> Some
        else None

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

    /// Contains the `ParameterPool` type, which represents the set of parameters
    /// to be estimated within an analysis. 
    [<RequireQualifiedAccess>]
    module Pool =

        type ParameterPool = Pool of CodedMap<Parameter>

        let private unwrap (Pool p) = p

        let asList pool = (pool |> unwrap) |> Map.toList

        let tryGetEstimate key (pool:ParameterPool) : float option =
            pool |> asList |> List.tryFind (fun (x,_) -> x.Value = key) |> Option.map snd |> Option.map getEstimate
            // TODO Make more efficient
            // match pool |> unwrap |> Map.tryFind (ShortCode.create key) with
            // | Some p -> p |> getEstimate
            // | None -> invalidOp (sprintf "[Parameter] The parameter %s has not been added to the parameter pool" key)

        let tryGetBoundsForEstimation (pool:ParameterPool) key : float * float =
            match pool |> unwrap |> Map.tryFindBy (fun k -> k.Value = key) with
            | Some p -> p |> bounds
            | None -> invalidOp (sprintf "[Parameter] The parameter %s has not been added to the parameter pool" key)

        let count pool = (pool |> unwrap).Count

        let fromList list = list |> Map.ofList |> Pool

        let toDomain (optimisationConstraints:Constraint list) pool =
            let x,y = 
                pool
                |> asList
                |> List.map (snd >> bounds)
                |> List.unzip
            List.zip3 x y optimisationConstraints |> List.toArray

        let fromPoint pool point : ParameterPool =
            if count pool = (point |> Array.length)
            then
                pool 
                |> asList
                |> List.mapi (fun i (sc,p) -> sc, setEstimate p point.[i] )
                |> fromList
            else
                invalidOp "The number of parameters estimated differs from those in the parameter pool"

        let fromEstimated pool =
            let result =
                pool
                |> asList 
                |> List.choose(fun (k,v) -> create (detatchConstraint v |> snd) (v |> getEstimate) (v |> getEstimate) |> Option.map (fun v -> k,v))
                |> fromList
            if count result <> count pool 
            then failwith "Parameter pools were of different lengths"
            else result



    type Pool = Pool.ParameterPool