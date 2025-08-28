namespace Bristlecone

open System
open DiffSharp
open Tensors

[<RequireQualifiedAccess>]
module Parameter =

    type ConstraintMode =
        | Transform
        | Detached

    type Constraint =
        | Unconstrained
        | PositiveOnly

    type Estimation<[<Measure>] 'u> =
        | NotEstimated of lowStartingBound: float<'u> * highStartingBound: float<'u>
        | Estimated    of estimate: float<'u>

    type Parameter<[<Measure>] 'u> =
        private Parameter of Constraint * ConstraintMode * Estimation<'u>

    // -----------------
    // Transform helpers
    // -----------------

    let internal transformIn con (x: float<'u>) : float<'u> =
        match con with
        | Unconstrained -> x
        | PositiveOnly  -> Units.floatMap exp x

    let internal transformOut con (x: float<'u>) : float<'u> =
        match con with
        | Unconstrained -> x
        | PositiveOnly  -> Units.floatMap log x

    let private unwrap (Parameter(c, m, e)) : Constraint * ConstraintMode * Estimation<'u> = c, m, e

    let internal isValidParamValue con (x: float<'u>) =
        let num = float x
        not (Double.IsInfinity num)
        && not (Double.IsNaN num)
        && (con <> PositiveOnly || num > 0.)

    // -----------------
    // Constructors
    // -----------------

    let create con (bound1: float<'u>) (bound2: float<'u>) =
        if isValidParamValue con bound1
            && isValidParamValue con bound2
            && match con with
                | PositiveOnly -> bound1 > 0.<_> && bound2 > 0.<_>
                | _ -> true
            then
                let min = min bound1 bound2
                let max = max bound1 bound2
                Parameter(con, Transform, NotEstimated(min, max)) |> Some
        else None

    // -----------------
    // State checks
    // -----------------

    let isEstimated parameter =
        let _, _, estimate = parameter |> unwrap
        match estimate with
        | NotEstimated _ -> false
        | Estimated _ -> true

    let getEstimate parameter =
        let _, _, estimate = parameter |> unwrap
        match estimate with
        | NotEstimated _ -> Error "Has not been estimated yet"
        | Estimated v    -> Ok v

    let bounds p =
        let c, m, estimate = p |> unwrap
        match estimate with
        | Estimated _ -> None
        | NotEstimated(s, e) ->
            match m with
            | Detached -> Some(s, e)
            | Transform -> Some(transformOut c s, transformOut c e)

    let internal getTransformedValue (p:Parameter<'u>) =
        let c, m, estimate = p |> unwrap
        match estimate with
        | NotEstimated _ -> invalidOp "Parameter has not been estimated"
        | Estimated v ->
            match m with
            | Detached  -> v
            | Transform -> transformOut c v

    let internal setTransformedValue parameter value =
        let con, m, _ = parameter |> unwrap
        match m with
        | Detached ->
            if isValidParamValue con value
            then Ok (Parameter(con, m, Estimated value))
            else Error (sprintf "Cannot set parameter value as %f" value)
        | Transform ->
            let realVal = transformIn con value
            if isValidParamValue con realVal
            then Ok (Parameter(con, m, Estimated realVal))
            else Error (sprintf "Cannot set parameter value as %f" value)

    let detatchConstraint (p: Parameter<'u>) : Parameter<'u> * Constraint =
        let c, _, estimate = p |> unwrap
        match estimate with
        | NotEstimated(x, y) -> Parameter(c, Detached, NotEstimated(x, y)), c
        | Estimated v        -> Parameter(c, Detached, Estimated v), c


    // -----------------
    // Pool submodule
    // -----------------
    [<RequireQualifiedAccess>]
    module Pool =

        type AnyParameter =
            private {
                Name                : string
                ToTensor            : unit -> Tensor
                FromTensor          : Tensor -> AnyParameter
                TryGetTransformed   : unit -> float<``optim-space``> option
                TryGetReal          : unit -> float option
                TryGetBounds        : unit -> (float * float) option
                TryDetach : unit -> (AnyParameter * Constraint)
            }

        let inline boxParam<[<Measure>] 'u> (name: string) (p: Parameter<'u>) : AnyParameter =
            let rec make param =
                {
                    Name = name

                    ToTensor = fun () ->
                        dsharp.scalar (float (getTransformedValue param))

                    FromTensor = fun t ->
                        let v = LanguagePrimitives.FloatWithMeasure<'u> (float t)
                        match setTransformedValue param v with
                        | Ok p' -> make p'
                        | Error msg -> invalidOp msg

                    TryGetTransformed = fun () ->
                        if isEstimated param
                        then Some (float (getTransformedValue param) |> (*) 1.<``optim-space``>)
                        else None

                    TryGetReal = fun () ->
                        match getEstimate param with
                        | Ok v    -> Some (float v)
                        | Error _ -> None

                    TryGetBounds = fun () ->
                        match bounds param with
                        | Some (lo, hi) -> Some (float lo, float hi)
                        | None -> None

                    TryDetach = fun () ->
                        let detachedParam, extra = detatchConstraint param
                        make detachedParam, extra
                }
            make p

        type ParameterPool = Pool of Map<ShortCode.ShortCode, AnyParameter>

        let toList (Pool p) = Map.toList p
        let count (Pool p) = p.Count
        let fromList list = list |> Map.ofList |> Pool

        let toTensor (pool: ParameterPool) : Tensor =
            pool
            |> toList
            |> List.map (fun (_, anyP) -> anyP.ToTensor())
            |> dsharp.stack

        let fromTensor (template: ParameterPool) (theta: Tensor) : ParameterPool =
            if count template <> theta.shape.[0] then
                invalidOp "Parameter count and tensor length do not match"
            else
                template
                |> toList
                |> List.mapi (fun i (name, anyP) -> name, anyP.FromTensor theta.[i])
                |> fromList

        /// Retrieves the bounds for un-estimated parameters in the pool
        /// in the form required by optimisation functions.
        let toDomain (optimisationConstraints: Constraint list) (Pool p) =
            let boundsList =
                p
                |> Map.toList
                |> List.choose (fun (_, anyP) -> anyP.TryGetBounds())
            let x, y = boundsList |> List.unzip

            if x.Length <> y.Length || y.Length <> p.Count then
                failwith "Could not convert pool to domain"

            List.zip3 x y optimisationConstraints |> List.toArray

        /// Gets the transformed (optimisation-space) value for the given key
        let tryGetTransformedValue key (Pool p) =
            p
            |> Map.tryFind key
            |> Option.bind (fun anyP -> anyP.TryGetTransformed())

        /// Gets the real-space (untransformed) value for the given key
        let tryGetRealValue key (Pool p) : float option =
            p
            |> Map.tryFind key
            |> Option.bind (fun anyP -> anyP.TryGetReal())

        /// Flips all parameters in the erased pool to `Detached` mode
        /// instead of `Transformed` mode.
        let detatchConstraints (Pool p) =
            let updatedEntries, extras =
                p
                |> Map.toList
                |> List.map (fun (k, anyP) ->
                    // We'll need a way to detach within AnyParameter.
                    // Let's assume AnyParameter has TryDetach : unit -> (AnyParameter * 'extra)
                    match anyP.TryDetach() with
                    | newParam, extra ->
                        (k, newParam), extra
                )
                |> List.unzip

            Pool (Map.ofList updatedEntries), extras

        /// Updates all parameters in the pool from a vector of optimiser‐space values,
        /// producing a new pool whose parameters have those values set (in real space internally).
        let fromPointInTransformedSpace (Pool p as template) (point: TypedTensor<Vector,``optim-space``>) : ParameterPool =
            if count template <> point.Value.shape.[0] then
                invalidOp "The number of parameters estimated differs from those in the parameter pool"
            else
                p
                |> Map.toList
                |> List.mapi (fun i (name, anyP) ->
                    try
                        name, anyP.FromTensor point.Value.[i]
                    with ex ->
                        printfn "Error updating parameter '%s': %s" name.Value ex.Message
                        // keep original if update fails
                        name, anyP
                )
                |> Map.ofList
                |> Pool


    // /// to be estimated within an analysis.
    // [<RequireQualifiedAccess>]
    // module Pool =

    //     /// Returns Some value if a parameter with the `key`
    //     /// exists in the Pool. The value returned is transformed
    //     /// for an unconstrained parameter space.
    //     let internal tryGetTransformedValue key (pool: ParameterPool) : float option =
    //         pool
    //         |> toList
    //         |> List.tryFind (fun (x, _) -> x.Value = key)
    //         |> Option.map snd
    //         |> Option.map getTransformedValue

    //     /// Gets the 'real' / non-transformed value for use in model
    //     /// calculation.
    //     let internal tryGetRealValue key (pool: ParameterPool) : float option =
    //         pool
    //         |> unwrap
    //         |> Map.tryFindBy (fun k -> k.Value = key)
    //         |> Option.bind (getEstimate >> Result.toOption)

    //     /// Returns the starting bounds in transformed parameter space if
    //     /// the parameter has not been estimated. If the parameter has already
    //     /// been estimated, returns None.
    //     let tryGetBoundsForEstimation (pool: ParameterPool) key : (float * float) option =
    //         match pool |> unwrap |> Map.tryFindBy (fun k -> k.Value = key) with
    //         | Some p -> p |> bounds
    //         | None -> None

    //     /// The number of parameters in the Pool.
    //     let count pool = (pool |> unwrap).Count

    //     let fromList list = list |> Map.ofList |> Pool

    //     let map f pool = pool |> unwrap |> Map.map f |> Pool

    //     /// Retrieves the bounds for un-estimated parameters in the `Pool`
    //     /// in the form required by optimisation functions. If one or more
    //     /// of the parameters have been estimated, an expection will be thrown.
    //     let internal toDomain (optimisationConstraints: Constraint list) pool =
    //         let x, y = pool |> toList |> List.choose (snd >> bounds) |> List.unzip

    //         if x.Length <> y.Length || y.Length <> count pool then
    //             failwith "Could not convert pool to domain"

    //         List.zip3 x y optimisationConstraints |> List.toArray

    //     let private validateLength x y =
    //         if count x = count y then
    //             y
    //         else
    //             failwith "Cannot set parameters as infinite or NaN values"

    //     /// Creates a `Pool` from a point in the transformed parameter
    //     /// space, for example from optimisation.
    //     let internal fromPointInTransformedSpace pool point : ParameterPool =
    //         if count pool = (point |> Array.length) then
    //             pool
    //             |> toList
    //             |> List.mapi (fun i (sc, p) -> sc, setTransformedValue p point.[i])
    //             |> List.choose (fun (c, r) ->
    //                 match r with
    //                 | Ok x -> Some(c, x)
    //                 | Error _ ->
    //                     printfn "Error in (%A, %A)" c r
    //                     None)
    //             |> fromList
    //             |> validateLength pool
    //         else
    //             invalidOp "The number of parameters estimated differs from those in the parameter pool"

    //     /// Create a Pool where all parameters are not estimated. The upper and
    //     /// lower bounds are set as the estimate from `pool`.
    //     let fromEstimated pool =
    //         let result =
    //             pool
    //             |> toList
    //             |> List.choose (fun (k, v) ->
    //                 match v |> getEstimate with
    //                 | Ok estimate ->
    //                     create (detatchConstraint v |> snd) estimate estimate
    //                     |> Option.map (fun v -> k, v)
    //                 | Error _ -> failwithf "Could not get estimate for %A" v)
    //             |> fromList

    //         if count result <> count pool then
    //             failwith "Parameter pools were of different lengths"
    //         else
    //             result
