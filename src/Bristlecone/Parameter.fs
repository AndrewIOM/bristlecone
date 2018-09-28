namespace Bristlecone

module Distibution =

    open MathNet.Numerics.Distributions

    let normalFromRange min max = Normal((max - min),(max - min) / 6.)

    let normal mean stdev = Normal(mean,stdev)

[<AutoOpen>]
module Parameter =

    type EstimationStartingBounds = float * float

    type Constraint =
    | Unconstrained
    | PositiveOnly

    type Estimation =
    | NotEstimated of EstimationStartingBounds
    | Estimated of float

    type Parameter = private Parameter of Constraint * Estimation

    let private unwrap (Parameter (c,e)) = c,e

    let create con bound1 bound2 =
        Parameter (con, (NotEstimated ([bound1; bound2] |> Seq.min, [bound1; bound2] |> Seq.max)))

    let setEstimate parameter value =
        let c,_ = parameter |> unwrap
        match c with
        | Unconstrained -> Parameter (Unconstrained, Estimated value)
        | PositiveOnly -> Parameter (PositiveOnly, Estimated (exp value))

    let bounds (p:Parameter) : float * float =
        let c,estimate = p |> unwrap
        match estimate with
        | NotEstimated (s,e) ->
            match c with
            | Unconstrained -> s,e
            | PositiveOnly -> log s, log e
        | Estimated _ -> invalidOp "Already estimated"

    let getEstimate (p:Parameter) : float =
        let c,estimate = p |> unwrap
        match estimate with
        | NotEstimated _ -> invalidOp (sprintf "Oops: Parameter not available")
        | Estimated v ->
            match c with
            | Unconstrained -> v
            | PositiveOnly -> v

    let detatchConstraint (p:Parameter) : Parameter * Constraint =
        let c,estimate = p |> unwrap
        match estimate with
        | NotEstimated x -> Parameter (Unconstrained, NotEstimated x), c
        | Estimated v -> Parameter (Unconstrained, Estimated v), c


    [<AutoOpen>]
    module Pool =

        type ParameterPool = CodedMap<Parameter>

        let getEstimate key (pool:ParameterPool) : float =
            let c,estimate = pool |> Map.find (ShortCode.create key) |> unwrap
            match estimate with
            | NotEstimated _ -> invalidOp (sprintf "Oops: Parameter %s not available" key)
            | Estimated v ->
                match c with
                | Unconstrained -> v
                | PositiveOnly -> v

        let getBoundsForEstimation (pool:ParameterPool) key : float * float =
            let c,estimate = pool |> Map.find key |> unwrap
            match estimate with
            | NotEstimated (s,e) ->
                match c with
                | Unconstrained -> s,e
                | PositiveOnly -> log s, log e
            | Estimated _ -> invalidOp "Already estimated"
