namespace Bristlecone

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Bristlecone.Tests")>]
[<assembly: InternalsVisibleTo("Bristlecone.Dendro")>]
do ()

module internal Units =

    let removeUnitFromInt (v: int<_>) = int v
    let removeUnitFromFloat (v: float<_>) = float v

    let floatToInt (x: float<'u>) : int<'u> =
        x |> int |> LanguagePrimitives.Int32WithMeasure

    let intToFloat (x: int<'u>) : float<'u> =
        x |> float |> LanguagePrimitives.FloatWithMeasure

    let round<[<Measure>] 'u> (x: float<'u>) : float<'u> =
        System.Math.Round(float x) |> LanguagePrimitives.FloatWithMeasure

    let floatMap<[<Measure>] 'u> fn (x:float<'u>) : float<'u> =
        fn(float x) |> LanguagePrimitives.FloatWithMeasure

    let isNan<[<Measure>] 'u> (v:float<'u>) =
        System.Double.IsNaN (removeUnitFromFloat v)

    let inInfinite<[<Measure>] 'u> (v:float<'u>) =
        System.Double.IsInfinity (removeUnitFromFloat v)

    let isFinite<[<Measure>] 'u> (v:float<'u>) =
        not (inInfinite v) && not (isNan v)

    let isNotFinite<[<Measure>] 'u> (v:float<'u>) =
        inInfinite v || isNan v


[<Measure>] type iteration
[<Measure>] type ``parameter``
[<Measure>] type ``-logL``

[<Measure>] type ``optim-space`` // bounded, real parameter space
[<Measure>] type ``optim-space-transformed``  // unconstrained transformed space


[<RequireQualifiedAccess>]
module PositiveInt =

    type PositiveInt<[<Measure>] 'm> = private PositiveInt of int<'m>

    let private (|Positive|Negative|Zero|) (num: int<_>) =
        if num > 0<_> then Positive
        elif num < 0<_> then Negative
        else Zero

    let create i =
        match i with
        | Positive -> i |> PositiveInt |> Some
        | _ -> None

    let private unwrap (PositiveInt p) = p

    type PositiveInt<'u> with
        member this.Value = unwrap this


[<RequireQualifiedAccess>]
module ShortCode =

    type ShortCode = private ShortCode of string
    let unwrap (ShortCode n) = n

    let create str =
        if System.String.IsNullOrEmpty(str) then None
        else if str.Length > 10 then None
        else str |> ShortCode |> Some

    type ShortCode with
        member this.Value = unwrap this


type CodedMap<'T> = Map<ShortCode.ShortCode, 'T>

module Conditioning =

    type Conditioning<[<Measure>] 'u> =
        | NoConditioning
        | RepeatFirstDataPoint
        | Custom of CodedMap<float<'u>>

[<RequireQualifiedAccess>]
module Seq =

    open System.Linq

    let intersect (xs: 'a seq) (ys: 'a seq) = xs.Intersect(ys)

    ///Groups two sequences together by key
    let align a b =

        let simplifyEntry (key, values) =
            let matches = [ for value in values -> snd value ]
            (key, matches)

        a |> Seq.append b |> Seq.groupBy fst |> Seq.map simplifyEntry |> Seq.toList

    let everyNth n seq =
        seq
        |> Seq.mapi (fun i el -> el, i) // Add index to element
        |> Seq.filter (fun (el, i) -> i % n = n - 1) // Take every nth element
        |> Seq.map fst // Drop index from the result


    ///**Description**
    /// Unifies two sequences into a tuple based on a string key
    let keyMatch (a: (string * 'a) seq) (b: (string * 'b) seq) =
        a
        |> Seq.choose (fun (s, x) ->
            b
            |> Seq.tryFind (fun (s2, _) -> s2 = s)
            |> Option.bind (fun f -> Some(s, x, snd f)))

    let private sqr x = x * x

    let stddev nums =
        let mean = nums |> List.average
        let variance = nums |> List.averageBy (fun x -> sqr (x - mean))
        sqrt (variance)

    let hasDuplicates seq =
        (seq |> Seq.distinct |> Seq.length) <> (seq |> Seq.length)

[<RequireQualifiedAccess>]
module Map =

    let merge group1 group2 appender =
        group1
        |> Seq.fold
            (fun (acc: Map<'a, 'b>) (KeyValue(key, values)) ->
                match acc.TryFind key with
                | Some items -> Map.add key (appender values items) acc
                | None -> Map.add key values acc)
            group2

    let tryFindBy (f: 'key -> bool) map =
        map |> Map.toSeq |> Seq.tryFind (fun (k, _) -> f k) |> Option.map snd

    let keys (map: Map<'key, 'value>) = map |> Seq.map (fun k -> k.Key)

[<RequireQualifiedAccess>]
module List =

    /// Remove a single element from a list l at index i.
    let rec remove i l =
        match (i, l) with
        | 0, x :: xs -> xs
        | i, x :: xs -> x :: remove (i - 1) xs
        | i, [] -> failwith "index out of range"

    let combine6 xs ys zs bs cs ds =
        [ for x in xs do
              for y in ys do
                  for z in zs do
                      for b in bs do
                          for c in cs do
                              for d in ds do
                                  yield (x, y, z, b, c, d) ]

    let combine5 xs ys zs bs cs =
        [ for x in xs do
              for y in ys do
                  for z in zs do
                      for b in bs do
                          for c in cs do
                              yield (x, y, z, b, c) ]

    let combine4 xs ys zs bs =
        [ for x in xs do
              for y in ys do
                  for z in zs do
                      for b in bs do
                          yield (x, y, z, b) ]

    let combine3 xs ys zs =
        [ for x in xs do
              for y in ys do
                  for z in zs do
                      yield (x, y, z) ]

    // See: https://stackoverflow.com/questions/32891307/matrix-transposition-in-f
    let flip matrix =
        match matrix with
        | [] -> []
        | x :: xs ->
            let rec loop matrix partial =
                match matrix with
                | [] -> partial
                | y :: ys ->
                    let newPartial = (y, partial) ||> List.map2 (fun x y -> x :: y)
                    loop ys newPartial

            let length = List.length x

            loop matrix (List.init length (fun _ -> []))
            |> List.map (fun x -> x |> List.rev)

[<AutoOpen>]
module Result =

    /// A result computation expression.
    /// Source: http://www.fssnip.net/7UJ/title/ResultBuilder-Computational-Expression
    type ResultBuilder() =
        let ofOption error =
            function
            | Some s -> Ok s
            | None -> Error error

        member __.Return(x) = Ok x
        member __.ReturnFrom(m: Result<_, _>) = m
        member __.Bind(m, f) = Result.bind f m
        member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f
        member __.Zero() = None
        member __.Combine(m, f) = Result.bind f m
        member __.Delay(f: unit -> _) = f
        member __.Run(f) = f ()

        member __.TryWith(m, h) =
            try
                __.ReturnFrom(m)
            with e ->
                h e

        member __.TryFinally(m, compensation) =
            try
                __.ReturnFrom(m)
            finally
                compensation ()

        member __.While(guard, f) =
            if not (guard ()) then
                Ok()
            else
                do f () |> ignore
                __.While(guard, f)

    let result = ResultBuilder()

    let succeed x = Ok x

    let fail x = Error x

    let either successFunc failureFunc twoTrackInput =
        match twoTrackInput with
        | Ok s -> successFunc s
        | Error f -> failureFunc f

    let bind f = either f fail

    let (>>=) x f = bind f x

    let switch f = f >> succeed

    let map f = either (f >> succeed) fail

    let tee f x =
        f x
        x

    let tryCatch f exnHandler x =
        try
            f x |> succeed
        with ex ->
            exnHandler ex |> fail

    let doubleMap successFunc failureFunc =
        either (successFunc >> succeed) (failureFunc >> fail)

    let plus addSuccess addFailure switch1 switch2 x =
        match ((switch1 x), (switch2 x)) with
        | Ok s1, Ok s2 -> Ok(addSuccess s1 s2)
        | Error f1, Ok _ -> Error f1
        | Ok _, Error f2 -> Error f2
        | Error f1, Error f2 -> Error(addFailure f1 f2)


    let toErrorList (result: Result<'a, 'b>) : Result<'a, 'b list> =
        match result with
        | Error e -> Error [ e ]
        | Ok c -> Ok c

    let apply f result =
        match (f, result) with
        | Ok f, Ok x -> f x |> Ok
        | Error e, Ok _
        | Ok _, Error e -> e |> Error
        | Error e1, Error e2 -> e1 |> Error

    let lift f result =
        let f' = f |> succeed
        apply f' result

    let (<*>) = apply
    let (<!>) = lift

    let retn = Ok

    let rec mapResult f list =
        let cons head tail = head :: tail

        match list with
        | [] -> retn []
        | head :: tail -> retn cons <*> (f head) <*> (mapResult f tail)

    let ofOption errorMessage o =
        match o with
        | Some o -> Ok o
        | None -> Error errorMessage

    /// If OK, return Some, otherwise None.
    let toOption r =
        match r with
        | Ok x -> Some x
        | Error _ -> None

    let forceOk r =
        match r with
        | Ok x -> x
        | Error e -> failwithf "Error occurred: %s" e


module Writer =
    type Writer<'a,'log> = AWriter of 'a * 'log list

    let run (AWriter (a, log)) = a, log
    let return' x = AWriter (x, [])
    let bind f (AWriter (a, log)) =
        let (AWriter (b, log2)) = f a
        AWriter (b, log @ log2)
    let map f w =
        let (AWriter (a, log)) = w
        AWriter (f a, log)
    let flatMap f w = bind f w
    let tell entry = AWriter ((), [entry])

    // Computation expression
    type WriterBuilder() =
        member _.Return(x) = return' x
        member _.Bind(m, f) = bind f m
        member _.Zero() = return' ()
        member _.For(seq, body) =
            seq |> Seq.fold (fun acc x -> bind (fun () -> body x) acc) (return' ())

    let writer = WriterBuilder()
