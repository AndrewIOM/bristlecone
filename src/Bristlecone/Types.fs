namespace Bristlecone

open Time.TimeSeries
open Time

[<AutoOpen>]
module Result =

    let succeed x = 
        Ok x

    let fail x = 
        Error x

    let either successFunc failureFunc twoTrackInput =
        match twoTrackInput with
        | Ok s -> successFunc s
        | Error f -> failureFunc f

    let bind f = 
        either f fail

    let (>>=) x f = 
        bind f x

    let switch f = 
        f >> succeed

    let map f = 
        either (f >> succeed) fail

    let tee f x = 
        f x; x 

    let tryCatch f exnHandler x =
        try
            f x |> succeed
        with
        | ex -> exnHandler ex |> fail

    let doubleMap successFunc failureFunc =
        either (successFunc >> succeed) (failureFunc >> fail)

    let plus addSuccess addFailure switch1 switch2 x = 
        match (switch1 x),(switch2 x) with
        | Ok s1,Ok s2 -> Ok (addSuccess s1 s2)
        | Error f1,Ok _  -> Error f1
        | Ok _ ,Error f2 -> Error f2
        | Error f1,Error f2 -> Error (addFailure f1 f2)


    let toErrorList (result:Result<'a,'b>) : Result<'a,'b list> =
        match result with
        | Error e -> Error [e]
        | Ok c -> Ok c

    let apply f result =
        match f,result with
        | Ok f, Ok x -> 
            f x |> Ok 
        | Error e, Ok _ 
        | Ok _, Error e -> 
            e |> Error
        | Error e1, Error e2 -> 
            e1 |> Error 

    let lift f result =
        let f' =  f |> succeed
        apply f' result

    let (<*>) = apply
    let (<!>) = lift

    let retn = Ok

    let rec mapResult f list =
        let cons head tail = head :: tail
        match list with
        | [] -> 
            retn []
        | head::tail ->
            retn cons <*> (f head) <*> (mapResult f tail)


// Year
[<Measure>] type year

// Millimetre
[<Measure>] type mm

[<AutoOpen>]
module GrowthSeries =

    type GrowthSeries<[<Measure>] 'u> =
    | Cumulative of TimeSeries<float<'u>>
    | Absolute of TimeSeries<float<'u>>
    | Relative of TimeSeries<float<'u>>

    let growthToTime growth =
        match growth with
        | Absolute g -> g
        | Cumulative g -> g
        | Relative g -> g


[<AutoOpen>]
module ShortCode =

    type ShortCode = private ShortCode of string
    let unwrap (ShortCode n) = n
    let create str = str |> ShortCode

    type ShortCode with
        member this.Value = unwrap this

type CodedMap<'T> = Map<ShortCode,'T>


module EnvironmentalVariables =

    type RegionalEnvironment = CodedMap<TimeSeries<float>>
    type LocalEnvironment = CodedMap<TimeSeries<float>>


type Conditioning =
| NoConditioning
| RepeatFirstDataPoint
| Custom of CodedMap<float>

module Seq =

    ///Groups two sequences together by key
    let align a b = 

        let simplifyEntry (key, values) =
            let matches = [for value in values -> snd value]
            key, matches

        a 
        |> Seq.append b
        |> Seq.groupBy fst
        |> Seq.map simplifyEntry
        |> Seq.toList

    let everyNth n seq = 
        seq |> Seq.mapi (fun i el -> el, i)              // Add index to element
            |> Seq.filter (fun (el, i) -> i % n = n - 1) // Take every nth element
            |> Seq.map fst                               // Drop index from the result


    ///**Description**
    /// Unifies two sequences into a tuple based on a string key
    let keyMatch (a:(string*'a)seq) (b:(string*'b)seq) =
        a
        |> Seq.choose (fun (s,x) -> 
            b 
            |> Seq.tryFind(fun (s2,_) -> s2 = s)
            |> Option.bind (fun f -> Some (s,x,snd f)))

    let private sqr x = x * x

    let stddev nums =
        let mean = nums |> List.average
        let variance = nums |> List.averageBy (fun x -> sqr(x - mean))
        sqrt(variance)


module Map =

    let merge group1 group2 appender = 
        group1 |> Seq.fold(fun (acc:Map<'a,'b>) (KeyValue(key, values)) -> 
                          match acc.TryFind key with
                                            | Some items -> Map.add key (appender values items) acc
                                            | None -> Map.add key values acc) group2

module List =

    let combine6 xs ys zs bs cs ds = [
        for x in xs do
        for y in ys do
        for z in zs do
        for b in bs do
        for c in cs do
        for d in ds do
        yield x, y, z, b, c, d ]

    let combine5 xs ys zs bs cs = [
        for x in xs do
        for y in ys do
        for z in zs do
        for b in bs do
        for c in cs do
        yield x, y, z, b, c ]
        
    let combine4 xs ys zs bs = [
        for x in xs do
        for y in ys do
        for z in zs do
        for b in bs do
        yield x, y, z, b ]

    let combine3 xs ys zs = [
        for x in xs do
        for y in ys do
        for z in zs do
        yield x, y, z ]