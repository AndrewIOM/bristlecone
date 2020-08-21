namespace Bristlecone

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

type CodedMap<'T> = Map<ShortCode.ShortCode,'T>

module Conditioning =

    type Conditioning<'a> =
        | NoConditioning
        | RepeatFirstDataPoint
        | Custom of CodedMap<'a>

[<RequireQualifiedAccess>]
module Seq =

    open System.Linq

    let intersect (xs:'a seq) (ys: 'a seq) = xs.Intersect(ys)

    ///Groups two sequences together by key
    let align a b = 

        let simplifyEntry (key, values) =
            let matches = [for value in values -> snd value]
            (key, matches)

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
            |> Option.bind (fun f -> Some (s, x, snd f)))

    let private sqr x = x * x

    let stddev nums =
        let mean = nums |> List.average
        let variance = nums |> List.averageBy (fun x -> sqr(x - mean))
        sqrt(variance)

[<RequireQualifiedAccess>]
module Map =

    let merge group1 group2 appender = 
        group1 
        |> Seq.fold(
            fun (acc:Map<'a,'b>) (KeyValue(key, values)) -> 
                match acc.TryFind key with
                | Some items -> Map.add key (appender values items) acc
                | None -> Map.add key values acc) group2

    let tryFindBy (f:'key->bool) map =
        map 
        |> Map.toSeq 
        |> Seq.tryFind (fun (k,_) -> f k) 
        |> Option.map snd

[<RequireQualifiedAccess>]
module List =

    /// Remove a single element from a list l at index i.
    let rec remove i l =
        match (i, l) with
        | 0, x::xs -> xs
        | i, x::xs -> x::remove (i - 1) xs
        | i, [] -> failwith "index out of range"

    let combine6 xs ys zs bs cs ds = [
        for x in xs do
        for y in ys do
        for z in zs do
        for b in bs do
        for c in cs do
        for d in ds do
        yield (x, y, z, b, c, d) ]

    let combine5 xs ys zs bs cs = [
        for x in xs do
        for y in ys do
        for z in zs do
        for b in bs do
        for c in cs do
        yield (x, y, z, b, c) ]
        
    let combine4 xs ys zs bs = [
        for x in xs do
        for y in ys do
        for z in zs do
        for b in bs do
        yield (x, y, z, b) ]

    let combine3 xs ys zs = [
        for x in xs do
        for y in ys do
        for z in zs do
        yield (x, y, z) ]

    // See: https://stackoverflow.com/questions/32891307/matrix-transposition-in-f
    let flip matrix = 
      match matrix with
      | [] -> []
      | x::xs ->
            let rec loop matrix partial = 
                match matrix with
                | [] -> partial
                | y::ys ->
                    let newPartial = (y, partial) ||> List.map2(fun x y->x::y)
                    loop ys newPartial

            let length = List.length x
            loop matrix (List.init length (fun _ -> [] )) |> List.map (fun x -> x |> List.rev)

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
        match ((switch1 x), (switch2 x)) with
        | Ok s1,Ok s2 -> Ok (addSuccess s1 s2)
        | Error f1,Ok _  -> Error f1
        | Ok _ ,Error f2 -> Error f2
        | Error f1,Error f2 -> Error (addFailure f1 f2)


    let toErrorList (result:Result<'a,'b>) : Result<'a,'b list> =
        match result with
        | Error e -> Error [e]
        | Ok c -> Ok c

    let apply f result =
        match (f, result) with
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

[<AutoOpen>]
module ListExtensions =

    let (|Single|Empty|AllIdentical|Neither|) (lst:'a list) =
        if lst.Length = 1 then Single
        elif lst.Length = 0 then Empty
        elif List.forall (fun elem -> elem = lst.[0]) lst then
            AllIdentical
        else
            Neither
