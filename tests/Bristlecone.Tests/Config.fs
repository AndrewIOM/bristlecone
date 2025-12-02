module Config

open System
open Expecto
open Expecto.ExpectoFsCheck
open Bristlecone
open Bristlecone.Language
open FsCheck

// Helpers
let floatEqualTol tol actual exp message =
    Expect.floatClose tol (Units.removeUnitFromFloat actual) (Units.removeUnitFromFloat exp) message

let sequenceEqual tol (arr: float<_> seq) (expected: float<_> seq) message =
    arr
    |> Seq.iteri (fun i actual ->
        let exp = expected |> Seq.item i
        floatEqualTol Accuracy.high actual exp (sprintf "%s, position %i" message i))

let sequenceEqualTol arr expected message = sequenceEqual Accuracy.high arr expected message

let genStrings minLength maxLength =
    gen {
        let! length = Gen.choose (minLength, maxLength)
        let! chars = Gen.arrayOfLength length Arb.generate<char>
        return String chars
    }

/// Active pattern for two arrays of NormalFloat with same length > 0
let (|SameLengthArrays|_|) (best: NormalFloat[]) (other: NormalFloat[]) =
    if best.Length = other.Length && best.Length > 0 then
        Some (best, other)
    else
        None

let genTuple<'fst, 'snd> (fn: Gen<'fst>) =
    gen {
        let! length = Gen.choose (0, 100)
        let! list1 = Gen.listOfLength length fn
        let! list2 = Gen.listOfLength length (Arb.generate<'snd>)
        return Seq.zip list1 list2
    }

let genMultiList minLength maxLength =
    gen {
        let! length = Gen.choose (minLength, maxLength)
        let! list = Gen.listOfLength length Arb.generate<float>
        return list
    }

[<Measure>] type testModelUnit

type BristleconeTypesGen() =
    static member ShortCode() : Arbitrary<ShortCode.ShortCode> =
        let createCode code = ShortCode.create code |> Option.get
        genStrings 1 10 |> Gen.map createCode |> Arb.fromGen

    static member EquationList = Arb.generate<ShortCode.ShortCode> |> genTuple<ShortCode.ShortCode,ModelExpression<testModelUnit>> |> Arb.fromGen

    static member MeasureList =
        Arb.generate<ShortCode.ShortCode> |> genTuple<ShortCode.ShortCode,ModelSystem.Measurement<ModelSystem.state>> |> Arb.fromGen

    static member Pool =
        gen {
            let! n = Gen.choose (1, 100)
            let! codes = Gen.listOfLength n Arb.generate<ShortCode.ShortCode>
            let! bounds1 = Gen.listOfLength n Arb.generate<NormalFloat>
            let! bounds2 = Gen.listOfLength n Arb.generate<NormalFloat>

            return
                List.zip3 codes bounds1 bounds2
                |> List.map (fun (c, b1, b2) ->
                    c,
                    match Parameter.create noConstraints b1.Get b2.Get with
                    | Some (p: Parameter.Parameter<1>) -> Parameter.Pool.boxParam<1> c.Value p
                    | None -> failwithf "The bounds %f - %f cannot be used to estimate a parameter. See docs." b1.Get b2.Get
                )
                |> Parameter.Pool.fromList
        }
        |> Arb.fromGen

    static member Radiocarbon =
        Arb.generate<NormalFloat> |> Gen.map(fun b -> min 200000. (max 0.05 b.Get) * 1.<Bristlecone.Time.``BP (radiocarbon)``> |> Bristlecone.Time.DatingMethods.Radiocarbon) |> Arb.fromGen
        
    static member CodedMap<'snd>() =
        gen {
            let! n = Gen.choose (1, 100)
            let! codes = Gen.listOfLength n Arb.generate<ShortCode.ShortCode>
            let! data = Gen.listOfLength n Arb.generate<'snd>
            return Seq.zip codes data |> Map.ofSeq
        }

    static member Tuple<'a,'b> () = genTuple<'a,'b> Arb.generate
    
    static member Floats() : Arbitrary<float list> = genMultiList 2 1000 |> Arb.fromGen

    static member PositveInt: Arbitrary<PositiveInt.PositiveInt<1>> =
        Gen.choose (1, 5) //Int32.MaxValue)
        |> Gen.map (PositiveInt.create >> Option.get)
        |> Arb.fromGen

    static member ObservationalTimeSpan =
        Gen.choose (1, TimeSpan.TicksPerDay * int64 (365 * 200) |> int)
        |> Gen.map (int64 >> TimeSpan.FromTicks >> Bristlecone.Time.ObservationalTimeSpan.create >> Option.get)
        |> Arb.fromGen

    static member Observations: Arbitrary<(float * DateTime) list> =
        gen {
            let! length = Gen.choose (2, 100)
            let! list1 = Gen.listOfLength length Arb.generate<DateTime>
            let! list2 = Gen.listOfLength length (Arb.generate<NormalFloat> |> Gen.map (fun f -> f.Get))
            return List.zip list2 list1
        }
        |> Arb.fromGen

    static member ObservationsBP: Arbitrary<(float * float<Time.``BP (radiocarbon)``>) list> =
        gen {
            let! length = Gen.choose (2, 100)
            let! list1 = Gen.listOfLength length Arb.generate<NormalFloat> |> Gen.map(fun f -> f |> List.map(fun g -> g.Get * 1.<Time.``BP (radiocarbon)``>))
            let! list2 = Gen.listOfLength length (Arb.generate<NormalFloat> |> Gen.map (fun f -> f.Get))
            return List.zip list2 list1
        }
        |> Arb.fromGen


let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<BristleconeTypesGen> ] }
