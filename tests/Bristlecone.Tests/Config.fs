module Config

open System
open Expecto
open Expecto.ExpectoFsCheck
open Bristlecone
open Bristlecone.Language
open FsCheck

let genStrings minLength maxLength =
    gen {
        let! length = Gen.choose (minLength, maxLength)
        let! chars = Gen.arrayOfLength length Arb.generate<char>
        return String chars
    }

let genTuple<'snd> fn =
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

type BristleconeTypesGen() =
    static member ShortCode() : Arbitrary<ShortCode.ShortCode> =
        let createCode code = ShortCode.create code |> Option.get
        genStrings 1 10 |> Gen.map createCode |> Arb.fromGen

    static member EquationList = genStrings 1 10 |> genTuple<ModelExpression> |> Arb.fromGen

    static member MeasureList =
        genStrings 1 10 |> genTuple<ModelSystem.Measurement<float>> |> Arb.fromGen

    static member Pool =
        gen {
            let! n = Gen.choose (1, 100)
            let! codes = Gen.listOfLength n Arb.generate<ShortCode.ShortCode>
            let! bounds1 = Gen.listOfLength n Arb.generate<NormalFloat>
            let! bounds2 = Gen.listOfLength n Arb.generate<NormalFloat>

            return
                List.zip3 codes bounds1 bounds2
                |> List.map (fun (c, b1, b2) -> c, Parameter.create noConstraints b1.Get b2.Get |> Option.get)
                |> Parameter.Pool.fromList
        }
        |> Arb.fromGen

    static member CodedMap<'snd>() =
        gen {
            let! n = Gen.choose (1, 100)
            let! codes = Gen.listOfLength n Arb.generate<ShortCode.ShortCode>
            let! data = Gen.listOfLength n Arb.generate<'snd>
            return Seq.zip codes data |> Map.ofSeq
        }

    static member Floats() : Arbitrary<float list> = genMultiList 2 1000 |> Arb.fromGen

    static member PositveInt: Arbitrary<PositiveInt.PositiveInt<1>> =
        Gen.choose (1, 5) //Int32.MaxValue)
        |> Gen.map (PositiveInt.create >> Option.get)
        |> Arb.fromGen

    static member RealTimeSpan =
        Gen.choose (1, Int32.MaxValue)
        |> Gen.map (int64 >> TimeSpan.FromTicks >> RealTimeSpan.create >> Option.get)
        |> Arb.fromGen

    static member Observations: Arbitrary<(float * DateTime) list> =
        gen {
            let! length = Gen.choose (2, 100)
            let! list1 = Gen.listOfLength length Arb.generate<DateTime>
            let! list2 = Gen.listOfLength length (Arb.generate<NormalFloat> |> Gen.map (fun f -> f.Get))
            return List.zip list2 list1
        }
        |> Arb.fromGen


let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<BristleconeTypesGen> ] }
