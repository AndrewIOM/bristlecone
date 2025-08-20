module Bristlecone.Data.PalaeoProviderTests

open Expecto

type NOAATest = Bristlecone.Data.Palaeo.NoaaProvider<"/Volumes/Server HD/GitHub Projects/bristlecone/src/Bristlecone.Dendro/data-types/noaa-template-4.txt">

[<Tests>]
let typeProviderTests =
    testList
        "NOAA type provider"
        [

        // testCase "Default constructor should create instance" <| fun _ -> 
        //     Expect.equal "My internal state" (MyType().InnerState) ""

        testCase "Can access properties of erase provider" <| fun _ ->             

            Expect.hasLength NOAATest.Columns 4 "Not correct number of columns"

        ]




// [<Test>]
// let ``Constructor with parameter should create instance`` () =
//     Assert.AreEqual("override", MyType("override").InnerState)

// [<Test>]
// let ``Method with ReflectedDefinition parameter should get its name`` () =
//     let myValue = 2
//     Assert.AreEqual("myValue", MyType.NameOf(myValue))

// type Generative4 = Bristlecone.Data.PalaeoProvider.GenerativeProvider<4>

// [<Test>]
// let ``Can access properties of generative provider 2`` () =
//     let obj = Generative2()
//     Assert.AreEqual(obj.Property1, 1)
//     Assert.AreEqual(obj.Property2, 2)

// [<Test>]
// let ``Can access properties of generative provider 4`` () =
//     let obj = Generative4()
//     Assert.AreEqual(obj.Property1, 1)
//     Assert.AreEqual(obj.Property2, 2)
//     Assert.AreEqual(obj.Property3, 3)
//     Assert.AreEqual(obj.Property4, 4)


[<EntryPoint>]
let main argv =
    Tests.runTestsInAssemblyWithCLIArgs [] argv
