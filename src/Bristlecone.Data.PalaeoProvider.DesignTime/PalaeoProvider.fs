module Bristlecone.Data.PalaeoProviderImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Bristlecone.Data.PalaeoProvider.Runtime

// Put any utility helpers here
[<AutoOpen>]
module internal Helpers =
    let x = 1


type InferredType =
    | Primitive of typ: Type * unit: option<System.Type> * optional: bool
    | Null


let private activeIs fn (str: string) =
   match fn str with
   | (true, x) -> Some(x)
   | _ -> None
let (|Int|_|) str = activeIs System.Int32.TryParse str
let (|Float|_|) str = activeIs System.Double.TryParse str


/// Functions for parsing units of measure
module UnitsOfMeasure =

    /// Based on FSharp.Data
    type UnitsOfMeasureProvider =
            static member SI(str) = ProvidedMeasureBuilder.SI str

            static member Product(measure1, measure2) =
                ProvidedMeasureBuilder.Product(measure1, measure2)

            static member Inverse(denominator) : Type =
                ProvidedMeasureBuilder.Inverse(denominator)

    /// Based on FSharp.Data
    let uomTransformations =
        [ 
            [ "²"; "^2" ], (fun t -> UnitsOfMeasureProvider.Product(t, t))
            [ "³"; "^3" ], (fun t -> UnitsOfMeasureProvider.Product(UnitsOfMeasureProvider.Product(t, t), t))
            [ "^-1" ], (fun t -> UnitsOfMeasureProvider.Inverse(t))
        ]

    /// From FSharp.Data
    let parseUnitOfMeasure (str: string) =
        let unit =
            uomTransformations
            |> List.collect (fun (suffixes, trans) -> suffixes |> List.map (fun suffix -> suffix, trans))
            |> List.tryPick (fun (suffix, trans) ->
                if str.EndsWith suffix then
                    let baseUnitStr = str.[.. str.Length - suffix.Length - 1]
                    let baseUnit = UnitsOfMeasureProvider.SI baseUnitStr

                    if isNull baseUnit then
                        None
                    else
                        baseUnit |> trans |> Some
                else
                    None)

        match unit with
        | Some _ -> unit
        | None ->
            let unit = UnitsOfMeasureProvider.SI str
            if isNull unit then None else Some unit


[<TypeProvider>]
type NOAAPalaeoTemplateProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Bristlecone.Data.PalaeoProvider.DesignTime", "Bristlecone.Data.PalaeoProvider.Runtime")])

    let ns = "Bristlecone.Data.Palaeo"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<NoaaFile>.Assembly.GetName().Name = asm.GetName().Name)  


    let noaaProvTy =
        ProvidedTypeDefinition(asm, ns, "NoaaProvider", Some typeof<obj>, hideObjectMethods = true, nonNullable = true)


    let dataType unitOfMeasure value =
        match value with
        | Float _ -> Some(Primitive(typeof<float>, unitOfMeasure, false))
        | Int _ -> Some(Primitive(typeof<int>, unitOfMeasure, false))
        | _ -> Some(Primitive(typeof<string>, unitOfMeasure, false))


    let inferTypes (columns:array<ColumnMetadata>) (rows: seq<_>) =

        // Not sure why this is necessary:
        let rowsIterator = rows.GetEnumerator()
        let rows =
            if rowsIterator.MoveNext() then
                seq {
                    yield rowsIterator.Current

                    try
                        while rowsIterator.MoveNext() do
                            yield rowsIterator.Current
                    finally
                        rowsIterator.Dispose()

                    yield Array.create columns.Length ""
                }
            else
                Array.create columns.Length ""
                |> Seq.singleton

        // For each row of data, infer the types of the column's values 
        // using the first value. TODO use more values.
        let fieldTypes =
            rows
            |> Seq.head
            |> Seq.zip columns
            |> Seq.map(fun (column, value) ->
                let unit = UnitsOfMeasure.parseUnitOfMeasure column.Units
                let typ = dataType unit value
                {| Name = column.What; Type = typ |}
            )

        fieldTypes


    let buildTypes (typeName: string) (args: obj[]) =

        let provided = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, hideObjectMethods = true)

        // let fileName = args.[0] :?> string

        // let txt = System.IO.File.ReadAllText fileName
        // let noaaFile =
        //     txt
        //     |> NoaaFile.Parse

        // let columns =
        //     System.IO.File.ReadAllLines fileName
        //     |> NoaaFile.ColumnDefinitions
        //     |> List.map snd
        //     |> List.toArray

        // let colTypes =
        //     inferTypes columns []

        // let m2 = ProvidedProperty("Columns", columns.GetType(), (fun _ -> <@@ columns @@>), isStatic = true)
        // provided.AddMember(m2)

        // Generate static Parse method
        let args = [ ProvidedParameter("text", typeof<string>) ]        
        let m: ProvidedMethod =
            let parseCode (args: Expr list) = <@@ NoaaFile.Parse %%args[0] @@>
            ProvidedMethod("Parse", args, typeof<NoaaFile>, isStatic = true, invokeCode = parseCode)

        m.AddXmlDoc("Parses the specified string representation of an NOAA data file")

        provided.AddMember(m)

        // provided.AddMember(ProvidedProperty("Cool", typeof<int>, (fun _ -> <@@ 2 @@>), isStatic = true))

        // // provided.AddMember(ProvidedConstructor([], (fun _ -> <@@ failwith "not finished" @@>)))
        // // Declare a constructor.
        // let ctor =
        //     ProvidedConstructor(
        //     parameters = [],
        //     invokeCode = fun args -> <@@ System.Text.RegularExpressions.Regex("") :> obj @@>)

        // ctor.AddXmlDoc "Initializes a TEST instance"
        // provided.AddMember ctor

        provided


    let parameters =
        [ 
            ProvidedStaticParameter("Sample", typeof<string>, parameterDefaultValue = "")
        ]

    do noaaProvTy.DefineStaticParameters(parameters, buildTypes)

    do this.AddNamespace(ns, [ noaaProvTy ])


// [<TypeProvider>]
// type PalaeoProvider (config : TypeProviderConfig) as this =
//     inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("Bristlecone.Data.PalaeoProvider.DesignTime", "Bristlecone.Data.PalaeoProvider.Runtime")])

//     let ns = "Bristlecone.Data.PalaeoProvider"
//     let asm = Assembly.GetExecutingAssembly()

//     // check we contain a copy of runtime files, and are not referencing the runtime DLL
//     do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  

//     let defaultServiceUrl = "https://www.ncei.noaa.gov/access/paleo-search/study/search.xml"

//     let createType typeName (count:int) =
//         let asm = ProvidedAssembly()
//         let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

//         let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ "My internal state" :> obj @@>)
//         myType.AddMember(ctor)

//         let ctor2 = ProvidedConstructor([ProvidedParameter("InnerState", typeof<string>)], invokeCode = fun args -> <@@ (%%(args.[1]):string) :> obj @@>)
//         myType.AddMember(ctor2)

//         for i in 1 .. count do 
//             let prop = ProvidedProperty("Property" + string i, typeof<int>, getterCode = fun args -> <@@ i @@>)
//             myType.AddMember(prop)

//         let meth = ProvidedMethod("StaticMethod", [], typeof<DataSource>, isStatic=true, invokeCode = (fun args -> Expr.Value(null, typeof<DataSource>)))
//         myType.AddMember(meth)
//         asm.AddTypes [ myType ]

//         let regionsType =
//             let regionCollectionType =
//                 ProvidedTypeBuilder.MakeGenericType(typedefof<RegionCollection<_>>, [ regionType ])

//             let t =
//                 ProvidedTypeDefinition(
//                     "Regions",
//                     Some regionCollectionType,
//                     hideObjectMethods = true,
//                     nonNullable = true
//                 )

//             t.AddMembersDelayed(fun () ->
//                 [ for code, name in connection.Regions do
//                         let prop =
//                             ProvidedProperty(
//                                 name,
//                                 regionType,
//                                 getterCode =
//                                     (fun (Singleton arg) ->
//                                         <@@ ((%%arg: RegionCollection<Region>) :> IRegionCollection).GetRegion(code) @@>)
//                             )

//                         prop.AddXmlDoc(sprintf "The data for region '%s'" name)
//                         yield prop ])

//             serviceTypesType.AddMember t
//             t

//         let noaaDataServiceType =
//             let t =
//                 ProvidedTypeDefinition(
//                     "NOAAPalaeoDataService",
//                     Some typeof<NOAAPalaeoSource>,
//                     hideObjectMethods = true,
//                     nonNullable = true
//                 )

//             t.AddMembersDelayed(fun () ->
//                 [
//                     ProvidedProperty(
//                         "Regions",
//                         regionsType,
//                         getterCode =
//                             (fun (Singleton arg) -> <@@ ((%%arg: WorldBankData) :> IWorldBankData).GetRegions() @@>)
//                     )
//                 ])

//             serviceTypesType.AddMember t
//             t

//         myType.AddMembersDelayed(fun () -> [
//             let urlVal = defaultServiceUrl
//             let gdcCode _ = <@@ NOAAPalaeoSource(urlVal) @@>
//             ProvidedMethod("GetDataContext", [], noaaDataServiceType, isStatic = true, invokeCode = gdcCode)
//             ]
//         )

//         myType

//     let noaaParamType = 
//         let t = ProvidedTypeDefinition(asm, ns, "GenerativeProvider", Some typeof<obj>, isErased=false)
//         t.DefineStaticParameters( [ProvidedStaticParameter("Count", typeof<int>)], fun typeName args -> createType typeName (unbox<int> args.[0]))        
//         t
//     do
//         this.AddNamespace(ns, [noaaParamType])

