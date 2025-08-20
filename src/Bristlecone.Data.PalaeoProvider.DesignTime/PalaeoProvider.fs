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
        [ [ "²"; "^2" ], (fun t -> UnitsOfMeasureProvider.Product(t, t))
          [ "³"; "^3" ], (fun t -> UnitsOfMeasureProvider.Product(UnitsOfMeasureProvider.Product(t, t), t))
          [ "^-1" ], (fun t -> UnitsOfMeasureProvider.Inverse(t)) ]

    /// From FSharp.Data
    let parseUnitOfMeasure (str: string) =
        let unit =
            uomTransformations
            |> List.collect (fun (suffixes, trans) -> suffixes |> List.map (fun suffix -> suffix, trans))
            |> List.tryPick (fun (suffix, trans) ->
                if str.EndsWith suffix then
                    let baseUnitStr = str.[.. str.Length - suffix.Length - 1]
                    let baseUnit = UnitsOfMeasureProvider.SI baseUnitStr

                    if isNull baseUnit then None else baseUnit |> trans |> Some
                else
                    None)

        match unit with
        | Some _ -> unit
        | None ->
            let unit = UnitsOfMeasureProvider.SI str
            if isNull unit then None else Some unit

type ColumnDefinition = {
    Name: string
    Type: InferredType option
}

[<TypeProvider>]
type NOAAPalaeoTemplateProvider(config: TypeProviderConfig) as this =
    inherit
        TypeProviderForNamespaces(
            config,
            assemblyReplacementMap =
                [ ("Bristlecone.Data.PalaeoProvider.DesignTime", "Bristlecone.Data.PalaeoProvider.Runtime") ]
        )

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

    let inferTypes (columns: array<ColumnMetadata>) (rows: seq<_>) =

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
                Array.create columns.Length "" |> Seq.singleton

        // For each row of data, infer the types of the column's values
        // using the first value. TODO use more values.
        let fieldTypes =
            rows
            |> Seq.head
            |> Seq.zip columns
            |> Seq.map (fun (column, value) ->
                let unit = UnitsOfMeasure.parseUnitOfMeasure column.Units
                let typ = dataType unit value
                { Name = column.What; Type = typ })

        fieldTypes


    let buildTypes (typeName: string) (args: obj[]) =

        let provided =
            ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, hideObjectMethods = true)

        // Parameterise the type by the file to use as a template.
        let fileName = ProvidedStaticParameter("filename", typeof<string>)

        // Resolve the filename relative to the resolution folder.
        let fileName = args.[0] :?> string
        let resolvedFilename = Path.Combine(config.ResolutionFolder, fileName)

        // Define a provided type for each row, erasing to a float[].
        let rowTy = ProvidedTypeDefinition("Row", Some typeof<float[]> )

        let fileData = System.IO.File.ReadAllLines fileName
        
        let columns =
            fileData
            |> NoaaFile.ColumnDefinitions
            |> List.map snd
            |> List.toArray

        let rows =
            fileData
            |> Array.filter (fun s -> System.Text.RegularExpressions.Regex.IsMatch(s, "^[^#](.*)"))
            |> Seq.map(fun row -> row.Split"\t")

        let colTypes =
            inferTypes columns rows |> Seq.toList

        colTypes |> List.iteri(fun i col ->
            
            let typeToUse =
                match col.Type with
                | Some t ->
                    match t with
                    | Null -> typeof<obj>
                    | Primitive (t,u,optional) -> t
                | None -> typeof<obj>

            let prop =
                ProvidedProperty(col.Name, typeToUse,
                    getterCode = fun [row] -> <@@ (%%row:float[])[i] @@>)
            
            // Add metadata that defines the property's location in the referenced file.
            prop.AddDefinitionLocation(1, i + 1, fileName)
            rowTy.AddMember prop
        )

        let m2: ProvidedProperty =
            let parseCode (args: Expr list) = <@@ colTypes @@>
            ProvidedProperty("Columns", typeof<string list>, isStatic = true, getterCode = parseCode)

        // let m2 = ProvidedProperty("Columns", columns.GetType(), (fun _ -> <@@ columns @@>), isStatic = true)
        m2 |> provided.AddMember

        // Add a parameterless constructor that loads the file that was used to define the schema.
        let ctor0 =
            ProvidedConstructor([],
                invokeCode = fun _ -> <@@ NoaaFile.Parse(System.IO.File.ReadAllText fileName) @@>)
        provided.AddMember ctor0

        // Add a constructor that takes the file name to load.
        let ctor1 = ProvidedConstructor([ProvidedParameter("filename", typeof<Stream>)],
            invokeCode = fun [filename] -> <@@ NoaaFile.Load(%%filename) @@>)
        provided.AddMember ctor1

        // Add a more strongly typed Data property, which uses the existing property at run time.
        let prop =
            ProvidedProperty("Data", typedefof<seq<_>>.MakeGenericType rowTy,
                getterCode = fun [noaaFile] -> <@@ (%%noaaFile:NoaaFile).Series @@>)
        provided.AddMember prop

        ProvidedProperty("``Indexed By Time``", typedefof<seq<_>>.MakeGenericType rowTy,
            getterCode = fun [noaaFile] -> <@@ (%%noaaFile:NoaaFile).Timelines @@>)
        |> provided.AddMember


        // Add the row type as a nested type.
        provided.AddMember rowTy

        // Static methods:

        let args = [ ProvidedParameter("text", typeof<string>) ]
        let m: ProvidedMethod =
            let parseCode (args: Expr list) = <@@ NoaaFile.Parse %%args[0] @@>
            ProvidedMethod("Parse", args, typeof<NoaaFile>, isStatic = true, invokeCode = parseCode)
        m.AddXmlDoc "Parses the specified string representation of an NOAA data file"
        m |> provided.AddMember

        // let loadMember =
        //     let parseCode (args: Expr list) = <@@ NoaaFile.Load %%args[0] @@>
        //     ProvidedMethod("Load", args, typeof<NoaaFile>, isStatic = true, invokeCode = parseCode)
        // loadMember |> provided.AddMember

        provided


    let parameters =
        [ ProvidedStaticParameter("Sample", typeof<string>, parameterDefaultValue = "") ]

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
