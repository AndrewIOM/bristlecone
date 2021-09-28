// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#if FAKE
#r "paket:
nuget FAKE.Core.Target
nuget FAKE.Core.ReleaseNotes
nuget FAKE.DotNet.Cli
nuget FAKE.DotNet.Fsi
nuget FAKE.DotNet.AssemblyInfoFile
nuget FAKE.Tools.Git
nuget FAKE.DotNet.Testing.XUnit2"
#load "./.fake/build.fsx/intellisense.fsx"
#else
#r "nuget: FAKE.Core.Target"
#r "nuget: FAKE.Core.ReleaseNotes"
#r "nuget: FAKE.DotNet.Cli"
#r "nuget: FAKE.DotNet.Fsi"
#r "nuget: FAKE.DotNet.AssemblyInfoFile"
#r "nuget: FAKE.Tools.Git"
#r "nuget: FAKE.DotNet.Testing.XUnit2"
#r "nuget: System.Reactive"
#r "nuget: MSBuild.StructuredLogger"
let execContext = Fake.Core.Context.FakeExecutionContext.Create false "build.fsx" []
Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)
#endif

open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO.FileSystemOperators
open Fake.DotNet

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let projectName = "Bristlecone"
let projectSummary = "Time-series modelling in F#"
let projectDescription = """
  An F# library for model-fitting model-selection (MFMS) of ecological 
  models to observational data. The library was developed for 
  individual-based plant modelling using tree ring time-series, 
  but can be used for any ecological models."""
let authors = "Andrew Martin"
let companyName = ""
let tags = "modelling time-series F# fsharp R data-science ecology model-fitting"
let license = "MIT"
let iconUrl = ""
let copyright = "(C) 2018 Andrew Martin"

let packageProjectUrl = "https://github.com/AndrewIOM/bristlecone"
let repositoryType = "git"
let repositoryUrl = "https://github.com/AndrewIOM/bristlecone"

// --------------------------------------------------------------------------------------
// The rest of the code is standard F# build script
// --------------------------------------------------------------------------------------

// Read release notes & version info from RELEASE_NOTES.md
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let binDir = __SOURCE_DIRECTORY__ @@ "bin"
let release = System.IO.File.ReadLines "RELEASE_NOTES.md" |> Fake.Core.ReleaseNotes.parse

// Generate assembly info files with the right version & up-to-date information
Target.create "AssemblyInfo" (fun _ ->
  let fileName = "src/Bristlecone/AssemblyInfo.fs"
  AssemblyInfoFile.createFSharpWithConfig fileName
      [ Fake.DotNet.AssemblyInfo.Title projectName
        Fake.DotNet.AssemblyInfo.Company companyName
        Fake.DotNet.AssemblyInfo.Product projectName
        Fake.DotNet.AssemblyInfo.Description projectSummary
        Fake.DotNet.AssemblyInfo.Version release.AssemblyVersion
        Fake.DotNet.AssemblyInfo.FileVersion release.AssemblyVersion ]
      (AssemblyInfoFileConfig(false))
)

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target.create "Clean" (fun _ ->
    Fake.IO.Shell.cleanDirs ["bin"; "output" ]
    Fake.IO.Shell.cleanDirs ["tests/Bristlecone.Tests/bin"; "tests/Bristlecone.Tests/obj" ]
)

Target.create "CleanDocs" (fun _ ->
    Fake.IO.Shell.cleanDirs [".fsdocs"]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Build" (fun _ ->
    Trace.log " --- Building the app --- "
    Fake.DotNet.DotNet.build id (projectName + ".sln")
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

Target.create "RunTests" (fun _ ->
    let rHome = Environment.environVarOrFail "R_HOME"
    Trace.logf "R_HOME is set as %s" rHome
    let result = Fake.DotNet.DotNet.exec (fun args -> 
        { args with Verbosity = Some Fake.DotNet.DotNet.Verbosity.Normal}) "test" (projectName + ".sln")
    if result.ExitCode <> 0 then failwith "Tests failed"
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "NuGet" (fun _ ->
    // Format the description to fit on a single line (remove \r\n and double-spaces)
    let projectDescription = projectDescription.Replace("\r", "").Replace("\n", "").Replace("  ", " ")
    
    // Format the release notes
    let releaseNotes = release.Notes |> String.concat "\n"

    let properties = [
        ("Version", release.NugetVersion)
        ("Authors", authors)
        ("PackageProjectUrl", packageProjectUrl)
        ("PackageTags", tags)
        ("RepositoryType", repositoryType)
        ("RepositoryUrl", repositoryUrl)
        ("PackageLicenseExpression", license)
        ("PackageRequireLicenseAcceptance", "false")
        ("PackageReleaseNotes", releaseNotes)
        ("Summary", projectSummary)
        ("PackageDescription", projectDescription)
        ("PackageIcon", "logo.png")
        ("PackageIconUrl", iconUrl)
        ("EnableSourceLink", "true")
        ("PublishRepositoryUrl", "true")
        ("EmbedUntrackedSources", "true")
        ("IncludeSymbols", "true")
        ("IncludeSymbols", "false")
        ("SymbolPackageFormat", "snupkg")
        ("Copyright", copyright)
    ]

    DotNet.pack (fun p ->
        { p with
            Configuration = DotNet.BuildConfiguration.Release
            OutputPath = Some "bin"
            MSBuildParams = { p.MSBuildParams with Properties = properties}
        }
    ) (projectName + ".sln"))

//--------------------------------------------------------------------------------------
//Generate the documentation

Target.create "GenerateDocs" (fun _ ->
   Fake.IO.Shell.cleanDir ".fsdocs"
   DotNet.exec id "fsdocs" "build --clean" |> ignore
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "All" ignore

"Clean" ==> "AssemblyInfo" ==> "Build"
"Build" ==> "CleanDocs" ==> "GenerateDocs" ==> "All"
"Build" ==> "NuGet" ==> "All"
"Build" ==> "RunTests" ==> "All"

Target.runOrDefault "All"
