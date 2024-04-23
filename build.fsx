// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#if FAKE
#r "paket: groupref Build //"
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
#r "nuget: MSBuild.StructuredLogger, 2.1.820"

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

let projectDescription =
    """
  An F# library for model-fitting model-selection (MFMS) of ecological 
  models to observational data. The library was developed for 
  individual-based plant modelling using tree ring time-series, 
  but can be used for any ecological models."""

let authors = "Andrew Martin"
let companyName = ""
let tags = "modelling time-series F# fsharp R data-science ecology model-fitting"
let license = "MIT"
let iconUrl = ""
let copyright = "(C) 2018-24 Andrew Martin"

let packageProjectUrl = "https://acm.im/bristlecone"
let repositoryType = "git"
let repositoryUrl = "https://github.com/AndrewIOM/bristlecone"
let repositoryContentUrl = "https://raw.githubusercontent.com/AndrewIOM/bristlecone"

// --------------------------------------------------------------------------------------
// The rest of the code is standard F# build script
// --------------------------------------------------------------------------------------

// Read release notes & version info from RELEASE_NOTES.md
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let binDir = __SOURCE_DIRECTORY__ @@ "bin"

let release =
    System.IO.File.ReadLines "RELEASE_NOTES.MD" |> Fake.Core.ReleaseNotes.parse

// Generate assembly info files with the right version & up-to-date information
Target.create "AssemblyInfo" (fun _ ->
    let fileName = "src/Bristlecone/AssemblyInfo.fs"

    AssemblyInfoFile.createFSharpWithConfig
        fileName
        [ Fake.DotNet.AssemblyInfo.Title projectName
          Fake.DotNet.AssemblyInfo.Company companyName
          Fake.DotNet.AssemblyInfo.Product projectName
          Fake.DotNet.AssemblyInfo.Description projectSummary
          Fake.DotNet.AssemblyInfo.Version release.AssemblyVersion
          Fake.DotNet.AssemblyInfo.FileVersion release.AssemblyVersion ]
        (AssemblyInfoFileConfig(false)))

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

Target.create "Clean" (fun _ ->
    Fake.IO.Shell.cleanDirs [ "bin"; "output" ]
    Fake.IO.Shell.cleanDirs [ "tests/Bristlecone.Tests/bin"; "tests/Bristlecone.Tests/obj" ])

Target.create "CleanDocs" (fun _ -> Fake.IO.Shell.cleanDirs [ ".fsdocs" ])

// --------------------------------------------------------------------------------------
// Check formatting with Fantomas

Target.create "CheckFormat" (fun _ ->
    let result = DotNet.exec id "fantomas" "./src --check"
    let resultDocs = DotNet.exec id "fantomas" "./docs --check"

    if result.ExitCode = 0 && resultDocs.ExitCode = 0 then
        Trace.log "No files need formatting"
    elif result.ExitCode = 99 then
        failwith "Some files need formatting, run \"dotnet fantomas  ./src\" to resolve this."
    elif result.ExitCode = 99 then
        failwith "Some files need formatting, run \"dotnet fantomas  ./docs\" to resolve this."
    else
        Trace.logf "Errors while formatting: %A" result.Errors)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Build" (fun _ ->
    Trace.log " --- Building the app --- "
    Fake.DotNet.DotNet.build id ("bristlecone.sln"))

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner & kill test runner when complete

Target.create "RunTests" (fun _ ->
    let rHome = Environment.environVarOrFail "R_HOME"
    Trace.logf "R_HOME is set as %s" rHome

    let result =
        Fake.DotNet.DotNet.exec
            (fun args ->
                { args with
                    Verbosity = Some Fake.DotNet.DotNet.Verbosity.Normal })
            "run"
            ("--project tests/Bristlecone.Tests")

    if result.ExitCode <> 0 then
        failwith "Tests failed")

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "NuGet" (fun _ ->
    // Format the description to fit on a single line (remove \r\n and double-spaces)
    let projectDescription =
        projectDescription.Replace("\r", "").Replace("\n", "").Replace("  ", " ")

    // Format the release notes
    let releaseNotes = release.Notes |> String.concat "\n"

    let properties =
        [ ("Version", release.NugetVersion)
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
          ("Copyright", copyright) ]

    DotNet.pack
        (fun p ->
            { p with
                Configuration = DotNet.BuildConfiguration.Release
                OutputPath = Some "bin"
                MSBuildParams =
                    { p.MSBuildParams with
                        Properties = properties } })
        ("bristlecone.sln"))

//--------------------------------------------------------------------------------------
//Generate the documentation

Target.create "DocsMeta" (fun _ ->
    [ "<Project xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"
      "<PropertyGroup>"
      sprintf "<Copyright>%s</Copyright>" copyright
      sprintf "<Authors>%s</Authors>" authors
      sprintf "<PackageProjectUrl>%s</PackageProjectUrl>" packageProjectUrl
      sprintf "<RepositoryUrl>%s</RepositoryUrl>" repositoryUrl
      sprintf "<PackageLicense>%s</PackageLicense>" license
      sprintf "<PackageReleaseNotes>%s</PackageReleaseNotes>" (List.head release.Notes)
      sprintf "<PackageIconUrl>%s/master/docs/content/logo.png</PackageIconUrl>" repositoryContentUrl
      sprintf "<PackageTags>%s</PackageTags>" tags
      sprintf "<Version>%s</Version>" release.NugetVersion
      sprintf "<FsDocsLogoSource>/img/logo.png</FsDocsLogoSource>"
      sprintf "<FsDocsLicenseLink>%s/blob/master/LICENSE</FsDocsLicenseLink>" repositoryUrl
      sprintf "<FsDocsReleaseNotesLink>%s/blob/master/RELEASE_NOTES.MD</FsDocsReleaseNotesLink>" repositoryUrl
      "<FsDocsWarnOnMissingDocs>true</FsDocsWarnOnMissingDocs>"
      "<FsDocsTheme>default</FsDocsTheme>"
      "</PropertyGroup>"
      "</Project>" ]
    |> Fake.IO.File.write false "Directory.Build.props")

Target.create "GenerateDocs" (fun _ ->
    Fake.IO.Shell.cleanDir ".fsdocs"
    DotNet.exec id "fsdocs" "build --clean --eval --strict" |> ignore)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "All" ignore

"Clean" ==> "CheckFormat" ==> "AssemblyInfo" ==> "Build"
"Build" ==> "CleanDocs" ==> "DocsMeta" ==> "GenerateDocs" ==> "All"
"Build" ==> "NuGet" ==> "All"
"Build" ==> "RunTests" ==> "All"

Target.runOrDefault "All"
