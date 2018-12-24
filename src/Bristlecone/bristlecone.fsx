#nowarn "211"

// Standard NuGet or Paket location
#I "."
#I "lib/netstandard2.0"

// Standard Paket locations
#I "../FSharp.Data/lib/net45"
#I "../MathNet.Numerics/lib/netstandard2.0"
#I "../MathNet.Numerics.FSharp/lib/netstandard2.0"

// Try various folders that people might like
#I "bin"
#I "../bin"
#I "../../bin"
#I "lib"

// Reference FSharp.Data, MathNet, and Bristlecone 
#r "FSharp.Data.dll"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"
//#r "Microsoft.Research.Oslo.dll"
#r "Bristlecone.dll"
