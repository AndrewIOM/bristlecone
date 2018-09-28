#load "../src/bristlecone.fsx"
#load "../packages/RProvider/RProvider.fsx"
open RProvider
open RProvider.graphics
open RProvider.ggplot2
open Bristlecone

module Plotting =

    let (++) (plot1:RDotNet.SymbolicExpression) (plot2:RDotNet.SymbolicExpression) = 
        R.``+``(plot1, plot2)

    let plot' df =
        ["data", box df; ]
        |> namedParams 
        |> R.ggplot

    let plot (variables:CodedMap<float[]>) =
        variables
        |> Map.toList
        |> List.map (fun (c,x) -> c.Value, x)
        |> namedParams
        |> R.data_frame
        |> plot'
