module DendroTests

open System
open Expecto
open Bristlecone
open Bristlecone.Dendro
open FsCheck

let config = Config.config

[<Tests>]
let sunrise =
    testList
        "Sunrise calculations"
        [ testCase "Complete dark during polar winter"
          <| fun _ ->

              let result =
                  Sunrise.calculate 2020 01 01 69.682778<latitude> 18.942778<longitude> "W. Europe Standard Time"

              Expect.equal result Sunrise.CompleteDark "Was not complete dark in Tromso in January"

          ]
