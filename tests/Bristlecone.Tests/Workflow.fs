module WorkflowTests

open Bristlecone.Workflow
open Expecto

module Orchestration =

    let writeOut _ = ()

    [<Tests>]
    let agentTests =
        testList
            "Workflow package agent"
            [

            //   testProperty "Only runs n maximum packages at one"
            //   <| fun n ->

            //       // let agent = Orchestration.orchestrationAgent writeOut n



            //       failwith "Not implemented" ]
            ]
