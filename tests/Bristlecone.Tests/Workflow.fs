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
            ]
