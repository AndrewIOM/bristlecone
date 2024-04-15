(**
---
title: Ensembles and Parallel Processing
category: Techniques
categoryindex: 2
index: 4
---
*)

(*** condition: prepare ***)
#nowarn "211"
#r "../src/Bristlecone/bin/Release/netstandard2.0/Bristlecone.dll"
(*** condition: fsx ***)
#if FSX
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // FSX
(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Bristlecone,{{fsdocs-package-version}}"
#endif // IPYNB

(**
Techniques: Ensembles and Parallel Processing
======================

Model-fitting and model-selection usually requires running
multiple model runs, for multiple hypotheses, and sometimes
for multiple datasets, resulting in hundreds of individual
model runs.

Bristlecone includes capabilities to orchestrate the parallel
running of such complex analyses within the 
`Bristlecone.Workflow` namespace. Using Bristlecone's 
`OrchestrationAgent`, you can queue jobs to run when processor
cores are available.

### Work Packages

To run your analyses in parallel, you must wrap up each analysis
as a *work package*. A *work package* is simply an async computation
that returns an estimation result, with the function signature 
`Async<ModelSystem.EstimationResult>`. A simple example function to
setup work packages for a number of datasets, hypotheses, and individuals
is given below:
*)

open Bristlecone

let replicates = 3 // number of replications per analysis
let endCondition = Optimisation.EndConditions.afterIteration 100000

let workPackages datasets hypotheses engine =
    seq {
        for d in datasets do
            for h in [ 1 .. hypotheses |> List.length ] do
                for _ in [ 1 .. replicates ] do
                    yield async {
                        return Bristlecone.fit 
                                engine 
                                endCondition 
                                d
                                hypotheses.[h-1]
                    }
    }

(**
You can perform any additional steps within each work package. A common
approach is to save each model result to disk (i.e., using functions from 
the `Bristlecone.Data` namespace) within the async
block in the above code, so that results are available as soon as
they are complete.

When the `EstimationEngine`, datasets, and hypotheses are
applied to this function, the resultant `seq<Async<EstimationResult>>` 
can be passed to an orchestration agent.

### Orchestration Agent

An orchestration agent manages the running of work packages depending on
your local computer's resources. You can access these features through the following
namespace:
*)

open Bristlecone.Workflow

(**
There are three arguments required to create an `OrchestrationAgent`:

* A **logger** (`LogEvent -> unit`), which consumes log messages from all threads. You
  must ensure that the logger is safe to use from multiple processes. Bristlecone
  includes some loggers that are thread-safe. In addition, the `Bristlecone.Charts.R`
  NuGet package contains interoperability with R to produce real-time traces of the movement
  of each analysis through parameter space; this is very useful for example
  with MCMC optimisation techniques to compare chains.

* The **number of processes to run in parallel**. The recommended approach is to set
  this to `System.Environment.ProcessorCount`, which represents the number of cores
  available on your system.

* Whether to **cache results in the resultant object**. This increases memory usage, but allows
  post-hoc analysis of the results. A common approach is to set this to `false` but save each
  result to disk within the work package itself (see above). The results
  can then be re-loaded at a later date for diagnostics and further analysis.

First, let's use one of Bristlecone's built-in loggers to print the progress of each
work package:
*)

let logger = Logging.Console.logger 1000

(**
This logger will print the current point in parameter space each thousand
iteration, for each chain (along with process IDs). Next, let's create and setup
the orchestration agent:
*)

let orchestrator = Orchestration.OrchestrationAgent(logger, System.Environment.ProcessorCount, false)

// fun datasets hypotheses engine ->

//     // Orchestrate the analyses
//     let work = workPackages datasets hypotheses engine
//     let run() = 
//         work 
//         |> Seq.iter (
//             Orchestration.OrchestrationMessage.StartWorkPackage 
//             >> orchestrator.Post)

//     run()

(**
If the above code is supplied with datasets, hypotheses, and an
`EstimationEngine`, it will schedule, queue and run the jobs until complete.
*)