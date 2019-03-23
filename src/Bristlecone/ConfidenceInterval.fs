namespace Bristlecone.Optimisation.ConfidenceInterval

open Bristlecone.Logging

// Profile Likelihood Method
// _______________________________

// Given the MLE (i.e. from Simulated Annealing), 
// run an MCMC algorithm that samples around the MLE
// Collect any results where the difference in likelihoods is less than 2

type Interval = {
    Lower: float
    Upper: float
}

and ConfidenceInterval = {
    ``95%``: Interval
    ``68%``: Interval
    Estimate: float
}

module Bounds =

    /// The difference in likelihood at 68% confidence
    let lowerBound = 0.49447324 // qchisq(0.68,1)/2
    /// The difference in likelihood at 95% confidence
    let upperBound = 1.92072941 // qchisq(0.95,1)/2

type OptimOutput() =
    let mutable (events:Bristlecone.Logging.ModelFitState list) = []
    with
        member __.SaveEvent(e) =
            match e with
            | Bristlecone.Logging.LogEvent.OptimisationEvent o -> events <- (events |> List.append [o])
            | _ -> ()

        member __.GetAll() = events

