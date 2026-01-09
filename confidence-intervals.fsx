(**

*)
#r "nuget: Bristlecone,3.0.0-beta1"
(**
# Calculating Confidence Intervals

Bristlecone includes functions to calculate confidence intervals based
on a profile likelihood method. Given the minimum -log likelihood, the
profile likelihood method samples the parameter space around this point.
The range sampled for each parameter is discovered at 95% and 68%
confidence based on a chi squared distribution.

These functions are included in the `Bristlecone.Optimisation.ConfidenceInterval` module.
An example of their use is given below:

*)
open Bristlecone
open Bristlecone.Optimisation
open Bristlecone.Data

fun engine dataset hypothesis result ->

    // The function used to fit the model, which unless an
    // advanced scenario is usually Bristlecone.fit
    let fitFn = Bristlecone.fit

    // The number of jumps to perform in parameter space
    let n = 10000<iteration>

    let ci =
        Confidence.ProfileLikelihood.profile fitFn engine dataset hypothesis n result

    // Optionally, save the result
    let saveDir = "/some/save/dir"
    let subjectName = "some subject"
    let modelId = "some model hypothesis"
    Confidence.save saveDir subjectName modelId result.ResultId ci

