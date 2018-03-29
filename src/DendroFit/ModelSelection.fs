module ModelSelection

module Akaike =

    ///**Description**
    /// The Akaike information criterion, a standardised index of model fit quality for models that have different numbers of parameters.
    ///**Parameters**
    ///  * `k` - The number of parameters within the model in question.
    ///  * `logLikelihood` - a `float` representing the minimum log-likelihood achieved for the model in question.
    let aic (k:int) logLikelihood =
        2. * (float k) - 2. * logLikelihood

    
    ///**Description**
    /// The Akaike information criterion, corrected for small sample sizes. 
    /// It represents standardised index of model fit quality for models that have different numbers of parameters.
    ///**Assumptions**
    /// Your model must adhere to the following assumptions:
    /// - Univariate
    /// - Linear in parameters
    /// - Normally-distributed residuals
    ///**Parameters**
    ///  * `n` - The sample size 
    ///  * `k` - The number of parameters within the model in question.
    ///  * `logLikelihood` - a `float` representing the minimum log-likelihood achieved for the model in question.
    let aicc n k logLikelihood =
        let aic = aic k logLikelihood
        let correction = (2. * ((float k) ** 2.) + 2. * (float k)) / ((float n) - (float k) - 1.)
        aic + correction