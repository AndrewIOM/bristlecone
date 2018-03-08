module ModelLibrary

module Likelihood =

    open Types.ParameterEstimation

    let sumOfSquares paramaters exp obs =
        Array.zip obs exp
        |> Array.sumBy (fun d -> ((fst d) - (snd d)) ** 2.)

//     let private bivariateGaussian' (p:Map<string,ParameterPool>) obsx obsy expx expy = 
//         let diffx = obsx - expx
//         let diffy = obsy - expy
//         let zta1 = diffx ** 2. / p.["sigmax"].Value ** 2.
//         let zta2 = 2. * p.["rho"].Value * ((diffx * diffy) / (p.["sigmax"].Value * p.["sigmay"].Value))
//         let zta3 = diffy ** 2. / p.["sigmay"].Value ** 2.
//         let z = zta1 - zta2 + zta3
//         -log((1./(2.*System.Math.PI*p.["sigmax"].Value*p.["sigmay"].Value*sqrt(1.-(p.["rho"].Value*p.["rho"].Value)))) * exp (-z / (2. * (1. - (p.["rho"].Value * p.["rho"].Value)))))    

//     /// <summary> 
//     /// Log likelihood function for dual simultaneous system, assuming Gaussian error for both x and y.
//     /// </summary> 
//     let bivariateGaussian (p:Map<string,ParameterPool>) (obsx:float list) (obsy:float list) (expx: float list) (expy: float list) = 
//         [1 .. (List.length obsx) - 1] 
//         |> List.sumBy (fun x -> (bivariateGaussian' p obsx.[x] obsy.[x] expx.[x] expy.[x])) 

// /// <summary> 
// /// Models of absolute plant growth rate (AGR), in the form dx/dt
// /// </summary>
// module PlantGrowth =

//     open Types.ParameterEstimation

//     let private inverseLogitTransform q = exp (q) / (1. + exp q)

//     let boxCox' lambda y =
//         match lambda with
//         | 0. -> log y
//         | _ -> ((y ** lambda) - 1.) / lambda

//     // Plant growth models: 

//     let linear (p:ParameterPool) t m =
//         p.["r"].Value
    
//     let exponential (p:ParameterPool) t m =
//         p.["r"].Value * m

//     let powerLaw (p:ParameterPool) t m =
//         p.["r"].Value * m ** p.["power"].Value

//     let boxCox (p:ParameterPool) t m =
//         let y = p.["r"].Value * m
//         let delta = 0.01
//         match p.["lambda"].Value with
//         | 0. -> log y
//         | _ -> ((y ** p.["lambda"].Value) + delta) / p.["lambda"].Value

//     let monoMolecular (p:ParameterPool) t m =
//         p.["r"].Value * (p.["k"].Value - m)

//     let logistic (p:ParameterPool) t m =
//         p.["r"].Value * m * ( 1. - (m / p.["k"].Value) )

//     let logisticFourParam (p:ParameterPool) t m =
//         let r = p.["r"].Value
//         let k = exp p.["k"].Value //Positive
//         let lratio = inverseLogitTransform p.["l"].Value //Positive and 0-1
//         let l = lratio * k
//         r * (m - l) * ((k - m)/(k - l))

//     let generalisedLogistic (p:ParameterPool) t m =
//         let r = exp p.["r"].Value
//         let alpha = exp p.["alpha"].Value
//         let beta = exp p.["beta"].Value
//         let gamma = exp p.["gamma"].Value
//         let k = exp p.["k"].Value
//         r * m ** alpha * (1. - (m / k) ** beta) ** gamma

//     let gompertz (p:ParameterPool) t m =
//         let r = exp p.["r"].Value // Positive only
//         let k = exp p.["k"].Value // Positive only
//         //printfn "g = %f; t = %f" m t
//         r * m * (log (k / m))


//     // Coupled growth-resource models 2:

//     let private gompertzResourceLimit mt n u s =

//         let nRequiredForMaxMt = mt * s
//         let nRequiredForMinMt = 0.1 * s
//         let nl =
//             if nRequiredForMaxMt > u
//                 then u
//                 else nRequiredForMaxMt
//         let ns = nRequiredForMinMt
//         match n with
//         | n when n <= ns -> 0.
//         | n when ns <= n && n <= nl -> (n - ns) / (nl - ns)
//         | n when n >= nl -> 1.
//         | _ -> 1.


//     let logisticWithNoResourceLimitation (p:ParameterPool) t m d15N =
//         let r = exp p.["r"].Value
//         let k = exp p.["k"].Value
//         let maxGrowthRate = m * (1. - (m / k))
//         r *  maxGrowthRate

//     let resourceLimitationNone (p:ParameterPool) t m d15N =
//         let lambda = exp p.["lambda"].Value
//         lambda

//     let logisticWithResourceLimitation (p:ParameterPool) t m d15N =
//         let frac = p.["frac"].Value
//         let n = d15N - frac
//         let r = exp p.["r"].Value
//         let k = exp p.["k"].Value
//         let u = exp p.["u"].Value
//         let s = exp p.["s"].Value
//         let maxGrowthRate = m * (1. - (m / k))
//         let a = gompertzResourceLimit maxGrowthRate n u s
//         r * a * maxGrowthRate

//     let resourceLimitationGompertz (p:ParameterPool) t m d15N =
//         let lambda = exp p.["lambda"].Value
//         let frac = p.["frac"].Value
//         let n = d15N - frac
//         let r = exp p.["r"].Value
//         let k = exp p.["k"].Value
//         let u = exp p.["u"].Value
//         let s = exp p.["s"].Value
//         let maxGrowthRate = m * (1. - (m / k))
//         let a = gompertzResourceLimit maxGrowthRate n u s
//         let result = lambda - (s * m ** -3.) * (r * maxGrowthRate * a)    
//         if n - result < 0. then nan else result
        
        
//     // Using generalisedLogistic
//     let GeneralisedLogisticWithResourceLimitation (p:ParameterPool) t m d15N =
//         let frac = p.["frac"].Value
//         let n = d15N - frac
//         let r = exp p.["r"].Value
//         let k = exp p.["k"].Value
//         let u = exp p.["u"].Value
//         let s = exp p.["s"].Value
//         let alpha = exp p.["alpha"].Value
//         let beta = exp p.["beta"].Value
//         let gamma = exp p.["gamma"].Value
//         let maxGrowthRate = m ** alpha * (1. - (m / k) ** beta) ** gamma
//         let a = gompertzResourceLimit maxGrowthRate n u s
//         r * a * maxGrowthRate

//     let GeneralisedResourceLimitationGompertz (p:ParameterPool) t m d15N =
//         let lambda = exp p.["lambda"].Value
//         let frac = p.["frac"].Value
//         let alpha = exp p.["alpha"].Value
//         let beta = exp p.["beta"].Value
//         let gamma = exp p.["gamma"].Value
//         let n = d15N - frac
//         let r = exp p.["r"].Value
//         let k = exp p.["k"].Value
//         let u = exp p.["u"].Value
//         let s = exp p.["s"].Value
//         let maxGrowthRate = m ** alpha * (1. - (m / k) ** beta) ** gamma
//         let a = gompertzResourceLimit maxGrowthRate n u s
//         lambda - s * (r * maxGrowthRate * a)
