module Optimisation

type Point = float []
type Solution = float * Point
type Objective = Point -> float
type Domain = (float*float) []

/// MCMC random walk algorithm.
/// For more information, see https://doi.org/10.3389/fams.2017.00006.
module MCMC =
    
    let initialise (d:Domain) (rng:System.Random) =
        [| for (min,max) in d -> min + (max-min) * rng.NextDouble() |]

    /// Current implementation draws from an independent univariate normal distribution for each individual parameter. 
    let draw random mean stdev =
        let distribution = MathNet.Numerics.Distributions.Normal(mean,stdev,random)
        fun () -> distribution.Sample()

    /// NB Requires log-likelihood generating objective
    let rec metropolis' propose f theta1 l1 remaining d vl =

        let theta2 = propose theta1
        let l2 = f theta2

        // printfn "Proposal %f" l2

        let thetaAccepted, lAccepted = 
            if l2 < l1  && l2 <> -infinity
                then 
                    // printfn "Accepting %f" l2
                    theta2, l2
                else
                    // TODO Move randomiser somewhere else
                    let rand = (MathNet.Numerics.Distributions.ContinuousUniform(0.,1.)).Sample()
                    let ratio = - (l2 - l1) |> exp

                    // printfn "Original was %f. New is %f Ratio is %f" l1 l2 ratio

                    if rand < ratio && l2 <> infinity && l2 <> -infinity && l2 <> nan
                        then 
                            // printfn "Accepting l2. Original was %f. Ratio is %f" l1 ratio
                            theta2, l2
                        else 
                            // printfn "Rejecting %f (current is %f" l2 l1
                            theta1, l1
        
        match remaining with 
        | r when r <= 0 -> vl, d
        | _ -> 
            if remaining % 500 = 0 then printfn "MCMC Optimisation: %i remaining iterations" remaining
            metropolis' propose f thetaAccepted lAccepted (remaining - 1) (thetaAccepted::d) (lAccepted::vl)

    let randomWalk burn n domain (f:Point->float) =

        // 1. Randomly determine a candidate set of parameters theta
        let random = MathNet.Numerics.Random.MersenneTwister(true)
        let theta = initialise domain random

        // 2. Compute L1 = L(01)
        let l1 = f theta

        if l1 = nan then invalidOp "Initial likelihood could not be calculated"

        // Assume bounds for parameters represent -2 and 2 sigma (95%) confidence bounds
        // Set a variance that spreads these to six-sigma
        // Use an independent normal distribution for each parameter
        // 3. Define jumping method / distributions / priors in here
        let proposeJump theta = 
            domain 
            |> Array.map (fun (low,high) -> draw random 0. ((high - low) / 2.) () )
            |> Array.zip theta
            |> Array.map (fun (thetai,zi) -> thetai + zi)

        // 3. Burn
        let burnedL,burnedP = metropolis' proposeJump f theta l1 burn [] []

        // 4. Iterate
        metropolis' proposeJump f burnedP.Head burnedL.Head n [] []


// Nelder Mead implementation
// Modified from https://github.com/mathias-brandewinder/Amoeba
module Amoeba =

    type Point = float []
    type Solution = float * Point
    type Objective = Point -> float
    type Domain = (float*float) []

    module Solver = 

        type Amoeba = 
            { Dim:int; Solutions:Solution [] } // assumed to be sorted by fst value
            member this.Size = this.Solutions.Length
            member this.Best = this.Solutions.[0]
            member this.Worst = this.Solutions.[this.Size - 1]

        type Settings = { Alpha:float; Sigma:float; Gamma:float; Rho:float; Size:int }

        let Default = { Alpha=1.0; Sigma=0.5; Gamma=2.0; Rho=(-0.5); Size=3 }

        let print (a:Amoeba) = 
            printfn "Amoeba state"
            a.Solutions 
            |> Seq.iter (fun (v,x) -> 
                printfn "  %.2f, %s" v (x |> Seq.map string |> String.concat ","))

        let evaluate (f:Objective) (x:Point) = f x, x
        let valueOf (s:Solution) = fst s

        let replace (a:Amoeba) (s:Solution) = 
            let last = a.Size - 1
            let a' = Array.copy a.Solutions
            a'.[last] <- s
            { a with Solutions = a' |> Array.sortBy fst }

        let centroid (a:Amoeba) = 
            [| for d in 0 .. (a.Dim - 1) -> 
                (a.Solutions.[0..a.Size - 2] |> Seq.averageBy(fun (_,x) -> x.[d])) |]

        let stretch ((X,Y):Point*Point) (s:float) =
            Array.map2 (fun x y -> x + s * (x - y)) X Y

        let reflected v s = stretch v s.Alpha

        let expanded v s = stretch v s.Gamma

        let contracted v s = stretch v s.Rho

        let shrink (a:Amoeba) (f:Objective) s =
            let best = snd a.Best
            { a with Solutions =         
                        a.Solutions 
                        |> Array.map (fun p -> stretch (best,snd p) -s.Sigma)
                        |> Array.map (evaluate f) } 

        let update (a:Amoeba) (f:Objective) (s:Settings) =
            let cen = centroid a
            let rv,r = reflected (cen, (snd a.Worst)) s |> evaluate f
            if ((valueOf (a.Best) <= rv) && (rv < (valueOf (a.Solutions.[a.Size - 2])))) then
                replace a (rv,r)
            else
                if (rv < valueOf (a.Best)) then
                    let ev,e = expanded (cen, r) s |> evaluate f
                    if (ev < rv) then
                        replace a (ev,e)
                    else
                        replace a (rv,r)
                else
                    let (cv,c) = contracted (cen, (snd a.Worst)) s |> evaluate f
                    if (cv < valueOf (a.Worst)) then
                        replace a (cv,c)
                    else
                        shrink a f s

        let initialize (d:Domain) (rng:System.Random) =
            [| for (min,max) in d -> min + (max-min) * rng.NextDouble() |]

        let solve settings domain f iter =
            let dim = Array.length domain
            let rng = System.Random()
            let start =             
                [| for _ in 1 .. settings.Size -> initialize domain rng |]
                |> Array.map (evaluate f)
                |> Array.sortBy fst
            let amoeba = { Dim = dim; Solutions = start }

            let rec search i a =
                if i > 0 then search (i-1) (update a f settings)
                else 
                    printfn "Solution: -L = %f" (fst a.Solutions.[0])
                    a.Solutions.[0]

            search iter amoeba
