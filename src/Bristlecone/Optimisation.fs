namespace Bristlecone.Optimisation

open Bristlecone.Logging

type Point = float []
type Solution = float * Point
type Objective = Point -> float
type Domain = (float*float*Bristlecone.Parameter.Constraint) []

module Distribution =

    open MathNet.Numerics.LinearAlgebra

    let mapply (m:Matrix<float>) f = 
        m.EnumerateIndexed() |> Seq.iter(fun (i,j,v) -> m.[i,j] <- f v ); m

    module MutlivariateNormal =

        // See: https://stackoverflow.com/questions/11277146/multivariate-normal-distribution-with-math-net
        let sample' (rnd:System.Random) = 
            fun () ->
                let rec randomNormal () = 
                    let u1, u2 = rnd.NextDouble(),rnd.NextDouble()
                    let r = sqrt (-2. * (log u1))
                    let theta = 2. * System.Math.PI * u2  
                    seq { 
                        yield r * sin theta
                        yield r * cos theta 
                        yield! randomNormal() }
                randomNormal ()

        let sample (cov:Matrix<float>) rnd = 
            let R = 
                if cov.Determinant() = 0. 
                then
                    let t = cov.Svd true
                    let u,d = t.U, t.W
                    let A = (mapply d sqrt) * u.Transpose()
                    let qr = A.QR() in qr.R.Transpose()
                else cov.Cholesky().Factor
            fun () -> 
                let v = vector ( sample' rnd () |> Seq.take cov.ColumnCount |> List.ofSeq )
                R * v



/// MCMC random walk algorithm.
/// For more information, see https://doi.org/10.3389/fams.2017.00006.
/// See also Haario, Saksman and Tamminen 2001: https://projecteuclid.org/download/pdf_1/euclid.bj/1080222083 
module MonteCarlo =

    // NB Lets set this up so that tuning only occurs during the burnin phase, then stops.
    // TODO Move all MathNet distribution work into a seperate module

    open MathNet.Numerics.LinearAlgebra

    /// Current implementation draws from an independent univariate normal distribution for each individual parameter. 
    let draw random mean stdev =
        let distribution = MathNet.Numerics.Distributions.Normal(mean,stdev,random)
        fun () -> distribution.Sample()

    let initialise (d:Domain) (rng:System.Random) =
        [| for (min,max,_) in d -> (draw rng (min + (max-min) / 2.) ((max - min) / 6.))() |]

    let constrainJump a b (scaleFactor:float) c =
        match c with
        | Bristlecone.Parameter.Constraint.Unconstrained -> a + (b * scaleFactor)
        | Bristlecone.Parameter.Constraint.PositiveOnly ->
            if (a + (b * scaleFactor)) < 0.
                then (a - (b * scaleFactor)) 
                else (a + (b * scaleFactor))

    /// f -> a log-likelihood function
    let rec metropolisHastings' writeOut propose tune f theta1 l1 remaining d scale =

        let sc = scale |> tune remaining d
        let theta2 = theta1 |> propose sc
        let thetaAccepted, lAccepted = 
            let l2 = f theta2
            if l2 < l1
            then theta2, l2
            else
                let rand = MathNet.Numerics.Distributions.ContinuousUniform(0.,1.).Sample()
                let ratio = - (l2 - l1) |> exp
                if rand < ratio && l2 <> infinity && l2 <> -infinity && l2 <> nan
                    then theta2, l2
                    else theta1, l1

        match remaining with 
        | r when r <= 0 -> d, sc
        | _ -> 
            if remaining % 1000 = 0 && d |> Seq.length >= 250 then 
                let ar = float ((d |> Seq.take 250 |> Seq.pairwise |> Seq.where(fun (x,y) -> x <> y) |> Seq.length)) / (float 250)
                writeOut <| GeneralEvent (sprintf "[Optimisation] %i remaining iterations (value %f, AR = %f)" remaining lAccepted ar)
            metropolisHastings' writeOut propose tune f thetaAccepted lAccepted (remaining - 1) ((lAccepted,thetaAccepted)::d) sc

    module TuningMode =

        /// Find an appropriate scale factor for a standard deviation and its acceptance rate.
        let tuneScale scale accRate =
            match accRate with
            | a when a > 0.95   -> scale * 10.0
            | a when a > 0.75   -> scale * 2.0
            | a when a > 0.50   -> scale * 1.1
            | a when a < 0.001  -> scale * 0.1
            | a when a < 0.05   -> scale * 0.5
            | a when a < 0.20   -> scale * 0.9
            | _ -> scale

        /// Modifies a scale factor depending on 
        let scaleFactor tuneInterval remainingIterations history scale =
            if remainingIterations % tuneInterval = 0 then
                if history |> Seq.length >= tuneInterval then
                    let acceptance = float ((history |> Seq.take tuneInterval |> Seq.pairwise |> Seq.where(fun (x,y) -> x <> y) |> Seq.length)) / (float tuneInterval)
                    tuneScale scale acceptance
                else scale
            else scale

        /// Tunes a covariance matrix based on recent samples.
        let tuneCovariance weight (recentCovariance:Matrix<float>) (oldCovariance:Matrix<float>) =
            weight * recentCovariance + (1. - weight) * oldCovariance

        /// Generate a covariance matrix
        let covarianceMatrix parameterCount (scale:float) =
            let m4 = MathNet.Numerics.LinearAlgebra.DiagonalMatrix.identity<float> parameterCount
            (m4 * scale) / sqrt (float parameterCount)

        /// Calculates the covariance of a given matrix
        let computeCovariance (matrix:Matrix<float>) =
            let columnAverages = matrix.ColumnSums() / (float matrix.RowCount)
            let centredColumns = matrix.EnumerateColumns() |> Seq.zip columnAverages |> Seq.map (fun (col,avg) -> col - avg) |> Seq.toList
            let centred = MathNet.Numerics.LinearAlgebra.DenseMatrix.ofColumns centredColumns
            let normalisationFactor = float <| if matrix.RowCount = 1 then 1 else matrix.RowCount - 1
            centred.TransposeThisAndMultiply(centred) / normalisationFactor

        /// Parameters to matrix
        let samplesToMatrix (samples:float[][]) =
            samples |> matrix

        /// The starting covariance matrix for parameters in a multivariate distribution
        let defaultCovariance n = covarianceMatrix n 2.38

        let covarianceFromBounds n (domain:Domain) random =
            [| 1 .. n |]
            |> Array.map(fun _ ->
                domain
                |> Array.map (fun (low,high,_) -> draw random 0. ((high - low) / 4.) () ) )
                |> samplesToMatrix
                |> computeCovariance


        /// Tune previously observed covariance based on most recent period
        let covariance tuneInterval weighting remaining (history:(float*float[]) seq) scale =
            if remaining % tuneInterval = 0 then
                if history |> Seq.length >= tuneInterval then
                    history
                    |> Seq.map snd
                    |> Seq.take tuneInterval
                    |> matrix
                    |> computeCovariance
                    |> fun c -> tuneCovariance weighting scale c
                else scale
            else scale

        let dual tuneInterval weighting remaining history tuningFactors =
            let cov,sc = tuningFactors
            let tunedSc = scaleFactor tuneInterval remaining history sc
            let tunedCov = covariance tuneInterval weighting remaining history cov
            tunedCov, tunedSc

        let scaleOnly tuneInterval remaining history tuningFactors =
            let cov,sc = tuningFactors
            let tunedSc = scaleFactor tuneInterval remaining history sc
            cov, tunedSc

        let covarianceOnly tuneInterval weighting remaining history tuningFactors =
            let cov,sc = tuningFactors
            let tunedCov = covariance tuneInterval weighting remaining history cov
            tunedCov, sc

        let none _ _ factors = factors

    type TuneMethod =
        | Covariance of float
        | Scale
        | CovarianceWithScale of float

    let toFn method interval =
        match method with
        | Covariance w -> TuningMode.covarianceOnly interval w
        | Scale -> TuningMode.scaleOnly interval
        | CovarianceWithScale w -> TuningMode.dual interval w

    type Frequency = int
    type TuneStep = TuneMethod * Frequency * int

    let randomWalk (tuningSteps:TuneStep seq) writeOut n domain (f:Point->float) : (float * float[]) list =
        writeOut <| GeneralEvent (sprintf "[Optimisation] Starting MCMC Random Walk")
        let random = MathNet.Numerics.Random.MersenneTwister(true)
        let theta = initialise domain random
        writeOut <| GeneralEvent (sprintf "[Optimisation] Initial theta is %A" theta)
        let initialCovariance = TuningMode.covarianceFromBounds 10000 domain random
        let sample cov = Distribution.MutlivariateNormal.sample cov random
        let proposeJump (cov:Matrix<float>,scale) (theta:float[]) =
            sample ((2.38 ** 2.) * cov / (float theta.Length)) <| ()
            |> Vector.toArray
            |> Array.map (fun i -> i * scale)
            |> Array.mapi (fun i x -> x, domain.[i] )
            |> Array.zip theta
            |> Array.map (fun (thetai,(zi,(_,_,con))) -> constrainJump thetai zi scale con)

        tuningSteps
        |> Seq.fold(fun (s,sc) (method,frequency,endCondition) -> 
            let l,t = s |> Seq.head
            metropolisHastings' writeOut proposeJump (toFn method frequency) f t l endCondition s sc ) ([f theta, theta], (initialCovariance,1.))
        ||> fun r s -> 
            let l,t = r |> Seq.head
            metropolisHastings' writeOut proposeJump (TuningMode.none) f t l n r s
        |> fst


// Nelder Mead implementation
// Modified from https://github.com/mathias-brandewinder/Amoeba
module Amoeba =

    type Point = float []
    type Solution = float * Point
    type Objective = Point -> float

    module Solver = 

        type Amoeba = 
            { Dim:int; Solutions:Solution [] } // assumed to be sorted by fst value
            member this.Size = this.Solutions.Length
            member this.Best = this.Solutions.[0]
            member this.Worst = this.Solutions.[this.Size - 1]

        type Settings = { Alpha:float; Sigma:float; Gamma:float; Rho:float; Size:int }

        let Default = { Alpha=1.0; Sigma=0.5; Gamma=2.0; Rho=(-0.5); Size=3 }

        let print logger (a:Amoeba) = 
            logger <| GeneralEvent "Amoeba state"
            a.Solutions 
            |> Seq.iter (fun (v,x) -> 
                logger <| GeneralEvent (sprintf "  %.2f, %s" v (x |> Seq.map string |> String.concat ",")))

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
            [| for (min,max,_) in d -> min + (max-min) * rng.NextDouble() |]

        let solve settings logger iter domain f =
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
                    logger <| GeneralEvent (sprintf "Solution: -L = %f" (fst a.Solutions.[0]))
                    a.Solutions.[0]

            search iter amoeba

        let solveTrace settings log iter domain f =
            [ solve settings log iter domain f ]


module RootFinding =

    /// Secant method for finding root of non-linear equations. This method is faster than bisection, but may not converge on a root.
    let rec secant n N f x0 x1 x2 : float =
        if n >= N then x0
        else
            let x = x1 - (f(x1))*((x1 - x0)/(f(x1) - f(x0)))
            secant (n + 1) N f x x0 x2

    /// Bisect method for finding root of non-linear equations. A "strong and stable" algorithm.
    let rec bisect n N f a b t : float =
        if n >= N then nan
        else
            let c = (a + b) / 2.
            if (f c) = 0.0 || (b - a) / 2. < t 
                then c
                else 
                    if sign(f c) = sign (f a)
                    then bisect (n + 1) N f c b t
                    else bisect (n + 1) N f a c t
