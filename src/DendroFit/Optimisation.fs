module Optimisation

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
