namespace Bristlecone

module PlantIndividual =

    open EnvironmentalVariables
    open Time

    type PlantGrowth =
    | RingWidth of GrowthSeries<mm>
    | BasalArea of GrowthSeries<mm^2>
    | StemVolume of GrowthSeries<mm^3>

    type Trait =
    | Static of float
    | Variable of TimeSeries<float>

    type PlantIndividual = {
        Identifier: ShortCode
        Growth: PlantGrowth
        InternalControls: Map<ShortCode,Trait>
        Environment: LocalEnvironment
    }

    let removeUnit (x:float<_>) =
        float x

    let zipEnv envName envData (plant:PlantIndividual) =
        { plant with Environment = plant.Environment.Add (envName, envData) }

    let growthSeries plant =
        match plant with
        | RingWidth rw ->
            match rw with
            | Absolute rws -> TimeSeries.map rws (fun (x,t) -> removeUnit x, t) |> Absolute
            | Cumulative m -> TimeSeries.map m (fun (x,t) -> removeUnit x, t) |> Cumulative
            | Relative rws -> TimeSeries.map rws (fun (x,t) -> removeUnit x, t) |> Relative
        | _ -> invalidOp "Not implemented"

    let private toCumulativeGrowth' (growth:GrowthSeries<_>) =
        match growth with
        | Absolute g ->
            let time = g |> toSeq |> Seq.map snd |> Seq.scan (+) (g |> TimeSeries.start)
            let agr = g |> toSeq |> Seq.map fst
            let biomass = agr |> Seq.scan (+) 0.<_> |> Seq.tail |> Seq.toList
            TimeSeries.createVarying (Seq.zip time biomass) |> Cumulative
        | Relative _ -> failwith "Not implemented"
        | Cumulative _ -> growth

    let private toRelativeGrowth' (growth:GrowthSeries<_>) =
        match growth with
        | Absolute g -> 
            let time = g |> toSeq |> Seq.map snd |> Seq.scan (+) (g |> TimeSeries.start)
            let agr = g |> toSeq |> Seq.map fst
            let biomass = agr |> Seq.scan (+) 0.<_> |> Seq.tail |> Seq.toList
            let rgr = agr |> Seq.mapi (fun i a -> a / biomass.[i] * 1.<_> )
            TimeSeries.createVarying (Seq.zip time rgr) |> Relative
        | Relative _ -> growth
        | Cumulative g -> 
            // Calculate agr and divide agr by biomass
            failwith "Not implemented"

    let toRelativeGrowth (plant:PlantIndividual) =
        match plant.Growth with
        | RingWidth s -> { plant with Growth = s |> toRelativeGrowth' |> RingWidth }
        | _ -> invalidOp "Not implemented"

    let toCumulativeGrowth (plant:PlantIndividual) =
        match plant.Growth with
        | RingWidth s -> { plant with Growth = s |> toCumulativeGrowth' |> RingWidth }
        | _ -> invalidOp "Not implemented"

    let keepCommonYears (plant:PlantIndividual) =
        let allSeries = 
            let response = plant.Growth |> growthSeries |> growthToTime
            let envSeries = plant.Environment |> Map.toList |> List.map snd
            response :: envSeries
        let startDates, endDates =
            allSeries
            |> List.map (fun s -> TimeSeries.start s, TimeSeries.endDate s)
            |> List.unzip
        let startDate = startDates |> List.max
        let endDate = endDates |> List.min
        let mapts f s = TimeSeries.map s f
        printfn "Start = %A End = %A" startDate endDate
        { plant with Environment = plant.Environment |> Map.toList |> List.map (fun (x,y) -> x,y |> TimeSeries.bound startDate endDate) |> Map.ofList
                     Growth = plant.Growth |> growthSeries |> growthToTime |> TimeSeries.bound startDate endDate |> mapts (fun (x,t) -> x * 1.<mm>, t) |> Absolute |> RingWidth }
