namespace Bristlecone

module PlantIndividual =

    open EnvironmentalVariables
    open Time

    // Year
    [<Measure>] type year

    // Millimetre
    [<Measure>] type mm

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
            | Absolute rws -> rws |> TimeSeries.map (fun (x,t) -> removeUnit x) |> Absolute
            | Cumulative m -> m |> TimeSeries.map (fun (x,t) -> removeUnit x) |> Cumulative
            | Relative rws -> rws |> TimeSeries.map (fun (x,t) -> removeUnit x) |> Relative
        | _ -> invalidOp "Not implemented"

    let private toCumulativeGrowth' (growth:GrowthSeries<_>) =
        match growth with
        | Absolute g ->
            let time = g |> TimeSeries.toObservations |> Seq.map snd
            let agr = g |> TimeSeries.toObservations |> Seq.map fst
            let biomass = agr |> Seq.scan (+) 0.<_> |> Seq.tail |> Seq.toList
            TimeSeries.fromObservations (Seq.zip biomass time) |> Cumulative
        | Relative _ -> failwith "Not implemented"
        | Cumulative _ -> growth

    let private toRelativeGrowth' (growth:GrowthSeries<_>) =
        match growth with
        | Absolute g -> 
            let time = g |> TimeSeries.toObservations |> Seq.map snd
            let agr = g |> TimeSeries.toObservations |> Seq.map fst
            let biomass = agr |> Seq.scan (+) 0.<_> |> Seq.tail |> Seq.toList
            let rgr = agr |> Seq.mapi (fun i a -> a / biomass.[i] * 1.<_> )
            TimeSeries.fromObservations (Seq.zip rgr time) |> Relative
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

    let bound (plant:PlantIndividual) =
        let allSeries = 
            let response = plant.Growth |> growthSeries |> growthToTime
            let envSeries = plant.Environment |> Map.toList |> List.map snd
            response :: envSeries
        let startDates, endDates =
            allSeries
            |> List.map (fun s -> (s.StartDate, TimeSeries.endDate s))
            |> List.unzip
        let startDate = startDates |> List.map snd |> List.max
        let endDate = endDates |> List.min
        let mapts f = TimeSeries.map f
        { plant with Environment = plant.Environment |> Map.toList |> List.map (fun (x,y) -> x,y |> TimeSeries.bound startDate endDate) |> Map.ofList
                     Growth = plant.Growth |> growthSeries |> growthToTime |> TimeSeries.bound startDate endDate |> mapts (fun (x,t) -> x * 1.<mm>) |> Absolute |> RingWidth }

    let keepCommonYears (plant:PlantIndividual) =
        let allSeries = 
            let response = plant.Growth |> growthSeries |> growthToTime
            let envSeries = plant.Environment |> Map.toList |> List.map snd
            response :: envSeries
        let commonDates = 
            allSeries 
            |> List.collect (TimeSeries.dates >> Seq.toList)
            |> List.groupBy id
            |> List.where(fun x -> x |> snd |> Seq.length = allSeries.Length)
            |> List.map fst
        let mapts f = TimeSeries.map f
        let makeCommonTime y = 
            let common = commonDates |> List.map (fun t -> ((y |> TimeSeries.findExact t)))
            common |> TimeSeries.fromObservations
        { plant with Environment = plant.Environment |> Map.toList |> List.map (fun (x,y) -> (x, makeCommonTime y)) |> Map.ofList
                     Growth = plant.Growth |> growthSeries |> growthToTime |> makeCommonTime |> mapts (fun (x,t) -> x * 1.<mm>) |> Absolute |> RingWidth }
