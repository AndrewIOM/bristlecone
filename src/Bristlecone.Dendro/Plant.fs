namespace Bristlecone.Dendro

module PlantIndividual =

    open Bristlecone
    open EnvironmentalVariables
    open Time

    // Year
    [<Measure>] type year

    // Millimetre
    [<Measure>] type mm

    type PlantGrowth =
        | RingWidth of GrowthSeries.GrowthSeries<mm>
        | BasalArea of GrowthSeries.GrowthSeries<mm^2>
        | StemVolume of GrowthSeries.GrowthSeries<mm^3>

    type Trait =
        | Static of float
        | Variable of TimeSeries<float>

    type PlantIndividual = {
        Identifier: ShortCode.ShortCode
        Growth: PlantGrowth
        InternalControls: Map<ShortCode.ShortCode,Trait>
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
            | GrowthSeries.Absolute rws -> rws |> TimeSeries.map (fun (x,t) -> removeUnit x) |> GrowthSeries.Absolute
            | GrowthSeries.Cumulative m -> m |> TimeSeries.map (fun (x,t) -> removeUnit x) |> GrowthSeries.Cumulative
            | GrowthSeries.Relative rws -> rws |> TimeSeries.map (fun (x,t) -> removeUnit x) |> GrowthSeries.Relative
        | _ -> invalidOp "Not implemented"

    let private toCumulativeGrowth' (growth:GrowthSeries.GrowthSeries<_>) =
        match growth with
        | GrowthSeries.Absolute g ->
            let time = g |> TimeSeries.toObservations |> Seq.map snd
            let agr = g |> TimeSeries.toObservations |> Seq.map fst
            let biomass = agr |> Seq.scan (+) 0.<_> |> Seq.tail |> Seq.toList
            TimeSeries.fromObservations (Seq.zip biomass time) |> GrowthSeries.Cumulative
        | GrowthSeries.Relative _ -> failwith "Not implemented"
        | GrowthSeries.Cumulative _ -> growth

    let private toRelativeGrowth' (growth:GrowthSeries.GrowthSeries<_>) =
        match growth with
        | GrowthSeries.Absolute g -> 
            let time = g |> TimeSeries.toObservations |> Seq.map snd
            let agr = g |> TimeSeries.toObservations |> Seq.map fst
            let biomass = agr |> Seq.scan (+) 0.<_> |> Seq.tail |> Seq.toList
            let rgr = agr |> Seq.mapi (fun i a -> a / biomass.[i] * 1.<_> )
            TimeSeries.fromObservations (Seq.zip rgr time) |> GrowthSeries.Relative
        | GrowthSeries.Relative _ -> growth
        | GrowthSeries.Cumulative g -> 
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
            let response = plant.Growth |> growthSeries |> GrowthSeries.growthToTime
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
                     Growth = plant.Growth |> growthSeries |> GrowthSeries.growthToTime |> TimeSeries.bound startDate endDate |> mapts (fun (x,t) -> x * 1.<mm>) |> GrowthSeries.Absolute |> RingWidth }

    let keepCommonYears (plant:PlantIndividual) =
        let allSeries = 
            let response = plant.Growth |> growthSeries |> GrowthSeries.growthToTime
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
                     Growth = plant.Growth |> growthSeries |> GrowthSeries.growthToTime |> makeCommonTime |> mapts (fun (x,t) -> x * 1.<mm>) |> GrowthSeries.Absolute |> RingWidth }
