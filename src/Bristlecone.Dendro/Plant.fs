namespace Bristlecone.Dendro

open Bristlecone
open Bristlecone.Time

module Units =

    open FSharp.Data.UnitSystems.SI.UnitNames

    [<Measure>]
    type millimetre

    let mmPerMetre : float<millimetre/metre> = 1000.0<millimetre/metre>


// Environmental space contains 

[<RequireQualifiedAccess>]
module PlantIndividual =

    open Units

    module PlantGrowth =

        /// Represents a measurement of plant growth.
        type PlantGrowthMeasure<'date, 'timeunit, 'timespan> =
            | RingWidth of GrowthSeries.GrowthSeries<millimetre,year,'date,'timeunit,'timespan>
            | BasalArea of GrowthSeries.GrowthSeries<millimetre^2,year,'date,'timeunit,'timespan>
            | StemVolume of GrowthSeries.GrowthSeries<millimetre^3,year,'date,'timeunit,'timespan>

        /// Get dates in the growth series
        let internal dates growth =
            match growth with
            | RingWidth rw -> GrowthSeries.dates rw
            | BasalArea a -> GrowthSeries.dates a
            | StemVolume vol -> GrowthSeries.dates vol

        let internal bound startDate endDate growth =
            match growth with
            | RingWidth rw -> rw |> GrowthSeries.bound startDate endDate |> Option.map RingWidth
            | BasalArea a -> GrowthSeries.bound startDate endDate a |> Option.map BasalArea
            | StemVolume vol -> GrowthSeries.bound startDate endDate vol |> Option.map StemVolume



    type Trait<'date, 'timeunit, 'timespan> =
        | StaticInTime of float
        | VariableInTime of TimeSeries<float, 'date, 'timeunit, 'timespan>

    /// <summary>A representation of a plant individual and
    /// the environmental conditions local to it.</summary>
    type PlantIndividual<'date, 'timeunit, 'timespan> =
        { Identifier: ShortCode.ShortCode
          Growth: PlantGrowth.PlantGrowthMeasure<'date, 'timeunit, 'timespan>
          InternalControls: CodedMap<Trait<'date, 'timeunit, 'timespan>>
          Environment: CodedMap<TimeSeries<float, 'date, 'timeunit, 'timespan>> }


    /// <summary>Attach an environmental time-series to a plant individual,
    /// for example for a local environmental condition or resource.</summary>
    let zipEnvironment envName envData plant =
        let code = ShortCode.create envName

        match code with
        | None -> failwith "%s is not a valid environment code."
        | Some c ->
            { plant with
                Environment = plant.Environment.Add(c, envData) }

    /// <summary>Assigns local environmental conditions to each plant in a sequence,
    /// given a sequence of environmental time-series where each time-series
    /// has the code of the plant associated with it.</summary>
    let zipEnvironments envName env plants =
        plants
        |> Seq.map (fun s -> (s.Identifier.Value, s))
        |> Seq.keyMatch env
        |> Seq.map (fun (_, plant, e) -> zipEnvironment envName plant e)

    let growthSeries plant =
        match plant with
        | PlantGrowth.RingWidth rw ->
            match rw with
            | GrowthSeries.Absolute rws -> rws |> TimeSeries.map (fun (x, t) -> removeUnitFromFloat x) |> GrowthSeries.Absolute
            | GrowthSeries.Cumulative m -> m |> TimeSeries.map (fun (x, t) -> removeUnitFromFloat x) |> GrowthSeries.Cumulative
            | GrowthSeries.Relative rws -> rws |> TimeSeries.map (fun (x, t) -> removeUnitFromFloat x) |> GrowthSeries.Relative
        | _ -> invalidOp "Not implemented"

    let bound plant =
        let growthDates = plant.Growth |> PlantGrowth.dates
        let envSeries = plant.Environment |> Map.toList |> List.map snd
        let startDates, endDates =
            envSeries
            |> List.map (fun s -> (s.StartDate |> snd, TimeSeries.endDate s))
            |> List.append [ growthDates |> Seq.head, growthDates |> Seq.last ]
            |> List.unzip

        let startDate = startDates |> List.max
        let endDate = endDates |> List.min

        { plant with
            Environment =
                plant.Environment
                |> Map.toList
                |> List.map (fun (x, y) -> x, y |> TimeSeries.bound startDate endDate |> Option.get)
                |> Map.ofList
            Growth =
                plant.Growth
                |> PlantGrowth.bound startDate endDate
                |> Option.get
        }

    /// <summary>Where a plant has associated environmental data, only keep observations
    /// where growth and environment time-series are present.</summary>
    let commonTimeline plant =
        let growthDates = plant.Growth |> PlantGrowth.dates |> Seq.toList
        let commonDates =
            plant.Environment 
            |> Map.toList 
            |> List.map snd
            |> List.collect (TimeSeries.dates >> Seq.toList)
            |> List.append growthDates
            |> List.groupBy id
            |> List.where (fun x -> x |> snd |> Seq.length = plant.Environment.Count + 1)
            |> List.map fst

        let makeCommonTime y =
            let common = commonDates |> List.map (fun t -> ((y |> TimeSeries.findExact t)))
            common |> TimeSeries.fromObservations y.DateMode

        let commonPlant =
            match plant.Growth with
            | PlantGrowth.RingWidth rw -> rw |> GrowthSeries.filter commonDates |> PlantGrowth.RingWidth
            | PlantGrowth.BasalArea b -> b |> GrowthSeries.filter commonDates |> PlantGrowth.BasalArea
            | PlantGrowth.StemVolume v -> v |> GrowthSeries.filter commonDates |> PlantGrowth.StemVolume

        { plant with
            Environment =
                plant.Environment
                |> Map.toList
                |> List.map (fun (x, y) -> (x, makeCommonTime y))
                |> Map.ofList
            Growth = commonPlant }

    /// <summary>Lower time-series data from a plant individual type into
    /// basic time-series that may be used for model-fitting</summary>
    /// <param name="plant">A plant individual</param>
    /// <returns>A coded map of time-series data for model-fitting</returns>
    let toTimeSeries plant =
        let g =
            match plant.Growth with
            | PlantGrowth.RingWidth g -> GrowthSeries.stripUnits g
            | PlantGrowth.BasalArea g -> GrowthSeries.stripUnits g
            | PlantGrowth.StemVolume g -> GrowthSeries.stripUnits g

        plant.Environment |> Map.add (ShortCode.create "x" |> Option.get) g
