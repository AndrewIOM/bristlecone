namespace Bristlecone.Dendro

open Bristlecone
open Bristlecone.Time

module Units =

    open FSharp.Data.UnitSystems.SI.UnitNames

    [<Measure>]
    type millimetre

    let mmPerMetre: float<millimetre / metre> = 1000.0<millimetre / metre>


// Environmental space contains

[<RequireQualifiedAccess>]
module PlantIndividual =

    open Units

    type Trait<'date, 'timeunit, 'timespan> =
        | StaticInTime of float
        | VariableInTime of TimeSeries<float, 'date, 'timeunit, 'timespan>

    /// <summary>A representation of a plant individual and
    /// the environmental conditions local to it.</summary>
    type PlantIndividual<[<Measure>] 'growth, 'date, 'timeunit, 'timespan> =
        { Identifier: ShortCode.ShortCode
          Growth: GrowthSeries.GrowthSeries<'growth, year, 'date, 'timeunit, 'timespan>
          InternalControls: CodedMap<Trait<'date, 'timeunit, 'timespan>>
          Environment: CodedMap<TimeSeries<float, 'date, 'timeunit, 'timespan>> }

    let enforceMinimumSize minSize (plant:PlantIndividual<'growthUnit,'date,'timeunit,'timespan>) =
        let cumulative = plant.Growth |> GrowthSeries.cumulative
        let trimmed =
            cumulative
            |> TimeSeries.toObservations
            |> Seq.skipWhile (fun (x,_) -> x < minSize)
            |> TimeSeries.fromObservations cumulative.DateMode
            |> GrowthSeries.Cumulative
        { plant with Growth = trimmed }

    let tailGrowth (plant:PlantIndividual<'growthUnit, 'date,'timeunit,'timespan>) =
        let bounded =  plant.Growth |> GrowthSeries.tail
        { plant with Growth = bounded }

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

    let bound plant =
        let growthDates = plant.Growth |> GrowthSeries.dates
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
            Growth = plant.Growth |> GrowthSeries.bound startDate endDate |> Option.get }

    /// <summary>Where a plant has associated environmental data, only keep observations
    /// where growth and environment time-series are present.</summary>
    let commonTimeline plant =
        let growthDates = plant.Growth |> GrowthSeries.dates |> Seq.toList

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

        let commonPlant = plant.Growth |> GrowthSeries.filter commonDates
        { plant with
            Environment =
                plant.Environment
                |> Map.toList
                |> List.map (fun (x, y) -> (x, makeCommonTime y))
                |> Map.ofList
            Growth = commonPlant }

    let private stripUnits ts =
        ts |> TimeSeries.map (fun (t, v) -> Units.removeUnitFromFloat t)

    let internal forFittingCumulative (code:ShortCode.ShortCode) plant =
        let g = plant.Growth |> GrowthSeries.cumulative |> stripUnits
        plant.Environment |> Map.add code g
