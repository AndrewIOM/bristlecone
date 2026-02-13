namespace Bristlecone.Dendro

open System
open Bristlecone.Units

module Environment =

    [<Measure>]
    type celsius

    /// <summary>Generate synthetic environmental data, for example for model testing.</summary>
    module Synthetic =

        open Bristlecone.Test
        open Bristlecone.Time

        /// <summary>Generate smooth temperature profiles that represent
        /// seasonal cycling.</summary>
        /// <param name="mean"></param>
        /// <param name="amplitude"></param>
        /// <param name="periodDays"></param>
        /// <param name="phase">The fraction of the cycle completed at day zero.</param>
        /// <returns>A function that generates </returns>
        let genTemperatureSeasonal mean amplitude (period: float<day>) phase =
            Synthetic.sinusoid mean amplitude period phase

        let genTemperatureNorthernMidLatitude mean amplitude =
            genTemperatureSeasonal mean amplitude 365.<day> 0.0

        let genTemperatureSouthernMidLatitude mean amplitude =
            genTemperatureSeasonal mean amplitude 365.<day> 0.0

        let temperatureAnomaly phi sigma seed =
            Synthetic.ar1 phi sigma seed |> Seq.map ((*) 1.<celsius>)
