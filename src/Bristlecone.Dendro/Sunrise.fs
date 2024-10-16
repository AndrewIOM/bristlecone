namespace Bristlecone.Dendro

open System
open Bristlecone.Units

[<Measure>] type latitude
[<Measure>] type longitude

/// <summary>Provides functions for converting to and from dates in the Julian calendar</summary>
module JulianDate =

    let minutesInDay = 24 * 60
    let secondsInDay = minutesInDay * 60
    let J2000 = 2451545

    let gregorianToJulian (gDate: DateTime) timeZoneOffsetHours =
        let year, month =
            if gDate.Month <= 2 then
                float (gDate.Year - 1), float (gDate.Month + 12)
            else
                float gDate.Year, float gDate.Month

        let a = floor (year / 100.)
        let b = 2. - a + floor (a / 4.)

        let jDay =
            floor (365.25 * (year + 4716.))
            + floor (30.6001 * (month + 1.))
            + (float gDate.Day)
            + b
            - 1524.5

        let jTime =
            (((float gDate.Hour) * (60. * 60.))
             + ((float gDate.Minute) * 60.)
             + (float gDate.Second))
            / (float secondsInDay)

        jDay + jTime - timeZoneOffsetHours / 24.

    let toDate (timeZone: TimeZoneInfo) (gDate: DateTime) (time: float) =
        let additionalDays = floor time
        let hours = int <| floor (time * 24.) % 24.
        let minutes = int <| floor ((time * 24. * 60.) % 60.)
        let seconds = int <| floor ((time * 24. * 60. * 60.) % 60.)

        (DateTimeOffset(gDate.Year, gDate.Month, gDate.Day, hours, minutes, seconds, timeZone.GetUtcOffset(gDate)))
            .AddDays(additionalDays)

    let julianCentury jDate =
        let daysInCentury = 36525.
        (jDate - (float J2000)) / daysInCentury


/// <summary>The sunrise equation can be used to calculate the
/// time of sunrise and sunset for any latitude, longitude,
/// and date.</summary>
/// <remarks>See: https://en.wikipedia.org/wiki/Sunrise_equation#Complete_calculation_on_Earth
/// Adapted from the SolarCalc by NOAA.
/// Source: https://dotnetfiddle.net/N3j5th</remarks>
module Sunrise =

    type DayLength =
        | CompleteLight
        | CompleteDark
        | PartialLight of DateTimeOffset * DateTimeOffset

    let dayFraction dayLength =
        match dayLength with
        | CompleteLight -> 1.
        | CompleteDark -> 0.
        | PartialLight(sr, ss) -> (ss - sr).TotalHours / 24.

    let geomMeanAnomalySun t =
        357.52911 + t * (35999.05029 - 0.0001537 * t)

    let geomMeanLongitudeSun t =
        (280.46646 + t * (36000.76983 + t * 0.0003032)) % 360.

    let eccentricityEarthOrbit t =
        0.016708634 - t * (0.000042037 + 0.0000001267 * t)

    let radians degrees = degrees * Math.PI / 180.

    let degrees radians = radians * 180. / Math.PI

    let equationOfCentreSun ma t =
        sin (radians (ma)) * (1.914602 - t * (0.004817 + 0.000014 * t))
        + sin (radians (2. * ma)) * (0.019993 - 0.000101 * t)
        + sin (radians (3. * ma)) * 0.000289

    let trueLongitudeSun (m1: float) eoc = m1 + eoc

    let apparentLongitudeSun t1 t =
        t1 - 0.00569 - 0.00478 * sin (radians (125.04 - 1934.136 * t))

    let meanObliquityOfEcliptic t =
        23.
        + (26. + ((21.448 - t * (46.815 + t * (0.00059 - t * 0.001813)))) / 60.) / 60.

    let obliquityCorrection oe t =
        oe + 0.00256 * cos (radians (125.04 - 1934.136 * t))

    let equationOfTime oc ml eo ma =
        let y = tan (radians (oc / 2.)) * tan (radians (oc / 2.))

        let eTime =
            y * sin (2. * radians (ml)) - 2. * eo * sin (radians (ma))
            + 4. * eo * y * sin (radians (ma)) * cos (2. * radians (ml))
            - 0.5 * y * y * sin (4. * radians (ml))
            - 1.25 * eo * eo * sin (2. * radians (ma))

        4. * degrees (eTime)

    let declinationOfSun oc al =
        degrees (asin (sin (radians (oc)) * sin (radians (al))))

    let cosHourAngle declinationOfSun (northLatitude: float<latitude>) =
        (sin (radians (-0.83))
         - (sin (radians (removeUnitFromFloat northLatitude)) * sin (radians (declinationOfSun))))
        / (cos (radians (removeUnitFromFloat northLatitude)) * cos (radians (declinationOfSun)))

    let hourAngleSunrise (lat: float<latitude>) d =
        degrees (
            acos (
                cos (radians (90.833)) / (cos (radians (Bristlecone.Units.removeUnitFromFloat lat)) * cos (radians (d)))
                - tan (radians (Bristlecone.Units.removeUnitFromFloat lat)) * tan (radians (d))
            )
        )

    let solarNoon (lng: float<longitude>) eot tzoff =
        (720. - 4. * removeUnitFromFloat lng - eot + tzoff * 60.) / (float JulianDate.minutesInDay)

    let sunrise sn ha =
        sn - ha * 4. / (float JulianDate.minutesInDay)

    let sunset sn ha =
        sn + ha * 4. / (float JulianDate.minutesInDay)

    let calculate year month day (latitude: float<latitude>) (longitude: float<longitude>) timeZoneId =

        let lat = latitude
        let lng = longitude
        let gDate = DateTime(year, month, day, 12, 0, 0, DateTimeKind.Utc)
        let timeZone = TimeZoneInfo.FindSystemTimeZoneById(timeZoneId)
        let timeZoneOffset = timeZone.GetUtcOffset(gDate)
        let tzOffHr = timeZoneOffset.TotalHours
        let jDate = JulianDate.gregorianToJulian gDate tzOffHr
        let t = JulianDate.julianCentury jDate
        let ml = geomMeanLongitudeSun t
        let ma = geomMeanAnomalySun t
        let eo = eccentricityEarthOrbit t
        let eoc = equationOfCentreSun ma t
        let tl = trueLongitudeSun ml eoc
        let al = apparentLongitudeSun tl t
        let oe = meanObliquityOfEcliptic t
        let oc = obliquityCorrection oe t
        let d = declinationOfSun oc al
        let eot = equationOfTime oc ml eo ma

        let cosHa = cosHourAngle d lat

        if cosHa > 1. then
            CompleteDark
        else if cosHa < -1. then
            CompleteLight
        else
            let ha = hourAngleSunrise lat d
            let sn = solarNoon lng eot tzOffHr
            let sunrise = sunrise sn ha
            let sunset = sunset sn ha
            let sunriseOffset = JulianDate.toDate timeZone gDate sunrise
            let sunsetOffset = JulianDate.toDate timeZone gDate sunset
            (sunriseOffset, sunsetOffset) |> PartialLight

    /// <summary>A cache that may be used in model computation where day lengths
    /// are continually accessed for the same dates over and over again. Stashes
    /// each calculated day length per latitude/longitude and time.</summary>
    type DayLengthCache(latitude, longitude, timeZone) =

        let mutable (lightData: Map<DateTime, DayLength>) = [] |> Map.ofList

        /// <summary>Daylight hours </summary>
        /// <param name="date"></param>
        /// <returns></returns>
        member __.GetLight(date) =
            match lightData |> Map.tryFind date with
            | Some l -> l
            | None ->
                let l = calculate date.Year date.Month date.Day latitude longitude timeZone
                lightData <- lightData |> Map.add date l
                l
