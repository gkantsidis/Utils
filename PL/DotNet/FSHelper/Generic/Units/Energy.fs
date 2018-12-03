namespace CGFSHelper.Units

/// Some convenient units for energy
module Energy =
    open System
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
    open Prefix.Decadic
    open BitRate

    /// Joule per information bit
    [<Measure>] type jpib = joule / ibit
    /// Pico Joule per information bit
    [<Measure>] type pjpib = pico * joule / ibit
    /// PicoJoule per bit
    [<Measure>] type pjpb = pico * joule / bit

    /// decibel-milliwatts
    [<Measure>] type dBm

    /// Convert dBm to milli Watt
    let from_dBm (input : float<dBm>) : float<milli * watt> =
        let value = float input
        1.0<milli*watt> * Math.Pow(value / 10.0, 10.0)

    /// Convert a value (in mWatt) to dBm
    let to_dBm (input : float<milli * watt>) : float<dBm> =
        let ratio = input / 1.0<milli*watt>
        10.0 * Math.Log10(ratio) |> LanguagePrimitives.FloatWithMeasure
