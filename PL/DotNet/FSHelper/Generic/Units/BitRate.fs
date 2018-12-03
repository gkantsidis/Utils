namespace CGFSHelper.Units

/// Units that deal with information and data rates
module BitRate =
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
    open Prefix.Decadic

    /// Information Bits (bits without overhead) --- to separate from bits that also include error detection and recovery overheads
    [<Measure>] type ibit

    /// Bits
    [<Measure>] type bit

    [<Measure>] type ibps = ibit / second
    [<Measure>] type ikbps = kilo * ibit / second
    [<Measure>] type imbps = mega * ibit / second
    [<Measure>] type igbps = giga * ibit / second
    [<Measure>] type itbps = tera * ibit / second
    [<Measure>] type bps = bit / second
    [<Measure>] type kbps = kilo * bit / second
    [<Measure>] type mbps = mega * bit / second
    [<Measure>] type gbps = giga * bit / second
    [<Measure>] type tbps = tera * bit / second

    /// Force convert a value to a number with a information bit/s metric, e.g. ibps 5.0<'T> gives 5.0<ibit/s> for all 'T convertible to float.
    let inline ibps (value : 'T)  : float<ibps>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a information kbit/s metric, e.g. ikbps 5.0<'T> gives 5.0<kilo*ibit/s> for all 'T convertible to float.
    let inline ikbps (value : 'T) : float<ikbps> = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a information mbit/s metric, e.g. imbps 5.0<'T> gives 5.0<mega*ibit/s> for all 'T convertible to float.
    let inline imbps (value : 'T) : float<imbps> = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a information gbit/s metric, e.g. igbps 5.0<'T> gives 5.0<giga*ibit/s> for all 'T convertible to float.
    let inline igbps (value : 'T) : float<igbps> = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a information tbit/s metric, e.g. itbps 5.0<'T> gives 5.0<tera*ibit/s> for all 'T convertible to float.
    let inline itbps (value : 'T) : float<itbps> = Operators.float value |> LanguagePrimitives.FloatWithMeasure

    /// Force convert a value to a number with a bit/s metric, e.g. bps 5.0<'T> gives 5.0<bit/s> for all 'T convertible to float.
    let inline bps (value : 'T)   : float<bps>   = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a kbit/s metric, e.g. kbps 5.0<'T> gives 5.0<kilo*bit/s> for all 'T convertible to float.
    let inline kbps (value : 'T)  : float<kbps>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a mbit/s metric, e.g. mbps 5.0<'T> gives 5.0<mega*bit/s> for all 'T convertible to float.
    let inline mbps (value : 'T)  : float<mbps>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a gbit/s metric, e.g. gbps 5.0<'T> gives 5.0<giga*bit/s> for all 'T convertible to float.
    let inline gbps (value : 'T)  : float<gbps>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a tbit/s metric, e.g. tbps 5.0<'T> gives 5.0<tera*bit/s> for all 'T convertible to float.
    let inline tbps (value : 'T)  : float<tbps>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure

    /// Convert bits to information bits, by using an efficiency factor
    let convert (efficiency : float<ibit/bit>) (input : float<'u * bit>) : float<'u * ibit> = efficiency * input
    /// Find the overhead bits
    let get_bit_overhead (total : float<bit * 'u>, information : float<ibit * 'u>) : float<bit * 'u> =
        (float total) - (float information) |> LanguagePrimitives.FloatWithMeasure

    /// Force convert information bits to bits
    let ibit_to_bit (input : float<ibit>) : float<bit> = float input |> LanguagePrimitives.FloatWithMeasure
