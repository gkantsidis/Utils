namespace CGFSHelper.Units.Prefix

/// Metrics prefixes based on ten
module Decadic =
    open System
    open Microsoft.FSharp.Core

    /// The base of the decadic metric prefix
    [<Measure>] type deca
    /// A metric prefix for 100
    [<Measure>] type hekto = deca^2
    [<Measure>] type kilo  = deca^3
    [<Measure>] type mega  = deca^6
    [<Measure>] type giga  = deca^9
    [<Measure>] type tera  = deca^12
    [<Measure>] type peta  = deca^15
    [<Measure>] type exa   = deca^18
    [<Measure>] type zetta = deca^21
    [<Measure>] type yotta = deca^24

    [<Measure>] type deci  = deca^-1
    [<Measure>] type centi = deca^-2
    [<Measure>] type milli = deca^-3
    [<Measure>] type micro = deca^-6
    [<Measure>] type nano  = deca^-9
    [<Measure>] type pico  = deca^-12
    [<Measure>] type femto = deca^-15
    [<Measure>] type atto  = deca^-18
    [<Measure>] type zepto = deca^-21
    [<Measure>] type yocto = deca^-24

    module Constants =
        let yocto = 1.0 / 1_000_000_000_000_000_000_000_000.0
        let zepto = 1.0 / 1_000_000_000_000_000_000_000.0
        let atto  = 1.0 / 1_000_000_000_000_000_000.0
        let femto = 1.0 / 1_000_000_000_000_000.0
        let pico  = 1.0 / 1_000_000_000_000.0
        let nano  = 1.0 / 1_000_000_000.0
        let micro = 1.0 / 1_000_000.0
        let milli = 1.0 / 1_000.0
        let centi = 1.0 / 100.0
        let deci  = 1.0 / 10.0

        let deca  = 10.0
        let hekto = 100.0
        let kilo  = 1_000.0
        let mega  = 1_000_000.0
        let giga  = 1_000_000_000.0
        let tera  = 1_000_000_000_000.0
        let peta  = 1_000_000_000_000_000.0
        let exa   = 1_000_000_000_000_000_000.0
        let zetta = 1_000_000_000_000_000_000_000.0
        let yotta = 1_000_000_000_000_000_000_000_000.0

    (*
    * The following are used to force convert a value to a unit. In the process, the original units are erased
    *)

    /// Force convert a value to a number with a yocto metric prefix, e.g. yockto 5.0<'T> gives 5.0<yocto> for all 'T convertible to float.
    let inline yocto (value : 'T) : float<yocto> = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a zepto metric prefix, e.g. zepto 5.0<'T> gives 5.0<zepto> for all 'T convertible to float.
    let inline zepto (value : 'T) : float<zepto> = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a atto metric prefix, e.g. atto 5.0<'T> gives 5.0<atto> for all 'T convertible to float.
    let inline atto (value : 'T)  : float<atto>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a femto metric prefix, e.g. femto 5.0<'T> gives 5.0<femto> for all 'T convertible to float.
    let inline femto (value : 'T) : float<femto> = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a pico metric prefix, e.g. pico 5.0<'T> gives 5.0<pico> for all 'T convertible to float.
    let inline pico (value : 'T)  : float<pico>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a nano metric prefix, e.g. nano 5.0<'T> gives 5.0<nano> for all 'T convertible to float.
    let inline nano (value : 'T)  : float<nano>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a micro metric prefix, e.g. micro 5.0<'T> gives 5.0<micro> for all 'T convertible to float.
    let inline micro (value : 'T) : float<micro> = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a milli metric prefix, e.g. milli 5.0<'T> gives 5.0<milli> for all 'T convertible to float.
    let inline milli (value : 'T) : float<milli> = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a centi metric prefix, e.g. centi 5.0<'T> gives 5.0<centi> for all 'T convertible to float.
    let inline centi (value : 'T) : float<centi> = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a deci metric prefix, e.g. deci 5.0<'T> gives 5.0<deci> for all 'T convertible to float.
    let inline deci (value : 'T)  : float<deci>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure

    /// Force convert a value to a number with a kilo metric prefix, e.g. kilo 5.0<'T> gives 5.0<kilo> for all 'T convertible to float.
    let inline kilo (value : 'T)  : float<kilo>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a mega metric prefix, e.g. mega 5.0<'T> gives 5.0<mega> for all 'T convertible to float.
    let inline mega (value : 'T)  : float<mega>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a giga metric prefix, e.g. giga 5.0<'T> gives 5.0<giga> for all 'T convertible to float.
    let inline giga (value : 'T)  : float<giga>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a tera metric prefix, e.g. tera 5.0<'T> gives 5.0<tera> for all 'T convertible to float.
    let inline tera (value : 'T)  : float<tera>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a peta metric prefix, e.g. peta 5.0<'T> gives 5.0<peta> for all 'T convertible to float.
    let inline peta (value : 'T)  : float<peta>  = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a exa metric prefix, e.g. exa 5.0<'T> gives 5.0<exa> for all 'T convertible to float.
    let inline exa  (value : 'T)  : float<exa>   = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a zetta metric prefix, e.g. zetta 5.0<'T> gives 5.0<zetta> for all 'T convertible to float.
    let inline zetta (value : 'T) : float<zetta> = Operators.float value |> LanguagePrimitives.FloatWithMeasure
    /// Force convert a value to a number with a kilo metric prefix, e.g. kilo 5.0<'T> gives 5.0<kilo> for all 'T convertible to float.
    let inline yotta (value : 'T) : float<yotta> = Operators.float value |> LanguagePrimitives.FloatWithMeasure

    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_atto (value  : float<atto * 'u>)  : float<'u> = value * Constants.atto / 1.0<atto>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_femto (value : float<femto * 'u>) : float<'u> = value * Constants.femto / 1.0<femto>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_pico  (value : float<pico * 'u>)  : float<'u> = value * Constants.pico  / 1.0<pico>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_nano  (value : float<nano * 'u>)  : float<'u> = value * Constants.nano  / 1.0<nano>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_micro (value : float<micro * 'u>) : float<'u> = value * Constants.micro / 1.0<micro>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_milli (value : float<milli * 'u>) : float<'u> = value * Constants.milli / 1.0<milli>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_centi (value : float<centi * 'u>) : float<'u> = value * Constants.centi / 1.0<centi>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_deci  (value : float<deci * 'u>)  : float<'u> = value * Constants.deci  / 1.0<deci>

    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_deca  (value : float<deca * 'u>)  : float<'u> = value * Constants.deca  / 1.0<deca>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_hekto (value : float<hekto * 'u>) : float<'u> = value * Constants.hekto / 1.0<hekto>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_kilo (value : float<kilo * 'u>)   : float<'u> = value * Constants.kilo  / 1.0<kilo>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_mega (value : float<mega * 'u>)   : float<'u> = value * Constants.mega  / 1.0<mega>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_giga (value : float<giga * 'u>)   : float<'u> = value * Constants.giga  / 1.0<giga>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_tera (value : float<tera * 'u>)   : float<'u> = value * Constants.tera  / 1.0<tera>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_peta (value : float<peta * 'u>)   : float<'u> = value * Constants.peta  / 1.0<peta>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_exa  (value : float<exa * 'u>)    : float<'u> = value * Constants.exa   / 1.0<exa>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_zetta (value : float<zetta * 'u>) : float<'u> = value * Constants.zetta / 1.0<zetta>
    /// Convert a value to the standard representation by removing the metric prefix
    let inline from_yotta (value : float<yotta * 'u>) : float<'u> = value * Constants.yotta / 1.0<yotta>

    /// Multiplier to convert a number so that it has the atto metrix prefix
    let to_atto  = 1.0<atto> / Constants.atto
    /// Multiplier to convert a number so that it has the femto metric prefix
    let to_femto = 1.0<femto> / Constants.femto
    /// Multiplier to convert a number so that it has the pico metric prefix
    let to_pico  = 1.0<pico> / Constants.pico
    /// Multiplier to convert a number so that it has the nano metric prefix
    let to_nano  = 1.0<nano> / Constants.nano
    /// Multiplier to convert a number so that it has the micro metric prefix
    let to_micro = 1.0<micro> / Constants.micro
    /// Multiplier to convert a number so that it has the milli metric prefix
    let to_milli = 1.0<milli> / Constants.milli
    /// Multiplier to convert a number so that it has the centi metric prefix
    let to_centi = 1.0<centi> / Constants.centi
    /// Multiplier to convert a number so that it has the deci metric prefix
    let to_deci  = 1.0<deci> / Constants.deci

    /// Multiplier to convert a number so that it has the deca metric prefix
    let to_deca  = 1.0<deca> / Constants.deca
    /// Multiplier to convert a number so that it has the hekto metric prefix
    let to_hekto = 1.0<hekto> / Constants.hekto
    /// Multiplier to convert a number so that it has the kilo metric prefix
    let to_kilo  = 1.0<kilo> / Constants.kilo
    /// Multiplier to convert a number so that it has the mega metric prefix
    let to_mega  = 1.0<mega> / Constants.mega
    /// Multiplier to convert a number so that it has the giga metric prefix
    let to_giga  = 1.0<giga> / Constants.giga
    /// Multiplier to convert a number so that it has the tera metric prefix
    let to_tera  = 1.0<tera> / Constants.tera
    /// Multiplier to convert a number so that it has the peta metric prefix
    let to_peta  = 1.0<peta> / Constants.peta
    /// Multiplier to convert a number so that it has the peta metric prefix
    let to_exa   = 1.0<exa> / Constants.exa
    /// Multiplier to convert a number so that it has the peta metric prefix
    let to_zetta = 1.0<zetta> / Constants.zetta
    /// Multiplier to convert a number so that it has the peta metric prefix
    let to_yotta = 1.0<yotta> / Constants.yotta

    type Convert =
        static member ToFloat<[<Measure>]'T> (value : int<'T>) : float<'T> = int value |> float |> LanguagePrimitives.FloatWithMeasure
        static member ToFloat<[<Measure>]'T> (value : int16<'T>) : float<'T> = int16 value |> float |> LanguagePrimitives.FloatWithMeasure
        static member ToFloat<[<Measure>]'T> (value : int64<'T>) : float<'T> = int64 value |> float |> LanguagePrimitives.FloatWithMeasure
        static member ToFloat<[<Measure>]'T> (value : decimal<'T>) : float<'T> = decimal value |> float |> LanguagePrimitives.FloatWithMeasure
        static member ToFloat<[<Measure>]'T> (value : float<'T>) : float<'T> = value
        static member ToFloat<[<Measure>]'T> (value : float32<'T>) : float<'T> = float32 value |> float |> LanguagePrimitives.FloatWithMeasure

        static member ToFloat32<[<Measure>]'T> (value : int<'T>) : float32<'T> = int value |> float32 |> LanguagePrimitives.Float32WithMeasure
        static member ToFloat32<[<Measure>]'T> (value : int16<'T>) : float32<'T> = int16 value |> float32 |> LanguagePrimitives.Float32WithMeasure
        static member ToFloat32<[<Measure>]'T> (value : int64<'T>) : float32<'T> = int64 value |> float32 |> LanguagePrimitives.Float32WithMeasure
        static member ToFloat32<[<Measure>]'T> (value : decimal<'T>) : float32<'T> = decimal value |> float32 |> LanguagePrimitives.Float32WithMeasure
        static member ToFloat32<[<Measure>]'T> (value : float<'T>) : float32<'T> = float value |> float32 |> LanguagePrimitives.Float32WithMeasure
        static member ToFloat32<[<Measure>]'T> (value : float32<'T>) : float32<'T> = value

        static member ToDecimal<[<Measure>]'T> (value : int<'T>) : decimal<'T> = int value |> decimal |> LanguagePrimitives.DecimalWithMeasure
        static member ToDecimal<[<Measure>]'T> (value : int16<'T>) : decimal<'T> = int16 value |> decimal |> LanguagePrimitives.DecimalWithMeasure
        static member ToDecimal<[<Measure>]'T> (value : int64<'T>) : decimal<'T> = int64 value |> decimal |> LanguagePrimitives.DecimalWithMeasure
        static member ToDecimal<[<Measure>]'T> (value : decimal<'T>) : decimal<'T> = value
        static member ToDecimal<[<Measure>]'T> (value : float<'T>) : decimal<'T> = float value |> decimal |> LanguagePrimitives.DecimalWithMeasure
        static member ToDecimal<[<Measure>]'T> (value : float32<'T>) : decimal<'T> = float value |> decimal |> LanguagePrimitives.DecimalWithMeasure

        static member ToInt<[<Measure>]'T> (value : int<'T>) : int<'T> = value
        static member ToInt<[<Measure>]'T> (value : int16<'T>) : int<'T> = int16 value |> int |> LanguagePrimitives.Int32WithMeasure
        static member ToInt<[<Measure>]'T> (value : int64<'T>) : int<'T> = int64 value |> int |> LanguagePrimitives.Int32WithMeasure
        static member ToInt<[<Measure>]'T> (value : decimal<'T>) : int<'T> = decimal value |> int |> LanguagePrimitives.Int32WithMeasure
        static member ToInt<[<Measure>]'T> (value : float<'T>) : int<'T> = float value |> int |> LanguagePrimitives.Int32WithMeasure
        static member ToInt<[<Measure>]'T> (value : float32<'T>) : int<'T> = float32 value |> int |> LanguagePrimitives.Int32WithMeasure

        static member ToInt16<[<Measure>]'T> (value : int<'T>) : int16<'T> = int value |> int16 |> LanguagePrimitives.Int16WithMeasure
        static member ToInt16<[<Measure>]'T> (value : int16<'T>) : int16<'T> = value
        static member ToInt16<[<Measure>]'T> (value : int64<'T>) : int16<'T> = int64 value |> int16 |> LanguagePrimitives.Int16WithMeasure
        static member ToInt16<[<Measure>]'T> (value : decimal<'T>) : int16<'T> = decimal value |> int16 |> LanguagePrimitives.Int16WithMeasure
        static member ToInt16<[<Measure>]'T> (value : float<'T>) : int16<'T> = float value |> int16 |> LanguagePrimitives.Int16WithMeasure
        static member ToInt16<[<Measure>]'T> (value : float32<'T>) : int16<'T> = float32 value |> int16 |> LanguagePrimitives.Int16WithMeasure

        static member ToInt64<[<Measure>]'T> (value : int<'T>) : int64<'T> = int value |> int64 |> LanguagePrimitives.Int64WithMeasure
        static member ToInt64<[<Measure>]'T> (value : int16<'T>) : int64<'T> = int16 value |> int64 |> LanguagePrimitives.Int64WithMeasure
        static member ToInt64<[<Measure>]'T> (value : int64<'T>) : int64<'T> = value
        static member ToInt64<[<Measure>]'T> (value : decimal<'T>) : int64<'T> = decimal value |> int64 |> LanguagePrimitives.Int64WithMeasure
        static member ToInt64<[<Measure>]'T> (value : float<'T>) : int64<'T> = float value |> int64 |> LanguagePrimitives.Int64WithMeasure
        static member ToInt64<[<Measure>]'T> (value : float32<'T>) : int64<'T> = float32 value |> int64 |> LanguagePrimitives.Int64WithMeasure

        static member ToAtto<[<Measure>]'T> (value : float<'T>) : float<atto * 'T> = (1.0<atto> / Constants.atto) * value
        static member ToAtto<[<Measure>]'T> (value : float32<'T>) : float32<atto * 'T> = Convert.ToFloat32(1.0<atto> / Constants.atto) * value
        static member ToAtto<[<Measure>]'T> (value : decimal<'T>) : decimal<atto * 'T> = (1.0M<atto> / (decimal Constants.atto)) * value
        // static member ToAtto<[<Measure>]'T> (value : int<'T>) : int<atto * 'T> = failwith "Cannot convert"
        // static member ToAtto<[<Measure>]'T> (value : int16<'T>) : int16<atto * 'T> = failwith "Cannot convert"
        static member ToAtto<[<Measure>]'T> (value : int64<'T>) : int64<atto * 'T> = 1L<atto> * int64(1.0 / Constants.atto) * value
        static member FromAtto<[<Measure>]'T> (value  : float<atto * 'T>)  : float<'T> = value * Constants.atto / 1.0<atto>
        static member FromAtto<[<Measure>]'T> (value  : float32<atto * 'T>)  : float32<'T> = value * Convert.ToFloat32(Constants.atto / 1.0<atto>)
        static member FromAtto<[<Measure>]'T> (value  : decimal<atto * 'T>)  : decimal<'T> = value * decimal(Constants.atto) / 1.0M<atto>
        // static member FromAtto<[<Measure>]'T> (value  : int<atto * 'T>)  : int<'T> = failwith "Cannot convert"
        // static member FromAtto<[<Measure>]'T> (value  : int16<atto * 'T>)  : int16<'T> = failwith "Cannot convert"
        static member FromAtto<[<Measure>]'T> (value  : int64<atto * 'T>)  : int64<'T> = Convert.ToFloat(value) * (Constants.atto / 1.0<atto>) |> Convert.ToInt64

        static member ToKilo<[<Measure>]'T> (value : float<'T>) : float<kilo * 'T> = (1.0<kilo> / Constants.kilo) * value
        static member ToKilo<[<Measure>]'T> (value : float32<'T>) : float32<kilo * 'T> = Convert.ToFloat32(1.0<kilo> / Constants.kilo) * value
        static member ToKilo<[<Measure>]'T> (value : decimal<'T>) : decimal<kilo * 'T> = (1.0M<kilo> / (decimal Constants.kilo)) * value
        static member ToKilo<[<Measure>]'T> (value : int<'T>) : int<kilo * 'T> = 1.0<kilo> * (Convert.ToFloat(value) / Constants.kilo) |> Convert.ToInt
        static member ToKilo<[<Measure>]'T> (value : int16<'T>) : int16<kilo * 'T> = 1.0<kilo> * (Convert.ToFloat(value) / Constants.kilo) |> Convert.ToInt16
        static member ToKilo<[<Measure>]'T> (value : int64<'T>) : int64<kilo * 'T> = 1.0<kilo> * (Convert.ToFloat(value) / Constants.kilo) |> Convert.ToInt64
        static member FromKilo<[<Measure>]'T> (value  : float<kilo * 'T>)  : float<'T> = value * Constants.kilo / 1.0<kilo>
        static member FromKilo<[<Measure>]'T> (value  : float32<kilo * 'T>)  : float32<'T> = value * Convert.ToFloat32(Constants.kilo / 1.0<kilo>)
        static member FromKilo<[<Measure>]'T> (value  : decimal<kilo * 'T>)  : decimal<'T> = value * decimal(Constants.kilo) / 1.0M<kilo>
        static member FromKilo<[<Measure>]'T> (value  : int<kilo * 'T>)  : int<'T> = Convert.ToFloat(value) * (Constants.kilo / 1.0<kilo>) |> Convert.ToInt
        static member FromKilo<[<Measure>]'T> (value  : int16<kilo * 'T>)  : int16<'T> = Convert.ToFloat(value) * (Constants.kilo / 1.0<kilo>) |> Convert.ToInt16
        static member FromKilo<[<Measure>]'T> (value  : int64<kilo * 'T>)  : int64<'T> = Convert.ToFloat(value) * (Constants.kilo / 1.0<kilo>) |> Convert.ToInt64

    (*
     * Some helper conversions
    *)

    /// Converts a number from giga^(-1) to pico
    let pico_from_giga_inverse (value : float<'u / giga>) : float<pico * 'u> = 1_000.0<pico * giga> * value
    /// Converts a number from giga^(-1) to nano
    let nano_from_giga_inverse (value : float<'u / giga>) : float<nano * 'u> = 1.0<nano * giga> * value
