open FSharp.Data

#I @"../../../.paket/load/netstandard2.0"
#load @"FSharp.Data.fsx"

#load "Vega.fs"

open System
open System.IO
open CGFSHelper.Plots

let example_1 = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "examples", "vega-example-1.json"))

Vega.plot_raw_specification example_1

open FSharp.Data

let inline private ts (value : string)  = JsonValue.String value
let inline private tni (value : int)    = JsonValue.Number (decimal(value))
let inline private tlo< ^T when ^T : (member ToJson : unit -> JsonValue)> (input : ^T list) =
    if List.isEmpty input 
    then None
    else
        input
        |> List.map (fun (i : ^T) -> (^T : (member ToJson : unit -> JsonValue) i)
        ) |> Array.ofList |> JsonValue.Array |> Some

let inline private tl< ^T when ^T : (member ToJson : unit -> JsonValue)> (input : ^T list) =
    input
    |> List.map (fun (i : ^T) -> (^T : (member ToJson : unit -> JsonValue) i)
    ) |> Array.ofList |> JsonValue.Array

let map label value = label, value

let inline private mapo label value = value |> Option.map (fun value -> label, value)

let inline private mapoj< ^T when ^T : (member ToJson : unit -> JsonValue)> label value =
    value |> Option.map (fun value -> label, (^T : (member ToJson : unit -> JsonValue) value))


type Color =
| Hex   of string
| RGB   of int * int * int
| Name  of string
with
    member this.ToJson() =
        match this with
        | Hex name      -> ts name
        | RGB (r, g, b) -> sprintf "rgb(%d, %d, %d)" r g b |> ts
        | Name name     -> ts name

type Padding =
| Symmetric of int
| Asymetric of Left:int * Top:int * Right:int * Bottom:int
with
    static member Make (padding : int) = Symmetric padding
    static member Make (left, top, right, bottom) = Asymetric (left, top, right, bottom)
    member this.ToJson() =
        match this with
        | Symmetric padding                     -> tni padding
        | Asymetric (left, top, right, bottom)  ->
            JsonValue.Record (
                [|
                    ("left",    tni left)
                    ("top",     tni top)
                    ("right",   tni right)
                    ("bottom",  tni bottom)
                |]
            )

type SizingFormat =
| Pad
| Fit
| FitX
| FitY
| NoSizing
with
    member this.ToJson() =
        match this with
        | Pad       -> ts "pad"
        | Fit       -> ts "fit"
        | FitX      -> ts "fit-x"
        | FitY      -> ts "fit-y"
        | NoSizing  -> ts "none"

type SizeContains =
| Content
| Padding
with
    member this.ToJson() =
        match this with
        | Content   -> ts "content"
        | Padding   -> ts "padding"

type DetailedSize = {
    Type        : SizingFormat
    Resize      : bool
    Contains    : SizeContains
}
with
    member this.ToJson() =
        let resize =
            if this.Resize then "true" else "false"
            |> JsonValue.String

        [|
            ("type",        this.Type.ToJson())
            ("resize",      resize)
            ("contains",    this.Contains.ToJson())
        |]
        |> JsonValue.Record

type AutoSize =
| Sizing of SizingFormat
| DetailedSize of DetailedSize
with
    member this.ToJson() =
        match this with
        | Sizing fm         -> fm.ToJson()
        | DetailedSize ds   -> ds.ToJson()


type Config = | Config
with
    member this.ToJson() =
        [|
        |]
        |> JsonValue.Record

type Signal = | Signal
with
    member this.ToJson() =
        [|
        |]
        |> JsonValue.Record

type Scale = | Scale
with
    member this.ToJson() =
        [|
        |]
        |> JsonValue.Record

type Projection = | Projection
with
    member this.ToJson() =
        [|
        |]
        |> JsonValue.Record

type Axis = | Axis
with
    member this.ToJson() =
        [|
        |]
        |> JsonValue.Record

type Legend = | Legend
with
    member this.ToJson() =
        [|
        |]
        |> JsonValue.Record

type Text =
| Line  of string
| MultiLine of string list
with
    member this.ToJson() =
        match this with
        | Line text         -> ts text
        | MultiLine lines   -> lines |> List.map ts |> Array.ofList |> JsonValue.Array

type Data = {
    Name        : string
}
with
    member this.ToJson() =
        [|
            map     "name"  (ts this.Name)
        |]
        |> JsonValue.Record

type Title = {
    Text    : Text
}
with
    member this.ToJson() =
        let text = this.Text.ToJson()

        [
            map "text"  text
        ]
        |> Array.ofList
        |> JsonValue.Record

type Mark = | Mark
with
    member this.ToJson() =
        [|
        |]
        |> JsonValue.Record

type Encode = | Encode
with
    member this.ToJson() =
        [|
        |]
        |> JsonValue.Record

type Specification = {
    Schema          : Uri
    Description     : string
    Color           : Color option
    Width           : decimal
    Height          : decimal
    Padding         : Padding
    AutoSize        : AutoSize option
    Config          : Config option
    Signals         : Signal list
    Data            : Data list
    Scales          : Scale list
    Projections     : Projection list
    Axes            : Axis list
    Legends         : Legend list
    Title           : Title option
    Marks           : Mark list
    Encode          : Encode option
    Metadata        : JsonValue option
}
with
    static member Create =
        {
            Schema          = Uri("https://vega.github.io/schema/vega/v5.json")
            Description     = ""
            Color           = None
            Width           = 400m
            Height          = 200m
            Padding         = Padding.Make 5
            AutoSize        = None
            Config          = None
            Signals         = []
            Data            = []
            Scales          = []
            Projections     = []
            Axes            = []
            Legends         = []
            Title           = None
            Marks           = []
            Encode          = None
            Metadata        = None
        }

    member this.ToJson () =
        let header = [
            ("$schema",     JsonValue.String this.Schema.AbsoluteUri)
            ("description", JsonValue.String this.Description)
            ("width",       JsonValue.Number this.Width)
            ("padding",     this.Padding.ToJson())
        ]

        let signals     = tlo this.Signals
        let projections = tlo this.Projections
        let legends     = tlo this.Legends
        let marks       = tlo this.Marks

        let data    = tl this.Data
        let scales  = tl this.Scales
        let axis    = tl this.Axes

        let optional = 
            [
                mapoj   "color"         this.Color
                mapoj   "autosize"      this.AutoSize
                mapoj   "config"        this.Config
                mapo    "signals"       signals
                mapo    "projections"   projections
                mapo    "legends"       legends
                mapoj   "title"         this.Title
                mapo    "marks"         marks
                mapoj   "encode"        this.Encode
                mapo    "metadata"      this.Metadata
            ]

        let all_properties = 
            header @
            (optional |> List.choose id) @
            [
                map "data"      data
                map "scales"    scales
                map "axes"      axis
            ]

        JsonValue.Record (Array.ofList all_properties)

#r "System.Runtime.Serialization.dll"
open System.Runtime.Serialization

[<DataContract>]
type ExampleData = {
    [<field : DataMember>]
    Category    : string
    [<field : DataMember>]
    Amount      : int
}
with
    static member Make (category, amount) = { Category = category; Amount = amount }

let data = [ExampleData.Make("A", 28); ExampleData.Make("B", 55); ExampleData.Make("C", 43)]

#load "System.Text.Encodings.Web.fsx"
#load "System.Text.Json.fsx"
System.Text.Json.JsonSerializer.Serialize(data)

