#I @"../../../.paket/load/net47/"
#load @"FSharp.Data.fsx"

#load @"../Collections.fs"
#load "GeoLocation.fs"
#load "GeoJSON.fs"

open System
open System.IO
open FSharp.Data
open FSharp.Data.JsonExtensions

open CGFSHelper.Spatial
open GeoJSON

let example1naked =
    let point = Point.Make(Position.MakeFromArcDegreeNaked(0.5, 102.0))
    let line =
        LineString.Make([
            Position.MakeFromArcDegreeNaked(0.0, 102.0)
            Position.MakeFromArcDegreeNaked(1.0, 103.0)
            Position.MakeFromArcDegreeNaked(0.0, 104.0)
            Position.MakeFromArcDegreeNaked(1.0, 105.0)
        ])
    let polygon =
        Polygon.Make ([
            Position.MakeFromArcDegreeNaked(0.0, 100.0)
            Position.MakeFromArcDegreeNaked(0.0, 101.0)
            Position.MakeFromArcDegreeNaked(1.0, 101.0)
            Position.MakeFromArcDegreeNaked(1.0, 100.0)
            Position.MakeFromArcDegreeNaked(0.0, 100.0)
        ])

    let geometry = [
        GeometryItem.Point point
        GeometryItem.LineString line
        GeometryItem.Polygon polygon
    ]

    GeometryCollection.Make geometry

let e1nj = Convert.ToJson(example1naked)
e1nj.ToString()

let path = (Path.Combine(__SOURCE_DIRECTORY__, @"../../../../../Data/GeoJsonExamples") |> DirectoryInfo).FullName

let us =
    let filename = Path.Combine(path, "gz_2010_us_outline_20m.json")
    let text = File.ReadAllText(filename)
    use reader = new StringReader(text)
    JsonValue.Load(reader)

type Entries = Map<string, JsonValue>
let mkEntries (data : (string * JsonValue) []) : Entries =
    data
    |> Array.map (
        fun (key, value) ->
            key.ToUpperInvariant(), value
    )
    |> Map.ofArray

let tryFind (entries : Entries) (key : string) = entries.TryGetValue(key.ToUpperInvariant())
let remove (entries : Entries) (key : string) = entries.Remove(key.ToUpperInvariant())

let inline assertType (entries : Entries) (ty : string) =
    match tryFind entries "type" with
    | false, _     -> failwithf "Could not find field 'type'"
    | true, (JsonValue.String thisType)     ->
        if ty.Equals(thisType, StringComparison.InvariantCultureIgnoreCase) then
            // expected
            ()
        else
            failwithf "Expected type %s; got %s" ty thisType
    | true, v   -> failwithf "Field 'type' is not of string type; it is %A" v

let inline isType (entries : Entries) (ty : string) =
    match tryFind entries "type" with
    | false, _     -> failwithf "Could not find field 'type'"
    | true, (JsonValue.String thisType)     ->
        if ty.Equals(thisType, StringComparison.InvariantCultureIgnoreCase)
        then true
        else false
    | true, v   -> failwithf "Field 'type' is not of string type; it is %A" v

let inline GetProperties (data : Entries) =
    match tryFind data "Properties" with
    | false, _      -> None
    | true,  v      -> Some v

let ParseCoordinates (coordinates : JsonValue) : Position =
    match coordinates with
    | JsonValue.Array coordinates ->
        if coordinates.Length <> 2 then
            failwithf "Coordinates array should have two elements; it has %d" (coordinates.Length)
        let lat = coordinates.[0]
        let lon = coordinates.[1]
        match lat, lon with
        | JsonValue.Float lat, JsonValue.Float lon  ->
            Position.MakeFromArcDegreeNaked (lat, lon)
        | JsonValue.Number lat, JsonValue.Number lon ->
            Position.MakeFromArcDegreeNaked (lat, lon)
        | _, _ ->
            failwithf "Expected coordinates to be floats; they are (%A, %A)" lat lon
    | _     -> failwithf "Cannot parse %A as coordinates" coordinates

let ParseLineString (data : Entries) : LineString =
    assertType data "LineString"
    let properties = GetProperties data
    let coordinates =
        match tryFind data "Coordinates" with
        | false, _          -> failwithf "Cannot find field 'coordinates' for LineString geometry"
        | true, coordinates ->
            match coordinates with
            | JsonValue.Array coordinates ->
                coordinates |> Array.toList |> List.map ParseCoordinates
            | _     -> failwithf "Don't know how to parse %A as LineString" coordinates

    LineString (coordinates, properties)

let ParseGeometry (data : Entries) : Geometry =
    let geometry =
        match tryFind data "type" with
        | false, _      -> failwithf "Could not find 'type' field in geometry object"
        | true, (JsonValue.String ty)   ->
            if ty.Equals("LineString", StringComparison.InvariantCultureIgnoreCase) then
                ParseLineString data |> GeometryItem.LineString
            else
                failwithf "Don't know how to parse geometry of type %s" ty
        | true, v                       ->
            failwithf "Geometry type should be string; it is %A" v

    Geometry (geometry, None)

let ParseFeature (data : Entries) : Feature =
    assertType data "feature"

    let geometry =
        match tryFind data "Geometry" with
        | false, _
        | true, JsonValue.Null              -> FeatureObject.Unlocated
        | true, JsonValue.Record entries    ->
            let data' = mkEntries entries
            FeatureObject.Geometry (ParseGeometry data')
        | true, v                           ->
            failwithf "Expected record for geometry object; got %A" v

    let properties = GetProperties data

    Feature (geometry, properties)

let ParseFeatureCollection (data : Entries) : FeatureCollection =
    assertType data "FeatureCollection"

    match tryFind data "features" with
    | false, _          -> failwithf "Cannot find field named 'feature' in FeatureCollection"
    | true,  (JsonValue.Array features) ->
        features
        |> Array.toList
        |> List.map (
            fun feature ->
                match feature with
                | JsonValue.Record feature  -> mkEntries feature |> ParseFeature
                | _     -> failwithf "Expected a feature record in feature collection; got %A" feature
        )
        |> FeatureCollection
    | true, v   -> failwithf "Expected the features field to be array; got %A" v




let ParseDocument (value : JsonValue) : Document =
    match value with
    | JsonValue.Record record ->
        let values = mkEntries record

        match tryFind values "type" with
        | false, _  ->
            failwith "Did not find field 'type' in JSON; input does not appear to be valid GeoJSON"
        | true, (JsonValue.String ty) ->
            if ty.Equals("FeatureCollection", StringComparison.InvariantCultureIgnoreCase) then
                ParseFeatureCollection values   |> Document.Collection
            elif ty.Equals("Geometry", StringComparison.InvariantCultureIgnoreCase) then
                ParseGeometry values            |> Document.Geometry
            elif ty.Equals("Feature", StringComparison.InvariantCultureIgnoreCase) then
                ParseFeature values             |> Document.Feature
            else
                failwithf "Unknown type %s" ty
        | true, ty   ->
            failwithf "Field 'type' is expected to be of type string, but it is %A. Input does not appear to be valid GeoJSON" ty

    | _     -> failwithf "Expecting a record"

let xx = ParseDocument us
