namespace CGFSHelper.Spatial


// TODO: Documentation of GeoJSON
// TODO: Helper methods to facilitate generating GeoJSON objects
// TODO: Reading GeoJSON from text file
// TODO: Testing of GeoJSON properties
// TODO: Testing of correcting lines that cross meridian
// TODO: Support for foreign members
// TODO: Support for bounding box

/// The GeoJSON standard (RFC 7946).
/// (https://tools.ietf.org/html/rfc7946)
module GeoJSON =
    open System
    open System.Text
    open FSharp.Data
    open GeoLocation

    type GeoJSONException (message:string, ?innerException:exn) =
        inherit ApplicationException(
            message,
            match innerException with | Some ex -> ex | _ -> null)

#if DONT_USE_NLOG
    let inline private display_message prefix message = Printf.printfn "[%s]: %s" prefix message
    let inline private throw fmt    = failwithf fmt
    let inline private warn fmt     = Printf.ksprintf  (display_message "WARNING") fmt
    let inline private debug fmt    = Printf.ksprintf (display_message "DEBUG") fmt
    let inline private error fmt    = Printf.ksprintf (fun msg -> raise (GraphsException msg)) fmt
#else
    open NLog

    /// Logger for this module
    let private _logger = LogManager.GetCurrentClassLogger()

    let inline private throw fmt =
        let do_throw (message : string) =
            _logger.Error message
            raise (GeoJSONException message)
        Printf.ksprintf do_throw fmt

    let inline private warn fmt = Printf.ksprintf _logger.Warn fmt
    let inline private debug fmt = Printf.ksprintf _logger.Debug fmt
    let inline private error fmt = Printf.ksprintf _logger.Error fmt

    #if INTERACTIVE
    // The following are used only in interactive (fsi) to help with enabling disabling
    // logging for particular modules.

    type internal Marker = interface end
    let _full_name = typeof<Marker>.DeclaringType.FullName
    let _name = typeof<Marker>.DeclaringType.Name
    #endif
#endif

    /// Used to capture arbitrary JSON values
    type Properties = JsonValue option

    let private append_properties (sb : StringBuilder) (properties : Properties) =
        if properties.IsSome then
            sb.AppendFormat("[{0}]", properties.Value) |> ignore

    /// The coordinates are refered as position in RFC 7946
    type Position = GeoLocation.Coordinates

    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type Point = Point of Position:Position * Properties:Properties
    with
        static member Make (position : Position)    = Point (position, None)
        static member Make (position, properties)   = Point (position, properties)
        member this.P with get() = let (Point (_, properties)) = this in properties

        member this.ToStringBuilder (sb : StringBuilder) =
            let (Point (point, properties)) = this
            Printf.bprintf sb "%s" point.StructuredFormatDisplay
            append_properties sb properties

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type MultiPoint = MultiPoint of Position:(Position list) * Properties:Properties
    with
        static member Make position                 = MultiPoint (position, None)
        static member Make (position, properties)   = MultiPoint (position, properties)
        member this.P with get() = let (MultiPoint (_, properties)) = this in properties

        member this.ToStringBuilder (sb : StringBuilder) =
            let (MultiPoint (points, properties)) = this
            let points_string =
                points
                |> List.map (fun point -> point.StructuredFormatDisplay)
                |> String.concat ", "

            Printf.bprintf sb "[%s]" points_string
            append_properties sb properties

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    // TODO: Enforce constraint that the list must have at least two items
    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type LineString = LineString of Position:(Position list) * Properties:Properties
    with
        static member Make (points : Position list) =
            assert (points.Length >= 2)
            LineString (points, None)

        static member Make (pointA, pointB) = LineString ([pointA; pointB], None)

        member this.Points = let (LineString (points, _)) = this in points
        member this.P with get() = let (LineString (_, properties)) = this in properties

        member this.ToStringBuilder (sb : StringBuilder) =
            let (LineString (position, properties)) = this
            let coordinates_string = position |> List.map (fun p -> p.StructuredFormatDisplay) |> String.concat "-"
            Printf.bprintf sb "%s" coordinates_string
            append_properties sb properties

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type MultiLineString = MultiLineString of Position:(Position list list) * Properties:Properties
    with
        static member Make (points : Position list list) =
            assert (points.Length > 0)
            assert (points |> List.forall (fun entry -> entry.Length >= 2))
            MultiLineString (points, None)

        static member Make (points : Position list list, properties) =
            assert (points.Length > 0)
            assert (points |> List.forall (fun entry -> entry.Length >= 2))
            MultiLineString (points, properties)

        member this.LineStrings = let (MultiLineString (mls, _)) = this in mls
        member this.P with get() = let (MultiLineString (_, properties)) = this in properties

        member this.ToStringBuilder (sb : StringBuilder) =
            let (MultiLineString (lines, properties)) = this
            let multi_line_string =
                lines
                |> List.map (fun line -> line |> List.map (fun point -> point.StructuredFormatDisplay) |> String.concat "-")
                |> String.concat ",\n"

            Printf.bprintf sb "[%s]" multi_line_string
            append_properties sb properties

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    // TODO: Enforce constraint that the polygon has at least four positions
    // TODO: Enforce polygon constraint that first and last positions are identical
    // TODO: Enforce polygon constraint that exterior rings are counterclockwise and holes clockwise
    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type Polygon = Polygon of Position:(Position list) * Properties:Properties
    with
        static member Make (points : Position list) =
            assert (points.Length >= 4)
            assert (points.Head = points.Item(points.Length - 1))
            Polygon (points, None)

        static member Make (points : Position list, properties) =
            assert (points.Length >= 4)
            assert (points.Head = points.Item(points.Length - 1))
            Polygon (points, properties)

        member this.Points = let (Polygon (points, _)) = this in points
        member this.P with get() = let (Polygon (_, properties)) = this in properties

        member this.ToStringBuilder (sb : StringBuilder) =
            let (Polygon (points, properties)) = this
            let points_string =
                points
                |> List.map (fun point -> point.StructuredFormatDisplay)
                |> String.concat "-"

            Printf.bprintf sb "%s" points_string
            append_properties sb properties

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type MultiPolygon = MultiPolygon of Position:(Position list list) * Properties:Properties
    with
        static member Make (points : Position list list) =
            MultiPolygon (points, None)

        static member Make (points : Position list list, properties) =
            MultiPolygon (points, properties)

        member this.Points = let (MultiPolygon (points, _)) = this in points
        member this.P with get() = let (MultiPolygon (_, properties)) = this in properties

        member this.ToStringBuilder (sb : StringBuilder) =
            let (MultiPolygon (polygons, properties)) = this
            let polygons_string =
                polygons
                |> List.map (
                    fun polygon ->
                        polygon
                        |> List.map (fun point -> point.StructuredFormatDisplay)
                        |> String.concat "-"
                )
                |> String.concat ",\n"

            Printf.bprintf sb "[%s]" polygons_string
            append_properties sb properties

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    let private meridian = Mk.Radian 0.0
    let private cross = Mk.Radian Math.PI

    let IsCuttingMeridian (posA : Position, posB : Position) =
        (posA.Longitude > meridian && posB.Longitude < meridian) ||
        (posA.Longitude < meridian && posB.Longitude > meridian)

    let private normalizeTwo (pointA : Position, pointB : Position) =
        if IsCuttingMeridian(pointA, pointB) then
            let half = Mk.Convert 2.0
            let midLatitude = (pointA.Latitude + pointB.Latitude) / half
            let crossA, crossB =
                if pointA.Longitude > meridian
                then cross, -cross
                else -cross, cross
            let crossA = Coordinates(midLatitude, crossA)
            let crossB = Coordinates(midLatitude, crossB)
            (crossA, crossB) |> Some
        else None

    let private normalizeManyLines (points : Position list) : Position list list =
        assert (points.Length >= 1)

        let rec work (worklist, previous : Position option) (acc : Position list, result : Position list list) =
            match worklist with
            | []        -> acc :: result
            | hd :: tl  ->
                match previous with
                | None  -> work(tl, Some hd) (acc @ [hd], result)
                | Some previous ->
                    match normalizeTwo (previous, hd) with
                    | None  -> work (tl, Some hd) ( acc @ [hd], result)
                    | Some (a, b)   ->
                        let acc = acc @ [a]
                        work (tl, Some hd) ([b; hd], acc :: result)

        work (points, None) ([], []) |> List.rev

    let private normalizePolygon (points : Position list) : Position list list =
        assert (points.Length >= 4)

        // TODO: What if the starting point is on the meridian?

        let rec processMain (worklist : Position list, previous : Position option) (main : Position list, result : Position list list) =
            match worklist with
            | []        -> main :: result
            | hd :: tl  ->
                match previous with
                | None  -> processMain (tl, Some hd) (main @ [hd], result)
                | Some previous ->
                    match normalizeTwo (previous, hd) with
                    | None  -> processMain (tl, Some hd) (main @ [hd], result)
                    | Some (a, b)   ->
                        processOther (tl, Some b) (main @ [a], [b; hd], result)
        and processOther (worklist : Position list, previous : Position option) (main : Position list, other : Position list, result : Position list list) =
            assert (previous.IsSome)
            // Since the last element of the polygon must be equal to the first
            assert (worklist.Length > 0)

            let current = worklist.Head
            match normalizeTwo (previous.Value, current) with
            | None  -> processOther (worklist.Tail, Some current) (main, other @ [current], result)
            | Some (a, b)   ->
                let otherPolygon = other @ [current; a; other.Head]
                processMain (worklist.Tail, Some current) (main @ [b; current], result @ [otherPolygon])

        processMain (points, None) ([], [])

    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    [<RequireQualifiedAccess>]
    type GeometryItem =
    | Point of Point
    | MultiPoint of MultiPoint
    | LineString of LineString
    | MultiLineString of MultiLineString
    | Polygon of Polygon
    | MultiPolygon of MultiPolygon
    with
        static member private Normalize (point : Point)     = Point point
        static member private Normalize (mp : MultiPoint)   = MultiPoint mp
        static member private Normalize (ls : LineString)   =
            let result = normalizeManyLines ls.Points
            if result.Length = 1
            then GeometryItem.LineString ls
            else GeometryItem.MultiLineString (MultiLineString.Make (result, ls.P))

        static member private Normalize (mls : MultiLineString) =
            let ls =
                mls.LineStrings
                |> List.collect normalizeManyLines

            GeometryItem.MultiLineString (MultiLineString.Make (ls, mls.P))

        static member private Normalize (polygon : Polygon) =
            let result = normalizePolygon polygon.Points
            if result.Length = 1
            then GeometryItem.Polygon polygon
            else GeometryItem.MultiPolygon (MultiPolygon.Make (result, polygon.P))

        static member private Normalize (polygon : MultiPolygon) =
            let polygons =
                polygon.Points
                |> List.collect normalizePolygon

            GeometryItem.MultiPolygon (MultiPolygon.Make (polygons, polygon.P))

        member this.Normalize() =
            match this with
            | Point _               -> this
            | MultiPoint _          -> this
            | LineString ls         -> GeometryItem.Normalize ls
            | MultiLineString mls   -> GeometryItem.Normalize mls
            | Polygon polygon       -> GeometryItem.Normalize polygon
            | MultiPolygon mplg     -> GeometryItem.Normalize mplg

        member this.ToStringBuilder (sb : StringBuilder) =
            match this with
            | Point point           -> Printf.bprintf sb "Point: ";         point.ToStringBuilder sb
            | MultiPoint points     -> Printf.bprintf sb "MultiPoint: ";    points.ToStringBuilder sb
            | LineString lineString -> Printf.bprintf sb "LineString: ";    lineString.ToStringBuilder sb
            | MultiLineString multiLineString   -> Printf.bprintf sb "MultiLineString: "; multiLineString.ToStringBuilder sb
            | Polygon polygon                   -> Printf.bprintf sb "Polygon: ";         polygon.ToStringBuilder sb
            | MultiPolygon polygon              -> Printf.bprintf sb "MultiPolygon: ";    polygon.ToStringBuilder sb


        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    let normalize (this : GeometryItem) = this.Normalize()

    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type Geometry = Geometry of Item:GeometryItem * Properties:Properties
    with
        static member Make (item : GeometryItem) : Geometry =
            Geometry (item, None)

        member this.Normalize() =
            let (Geometry (item, properties)) = this
            Geometry (item.Normalize(), properties)

        member this.ToStringBuilder (sb : StringBuilder) =
            let (Geometry (item, properties)) = this
            item.ToStringBuilder sb
            append_properties sb properties

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type GeometryCollection = GeometryCollection of Geometry:(Geometry list) * Properties:Properties
    with
        static member Make (item : GeometryItem) : GeometryCollection =
            let item = Geometry.Make item
            GeometryCollection ([item], None)

        static member Make (item : GeometryItem list) : GeometryCollection =
            let item = item |> List.map Geometry.Make
            GeometryCollection (item, None)

        member this.ToStringBuilder (sb : StringBuilder) =
            let (GeometryCollection (items, properties)) = this
            Printf.bprintf sb "Collection: ["
            items |> List.iter (fun item -> item.ToStringBuilder sb; Printf.bprintf sb "\n")
            Printf.bprintf sb "]"
            append_properties sb properties

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    [<RequireQualifiedAccess>]
    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type FeatureObject =
    | Geometry of Geometry
    | Unlocated
    with
        member this.ToStringBuilder (sb : StringBuilder) =
            match this with
            | Geometry geometry -> geometry.ToStringBuilder sb
            | Unlocated         -> Printf.bprintf sb "(unlocated)"

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type Feature = Feature of Geometry:FeatureObject * Properties:Properties
    with
        member this.ToStringBuilder (sb : StringBuilder) =
            let (Feature (geometry, properties)) = this
            geometry.ToStringBuilder sb
            append_properties sb properties

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type FeatureCollection = FeatureCollection of Feature list
    with
        member this.ToStringBuilder (sb : StringBuilder) =
            let (FeatureCollection features) = this
            Printf.bprintf sb "["
            features |> List.iter (fun feature -> feature.ToStringBuilder sb; Printf.bprintf sb "\n")
            Printf.bprintf sb "]"

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    [<RequireQualifiedAccess>]
    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type Document =
    | Geometry      of Geometry
    | Feature       of Feature
    | Collection    of FeatureCollection
    with
        member this.ToStringBuilder (sb : StringBuilder) =
            match this with
            | Geometry geometry     -> geometry.ToStringBuilder sb
            | Feature feature       -> feature.ToStringBuilder sb
            | Collection collection -> collection.ToStringBuilder sb

        member this.StructuredFormatDisplay =
            let sb = StringBuilder()
            this.ToStringBuilder sb
            sb.ToString()

        override this.ToString() = this.StructuredFormatDisplay

    type Convert =
        static member FromDegrees (angle : Degree) =
            let angle = angle |> Mk.DecimalFromDegree
            let rounded = Decimal.Round(angle, 4)
            JsonValue.Number rounded

        static member ToRecordItem (header, value : JsonValue) =
            (header, value)

        static member ToRecordItem (header, value : string) =
            (header, JsonValue.String value)

        static member ToType (value : JsonValue) = ("type", value)
        static member ToType (value : string) = ("type", JsonValue.String value)

        static member ToCoordinates (value : Position) =
            let value = [|
                Convert.FromDegrees value.LonDegrees
                Convert.FromDegrees value.LatDegrees
            |]

            ("coordinates", JsonValue.Array value)

        static member ToCoordinates (value : Position list) =
            let positions =
                value
                |> List.map (
                    fun position ->
                        [|
                            Convert.FromDegrees position.LonDegrees
                            Convert.FromDegrees position.LatDegrees
                        |]
                        |> JsonValue.Array
                )
                |> List.toArray

            ("coordinates", JsonValue.Array positions)

        static member ToCoordinates (value : Position list list) =
            let positions =
                value
                |> List.map (
                    fun everything ->
                        everything
                        |> List.map (
                            fun position ->
                                [|
                                    Convert.FromDegrees position.LonDegrees
                                    Convert.FromDegrees position.LatDegrees
                                |]
                                |> JsonValue.Array
                        )
                        |> List.toArray
                        |> JsonValue.Array
                )
                |> List.toArray

            ("coordinates", JsonValue.Array positions)

        static member AppendProperties (record, properties : Properties) =
            match properties with
            | None      -> record
            | Some p    -> record @ [("properties", p)]

        static member ToJson (Point (position, properties)) =
            let members = [
                Convert.ToType "Point"
                Convert.ToCoordinates position
            ]

            Convert.AppendProperties (members, properties)
            |> List.toArray
            |> JsonValue.Record

        static member ToJson (MultiPoint (points, properties)) =
            let members = [
                Convert.ToType "Point"
                Convert.ToCoordinates points
            ]

            Convert.AppendProperties (members, properties)
            |> List.toArray
            |> JsonValue.Record

        static member ToJson (LineString (points, properties)) =
            let members = [
                Convert.ToType "LineString"
                Convert.ToCoordinates points
            ]

            Convert.AppendProperties (members, properties)
            |> List.toArray
            |> JsonValue.Record

        static member ToJson (MultiLineString (points, properties)) =
            let members = [
                Convert.ToType "MultiLineString"
                Convert.ToCoordinates points
            ]

            Convert.AppendProperties (members, properties)
            |> List.toArray
            |> JsonValue.Record

        static member ToJson (Polygon (points, properties)) =
            let members = [
                Convert.ToType "Polygon"
                Convert.ToCoordinates points
            ]

            Convert.AppendProperties (members, properties)
            |> List.toArray
            |> JsonValue.Record

        static member ToJson (MultiPolygon (points, properties)) =
            let members = [
                Convert.ToType "MultiPolygon"
                Convert.ToCoordinates points
            ]

            Convert.AppendProperties (members, properties)
            |> List.toArray
            |> JsonValue.Record

        static member ToJson (Geometry (item, properties)) =
            let geometry =
                match item with
                | GeometryItem.Point point -> Convert.ToJson point
                | GeometryItem.MultiPoint point -> Convert.ToJson point
                | GeometryItem.LineString point -> Convert.ToJson point
                | GeometryItem.MultiLineString point -> Convert.ToJson point
                | GeometryItem.Polygon point -> Convert.ToJson point
                | GeometryItem.MultiPolygon point -> Convert.ToJson point

            let members = [
                Convert.ToType "Feature"
                Convert.ToRecordItem ("geometry", geometry)
            ]

            Convert.AppendProperties (members, properties)
            |> List.toArray
            |> JsonValue.Record

        static member ToJson (GeometryCollection (geometry, properties)) =
            let features =
                geometry
                |> List.map (fun g -> g.Normalize())
                |> List.map (fun g -> Convert.ToJson g)
                |> List.toArray
                |> JsonValue.Array

            let members = [
                Convert.ToType "FeatureCollection"
                Convert.ToRecordItem ("features", features)
            ]

            Convert.AppendProperties (members, properties)
            |> List.toArray
            |> JsonValue.Record

    module Parser =
        type private Entries = Map<string, JsonValue>
        let private mkEntries (data : (string * JsonValue) []) : Entries =
            data
            |> Array.map (
                fun (key, value) ->
                    key.ToUpperInvariant(), value
            )
            |> Map.ofArray

        let private tryFind (entries : Entries) (key : string) = entries.TryGetValue(key.ToUpperInvariant())
        let private remove (entries : Entries) (key : string) = entries.Remove(key.ToUpperInvariant())

        let inline private assertType (entries : Entries) (ty : string) =
            match tryFind entries "type" with
            | false, _     -> failwithf "Could not find field 'type'"
            | true, (JsonValue.String thisType)     ->
                if ty.Equals(thisType, StringComparison.InvariantCultureIgnoreCase) then
                    // expected
                    ()
                else
                    failwithf "Expected type %s; got %s" ty thisType
            | true, v   -> failwithf "Field 'type' is not of string type; it is %A" v

        let inline private isType (entries : Entries) (ty : string) =
            match tryFind entries "type" with
            | false, _     -> failwithf "Could not find field 'type'"
            | true, (JsonValue.String thisType)     ->
                if ty.Equals(thisType, StringComparison.InvariantCultureIgnoreCase)
                then true
                else false
            | true, v   -> failwithf "Field 'type' is not of string type; it is %A" v

        let inline private GetProperties (data : Entries) =
            match tryFind data "Properties" with
            | false, _      -> None
            | true,  v      -> Some v

        let ParseCoordinates (coordinates : JsonValue) : Position =
            match coordinates with
            | JsonValue.Array coordinates ->
                if coordinates.Length <> 2 then
                    failwithf "Coordinates array should have two elements; it has %d" (coordinates.Length)
                let lat = coordinates.[1]
                let lon = coordinates.[0]
                match lat, lon with
                | JsonValue.Float lat, JsonValue.Float lon  ->
                    Position.MakeFromArcDegreeNaked (lat, lon)
                | JsonValue.Number lat, JsonValue.Number lon ->
                    Position.MakeFromArcDegreeNaked (lat, lon)
                | _, _ ->
                    failwithf "Expected coordinates to be floats; they are (%A, %A)" lat lon
            | _     -> failwithf "Cannot parse %A as coordinates" coordinates

        let private ParseLineString (data : Entries) : LineString =
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

        let private ParseGeometry (data : Entries) : Geometry =
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

        let private ParseFeature (data : Entries) : Feature =
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

        let private ParseFeatureCollection (data : Entries) : FeatureCollection =
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
