namespace CGFSHelper.Spatial

/// The GeoJSON standard (RFC 7946).
/// (https://tools.ietf.org/html/rfc7946)
module GeoJSON =
    open System
    open FSharp.Data
    open GeoLocation

    /// Used to capture arbitrary JSON values
    type Properties = JsonValue option

    /// The coordinates are refered as position in RFC 7946
    type Position = GeoLocation.Coordinates

    type Point = Point of Position:Position * Properties:Properties
    with
        static member Make (position : Position)    = Point (position, None)
        static member Make (position, properties)   = Point (position, properties)
        member this.P with get() = let (Point (_, properties)) = this in properties

    type MultiPoint = MultiPoint of Position:(Position list) * Properties:Properties
    with
        static member Make position                 = MultiPoint (position, None)
        static member Make (position, properties)   = MultiPoint (position, properties)
        member this.P with get() = let (MultiPoint (_, properties)) = this in properties

    // TODO: Enforce constraint that the list must have at least two items
    type LineString = LineString of Position:(Position list) * Properties:Properties
    with
        static member Make (points : Position list) =
            assert (points.Length >= 2)
            LineString (points, None)

        static member Make (pointA, pointB) = LineString ([pointA; pointB], None)

        member this.Points = let (LineString (points, _)) = this in points
        member this.P with get() = let (LineString (_, properties)) = this in properties

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

    // TODO: Enforce constraint that the polygon has at least four positions
    // TODO: Enforce polygon constraint that first and last positions are identical
    // TODO: Enforce polygon constraint that exterior rings are counterclockwise and holes clockwise
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

    type MultiPolygon = MultiPolygon of Position:(Position list list) * Properties:Properties
    with
        static member Make (points : Position list list) =
            MultiPolygon (points, None)

        static member Make (points : Position list list, properties) =
            MultiPolygon (points, properties)

        member this.Points = let (MultiPolygon (points, _)) = this in points
        member this.P with get() = let (MultiPolygon (_, properties)) = this in properties

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

    let normalize (this : GeometryItem) = this.Normalize()

    type Geometry = Geometry of Item:GeometryItem * Properties:Properties
    with
        static member Make (item : GeometryItem) : Geometry =
            Geometry (item, None)

        member this.Normalize() =
            let (Geometry (item, properties)) = this
            Geometry (item.Normalize(), properties)

    type GeometryCollection = GeometryCollection of Geometry:(Geometry list) * Properties:Properties
    with
        static member Make (item : GeometryItem) : GeometryCollection =
            let item = Geometry.Make item
            GeometryCollection ([item], None)

        static member Make (item : GeometryItem list) : GeometryCollection =
            let item = item |> List.map Geometry.Make
            GeometryCollection (item, None)

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
