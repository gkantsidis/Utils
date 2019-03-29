namespace CGFSHelper.Spatial

(*
 * Dependencies (check also the open statements):
 * - Collections.fs
 *
 *)

/// <summary>
/// Set of types and helper functions to deal with the geographical coordinates
/// </summary>
module GeoLocation =
    open System
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

    open CGFSHelper.Collections

    // We provide two implementation based on the desired arithmetic type.
    // The semantics are slightly different for the floating implementation.
    // We use type aliases to decide which implementation to expose as default.

    /// <summary>
    /// Unit of measure to describe radians.
    /// A Radian describes the plane angle subtended by a circular arc as the length of the arc divided by the radius of the arc
    /// (Source: wikipedia).
    /// </summary>
    [<Measure>] type rad

    /// <summary>
    /// Unit of measure to describe degrees of arc.
    /// A degree of arc is a measurement of a plane angle, designed so that a full rotation is 360 degrees
    /// (Source: wikipedia).
    /// </summary>
    [<Measure>] type degree

    /// The radius of the earth in meters
    let earth_radius_float =  6371000.0

    (*
     * Implementation nodes
     * ====================
     *
     * Distances are computed using the formulas in https://en.wikipedia.org/wiki/Haversine_formula
     *)

    let private sin = Math.Sin
    let private cos = Math.Cos
    let private sqrt = Math.Sqrt
    let private asin = Math.Asin
    let private acos = Math.Acos
    let private atan2 = Math.Atan2

    module DecimalImplementation =
        type Radian     = decimal<rad>
        type Degree     = decimal<degree>
        type Distance   = decimal<meter>

        let earth_radius : Distance = decimal earth_radius_float |> LanguagePrimitives.DecimalWithMeasure
        let ZeroDistance : Distance = 0.0m<meter>

        let convert_arc_degree_to_radian (v : Degree) : Radian =
            let naked = decimal v
            let radians_multiplier = Math.PI / 180.0 |> decimal
            let radians = naked * radians_multiplier
            LanguagePrimitives.DecimalWithMeasure radians

        let convert_arc_radian_to_degree (v : Radian) : Degree =
            let naked = decimal v
            let degrees_multiplier = 180.0 / Math.PI |> decimal
            let degrees = naked * degrees_multiplier
            LanguagePrimitives.DecimalWithMeasure degrees

        let convert_float_arc_degree_to_radian (v : float<degree>) : Radian =
            let naked = decimal v
            let radians_multiplier = Math.PI / 180.0 |> decimal
            let radians = naked * radians_multiplier
            LanguagePrimitives.DecimalWithMeasure radians

        let convert_float_arc_radian_to_degree (v : float<rad>) : Degree =
            let naked = decimal v
            let degrees_multiplier = 180.0 / Math.PI |> decimal
            let degrees = naked * degrees_multiplier
            LanguagePrimitives.DecimalWithMeasure degrees

        let convert_float_arc_radian (v : float<rad>)       : Radian = decimal v |> LanguagePrimitives.DecimalWithMeasure
        let convert_float_arc_degree (v : float<degree>)    : Degree = decimal v |> LanguagePrimitives.DecimalWithMeasure
        let convert_arc_radian_to_float (v : Radian)        : float  = decimal v |> float
        let convert_arc_degree_to_float (v : Degree)        : float  = decimal v |> float

        let haversine (angle : Radian) =
            let degree = decimal angle |> float
            0.5 * (1.0 - Math.Cos degree) |> decimal

        let AreEqualDistance (distance1 : Distance) (distance2 : Distance) = (distance1 = distance2)

        type Make =
            static member ArcDegree (angle : float)     : Degree    = decimal angle |> LanguagePrimitives.DecimalWithMeasure
            static member ArcDegree (angle : decimal)   : Degree    = angle |> LanguagePrimitives.DecimalWithMeasure
            static member ArcRadian (angle : float)     : Radian    = decimal angle |> LanguagePrimitives.DecimalWithMeasure
            static member ArcRadian (angle : decimal)   : Radian    = angle |> LanguagePrimitives.DecimalWithMeasure

            static member Distance  (distance : float) : Distance =
                decimal distance |> LanguagePrimitives.DecimalWithMeasure
            static member Distance  (distance : decimal) : Distance =
                distance |> LanguagePrimitives.DecimalWithMeasure
            static member Distance  (distance : float option) : Distance option =
                distance |> Option.map Make.Distance
            static member Distance  (distance : decimal option) : Distance option =
                distance |> Option.map Make.Distance
            static member Distance (distance : Nullable<float>) : Nullable<Distance> =
                distance |> Nullable.map Make.Distance
            static member Distance (distance : Nullable<decimal>) : Nullable<Distance> =
                distance |> Nullable.map Make.Distance

        /// <summary>
        /// Represents a point in the coordinate space.
        /// The point is defined by its latitude (north-south),
        /// longitude (east-west), and altitude (distance from surface).
        /// </summary>
        /// <remarks>
        /// Currently, there is very limited support for points that are not at the surface of the earth.
        /// </remarks>
        [<CustomEquality>]
        [<CustomComparison>]
        [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
        type Coordinates =
            struct
                /// The latitude of the point (north-south)
                val Latitude    : Radian
                /// The longitude of the point (east-west)
                val Longitude   : Radian
                /// Distance from the surface of the earth
                val Altitude    : Distance

                new (lat : Radian, lon : Radian) = { Latitude = lat; Longitude = lon; Altitude = 0.0m<meter> }
                new (lat : Radian, lon : Radian, altitude : Distance) = { Latitude = lat; Longitude = lon; Altitude = altitude }
                new (lat : Radian, lon : Radian, altitude : Nullable<Distance>) =
                    if altitude.HasValue then
                        { Latitude = lat; Longitude = lon; Altitude = altitude.Value }
                    else
                        { Latitude = lat; Longitude = lon; Altitude = 0.0m<meter> }

                new (lat : Radian, lon : Radian, altitude : Distance option) =
                    { Latitude = lat; Longitude = lon; Altitude = defaultArg altitude 0.0m<meter> }

                new (lat : float<rad>, lon : float<rad>) =
                    { Latitude = convert_float_arc_radian lat; Longitude = convert_float_arc_radian lon; Altitude = 0.0m<meter> }

                new (lat : float<rad>, lon : float<rad>, altitude : float<meter>) =
                    { Latitude = convert_float_arc_radian lat; Longitude = convert_float_arc_radian lon; Altitude = altitude |> decimal |> Make.Distance }

                new (lat : float<rad>, lon : float<rad>, altitude : float<meter> option) =
                    match altitude with
                    | None          -> { Latitude = convert_float_arc_radian lat; Longitude = convert_float_arc_radian lon; Altitude = 0.0m<meter> }
                    | Some altitude ->
                        { Latitude = convert_float_arc_radian lat; Longitude = convert_float_arc_radian lon; Altitude = altitude |> decimal |> Make.Distance }

                new (lat : float<rad>, lon : float<rad>, altitude : Nullable<float<meter>>) =
                    if altitude.HasValue then
                        { Latitude = convert_float_arc_radian lat; Longitude = convert_float_arc_radian lon; Altitude = altitude.Value |> decimal |> Make.Distance }
                    else
                        { Latitude = convert_float_arc_radian lat; Longitude = convert_float_arc_radian lon; Altitude = 0.0m<meter> }

                static member MakeFromArcDegree(lat : Degree, lon : Degree) =
                    Coordinates(convert_arc_degree_to_radian lat, convert_arc_degree_to_radian lon)
                static member MakeFromArcDegree(lat : Degree, lon : Degree, altitude : Distance) =
                    Coordinates(convert_arc_degree_to_radian lat, convert_arc_degree_to_radian lon, altitude)
                static member MakeFromArcDegree(lat : Degree, lon : Degree, altitude : Nullable<Distance>) =
                    Coordinates(convert_arc_degree_to_radian lat, convert_arc_degree_to_radian lon, altitude)

                static member MakeFromArcDegreeNaked(lat : float, lon : float) =
                    Coordinates(lat |> Make.ArcDegree |> convert_arc_degree_to_radian, lon |> Make.ArcDegree |> convert_arc_degree_to_radian)
                static member MakeFromArcDegreeNaked(lat : float, lon : float, altitude : float) =
                    Coordinates(lat |> Make.ArcDegree |> convert_arc_degree_to_radian, lon |> Make.ArcDegree |> convert_arc_degree_to_radian, Make.Distance altitude)
                static member MakeFromArcDegreeNaked(lat : float, lon : float, altitude : Nullable<float>) =
                    let altitude = altitude |> Nullable.map Make.Distance
                    Coordinates(lat |> Make.ArcDegree |> convert_arc_degree_to_radian, lon |> Make.ArcDegree |> convert_arc_degree_to_radian, altitude)
                static member MakeFromArcDegreeNaked(lat : float, lon : float, altitude : float option) =
                    match altitude with
                    | None -> Coordinates(lat |> Make.ArcDegree |> convert_arc_degree_to_radian, lon |> Make.ArcDegree |> convert_arc_degree_to_radian)
                    | Some altitude -> Coordinates(lat |> Make.ArcDegree |> convert_arc_degree_to_radian, lon |> Make.ArcDegree |> convert_arc_degree_to_radian, Make.Distance altitude)

                static member MakeFromArcRadianNaked(lat : float, lon : float) =
                    Coordinates(lat |> Make.ArcRadian, lon |> Make.ArcRadian)
                static member MakeFromArcRadianNaked(lat : float, lon : float, altitude : float) =
                    Coordinates(lat |> Make.ArcRadian, lon |> Make.ArcRadian, Make.Distance altitude)
                static member MakeFromArcRadianNaked(lat : float, lon : float, altitude : Nullable<float>) =
                    let altitude = altitude |> Nullable.map Make.Distance
                    Coordinates(lat |> Make.ArcRadian, lon |> Make.ArcRadian, altitude)
                static member MakeFromArcRadianNaked(lat : float, lon : float, altitude : float option) =
                    match altitude with
                    | None -> Coordinates(lat |> Make.ArcRadian, lon |> Make.ArcRadian)
                    | Some altitude -> Coordinates(lat |> Make.ArcRadian, lon |> Make.ArcRadian, Make.Distance altitude)

                static member CompareLatitude(left : Coordinates, right : Coordinates) =
                    (decimal left.Latitude).CompareTo(right.Latitude)

                static member CompareLongitude(left : Coordinates, right : Coordinates) =
                    (decimal left.Longitude).CompareTo(right.Longitude)

                static member CompareAltitude(left : Coordinates, right : Coordinates) =
                    (decimal left.Altitude).CompareTo(right.Altitude)

                static member CompareDistanceToCenter(left : Coordinates, right : Coordinates) =
                    (decimal left.DistanceToCenter).CompareTo(right.DistanceToCenter)

                member this.Lat = this.Latitude
                member this.Lon = this.Longitude
                member this.LatDegrees = convert_arc_radian_to_degree this.Latitude
                member this.LonDegrees = convert_arc_radian_to_degree this.Longitude

                /// <summary>
                /// Computes the distance between two points on the surface of the earh
                /// (i.e. altitude is ignored).
                /// </summary>
                /// <param name="other">The position of the second point</param>
                member this.SurfaceDistanceTo(other : Coordinates) : Distance =
                    let latitude = this.Latitude - other.Latitude
                    let longitude = this.Longitude - other.Longitude
                    let hlat = haversine latitude |> float
                    let hlon = haversine longitude |> float
                    let lat2 = convert_arc_radian_to_float this.Latitude |> Math.Cos
                    let rhs = hlat + lat2 * lat2  * hlon
                    2.0 * (earth_radius_float) * Math.Asin(Math.Sqrt(rhs))
                    |> decimal |> LanguagePrimitives.DecimalWithMeasure

                /// <summary>
                /// Computes the distance between two points
                /// </summary>
                /// <param name="other">The position of the second point</param>
                member this.DistanceTo(other : Coordinates) : Distance =
                    if this.Altitude = ZeroDistance && other.Altitude = ZeroDistance then
                        this.SurfaceDistanceTo(other)
                    else
                        // TODO: Implement distance computation for points above the surface
                        raise (NotImplementedException("Distances between points not on the surface of the earth not implemented yet"))

                /// <summary>
                /// Compute the distance to the center of the earth
                /// </summary>
                member this.DistanceToCenter
                        with get () : Distance =
                            let hlat = haversine this.Latitude |> float
                            let hlon = haversine this.Longitude |> float
                            let rhs = hlat + Math.Cos(convert_arc_radian_to_float this.Latitude) * hlon
                            let surface : Distance =
                                2.0 * earth_radius_float * Math.Asin(Math.Sqrt(rhs))
                                |> decimal |> LanguagePrimitives.DecimalWithMeasure
                            // TODO: Check that the following is indeed correct
                            surface + this.Altitude

                /// <summary>
                /// Reduces the numerical precision of the coordinates.
                /// </summary>
                /// <param name="precision"></param>
                member this.Round (?precision : int) : Coordinates =
                    let precision = defaultArg precision 8
                    let latitude : Radian = Decimal.Round(decimal this.Latitude, precision) |> LanguagePrimitives.DecimalWithMeasure
                    let longitude : Radian = Decimal.Round(decimal this.Longitude, precision) |> LanguagePrimitives.DecimalWithMeasure
                    let altitude : Distance = Decimal.Round(decimal this.Altitude, precision) |> LanguagePrimitives.DecimalWithMeasure
                    Coordinates(latitude, longitude, altitude)

                member this.StructuredFormatDisplay =
                    let ns = if this.Latitude >= 0.0m<rad> then 'N' else 'S'
                    let ew = if this.Longitude >= 0.0m<rad> then 'E' else 'W'

                    if this.Altitude = ZeroDistance then
                        Printf.sprintf "(%fº%c,%fº%c)"
                            (convert_arc_radian_to_degree this.Latitude) ns
                            (convert_arc_radian_to_degree this.Longitude) ew
                    else
                        Printf.sprintf "(%fº%c,%fº%c,%f)"
                            (convert_arc_radian_to_degree this.Latitude) ns
                            (convert_arc_radian_to_degree this.Longitude) ew
                            this.Altitude

                override this.ToString() = this.StructuredFormatDisplay

                override this.Equals(other : obj) =
                    match other with
                    | :? Coordinates as other ->
                        let this = this :> IEquatable<Coordinates>
                        this.Equals(other)
                    | _ -> false

                override this.GetHashCode() =
                    let distance = this.DistanceToCenter |> decimal
                    let d = Math.Round(distance / 1000.0m)
                    int d

                // This implementation is not really needed, but we have it to keep the symmetry with the
                // implementation of the floating point version.
                interface IEquatable<Coordinates> with
                    member this.Equals(other : Coordinates) =
                        this.Latitude = other.Latitude && this.Longitude = other.Longitude && this.Altitude = other.Altitude

                // We provide a custom comparison function that orders points first by the distance to center of the earth,
                // then by latitude, then longitude, and then altitude. This is rather arbitrary ordering, but it may be
                // useful in some scenarios.
                interface IComparable<Coordinates> with
                    member this.CompareTo(other : Coordinates) =
                        let comparators = [
                            Coordinates.CompareDistanceToCenter
                            Coordinates.CompareLatitude
                            Coordinates.CompareLongitude
                            Coordinates.CompareAltitude
                        ]
                        Comparison.compareMany comparators (this, other)
            end

        /// <summary>
        /// Computes an intermediate point between two points which is at the specified fraction
        /// (of the distance between the two endpoints) from the first endpoint
        /// </summary>
        /// <param name="point1">The first endpoint</param>
        /// <param name="point2">The second endpoint</param>
        /// <param name="fraction">The fraction of the total distance that the new point will be from the first endpoint</param>
        let FindIntermediatePoint (point1: Coordinates, point2: Coordinates) (fraction : decimal) =
            // Computation from http://www.edwilliams.org/avform.htm#Intermediate

            let lat1 = decimal point1.Lat |> float
            let lat2 = decimal point2.Lat |> float
            let lon1 = decimal point1.Lon |> float
            let lon2 = decimal point2.Lon |> float
            let f = fraction |> float

            //let d = sin(lat1) * sin(lat2) +
            //        cos(lat1) * cos(lat2) * cos(lon1 - lon2)
            //        |> acos
            // Better formula for short distances:
            let d = 2.0 * asin(sqrt((sin((lat1-lat2)/2.0))**2.0 + cos(lat1) * cos(lat2) * (sin((lon1-lon2)/2.0))**2.0))

            let A = sin((1.0-f)*d)/sin(d)
            let B = sin(f*d)/sin(d)
            let x = A*cos(lat1)*cos(lon1) + B*cos(lat2)*cos(lon2)
            let y = A*cos(lat1)*sin(lon1) + B*cos(lat2)*sin(lon2)
            let z = A*sin(lat1) + B * sin(lat2)
            let lat = atan2(z, sqrt(x**2.0 + y**2.0))
            let lon = atan2(y, x)

            // printfn "Finding: %A - %A : %f" point1 point2 fraction
            Coordinates.MakeFromArcRadianNaked(lat, lon)


    module FloatImplementation =
        let private error : float<meter> = LanguagePrimitives.FloatWithMeasure 0.001

        type Radian     = float<rad>
        type Degree     = float<degree>
        type Distance   = float<meter>

        let earth_radius : Distance = earth_radius_float |> LanguagePrimitives.FloatWithMeasure
        let ZeroDistance : Distance = 0.0<meter>

        let convert_arc_degree_to_radian (v : Degree) : Radian =
            let naked = float v
            let radians_multiplier = Math.PI / 180.0
            let radians = naked * radians_multiplier
            LanguagePrimitives.FloatWithMeasure radians

        let convert_arc_radian_to_degree (v : Radian) : Degree =
            let naked = float v
            let degrees_multiplier = 180.0 / Math.PI
            let degrees = naked * degrees_multiplier
            LanguagePrimitives.FloatWithMeasure degrees

        let convert_decimal_arc_degree_to_radian (v : decimal<degree>) : Radian =
            let naked = decimal v |> float
            let radians_multiplier = Math.PI / 180.0
            let radians = naked * radians_multiplier
            LanguagePrimitives.FloatWithMeasure radians

        let convert_decimal_arc_radian_to_degree (v : decimal<rad>) : Degree =
            let naked = decimal v |> float
            let degrees_multiplier = 180.0 / Math.PI
            let degrees = naked * degrees_multiplier
            LanguagePrimitives.FloatWithMeasure degrees

        let convert_decimal_arc_degree (v : decimal<degree>) : Degree =
            v |> decimal |> float |> LanguagePrimitives.FloatWithMeasure

        let convert_decimal_arc_radian (v : decimal<rad>) : Radian =
            v |> decimal |> float |> LanguagePrimitives.FloatWithMeasure 

        let convert_arc_radian_to_float (v : Radian)        : float  = v |> float
        let convert_arc_degree_to_float (v : Degree)        : float  = v |> float

        let haversine (angle : Radian) =
            0.5 * (1.0 - Math.Cos (float angle)) |> decimal

        let AreEqualDistance (distance1 : Distance) (distance2 : Distance) =
            Math.Abs(float distance1 - float distance2) < (float error)

        type Make =
            static member ArcDegree (angle : float)     : Degree    = angle |> LanguagePrimitives.FloatWithMeasure
            static member ArcDegree (angle : decimal)   : Degree    = float angle |> LanguagePrimitives.FloatWithMeasure
            static member ArcRadian (angle : float)     : Radian    = angle |> LanguagePrimitives.FloatWithMeasure
            static member ArcRadian (angle : decimal)   : Radian    = float angle |> LanguagePrimitives.FloatWithMeasure

            static member Distance  (distance : float) : Distance = distance |> LanguagePrimitives.FloatWithMeasure
            static member Distance  (distance : decimal) : Distance =
                float distance |> LanguagePrimitives.FloatWithMeasure
            static member Distance  (distance : float option) : Distance option =
                distance |> Option.map Make.Distance
            static member Distance  (distance : decimal option) : Distance option =
                distance |> Option.map Make.Distance
            static member Distance (distance : Nullable<float>) : Nullable<Distance> =
                distance |> Nullable.map Make.Distance
            static member Distance (distance : Nullable<decimal>) : Nullable<Distance> =
                distance |> Nullable.map Make.Distance

        /// <summary>
        /// Represents a point in the coordinate space.
        /// The point is defined by its latitude (north-south),
        /// longitude (east-west), and altitude (distance from surface).
        /// </summary>
        /// <remarks>
        /// Currently, there is very limited support for points that are not at the surface of the earth.
        /// </remarks>
        [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
        type Coordinates =
            struct
                /// The latitude of the point (north-south)
                val Latitude    : Radian
                /// The longitude of the point (east-west)
                val Longitude   : Radian
                /// Distance from the surface of the earth
                val Altitude    : Distance

                new (lat : Radian, lon : Radian) = { Latitude = lat; Longitude = lon; Altitude = 0.0<meter> }
                new (lat : Radian, lon : Radian, altitude : Distance) = { Latitude = lat; Longitude = lon; Altitude = altitude }
                new (lat : Radian, lon : Radian, altitude : Nullable<Distance>) =
                    if altitude.HasValue then
                        { Latitude = lat; Longitude = lon; Altitude = altitude.Value }
                    else
                        { Latitude = lat; Longitude = lon; Altitude = 0.0<meter> }

                new (lat : Radian, lon : Radian, altitude : Distance option) =
                    { Latitude = lat; Longitude = lon; Altitude = defaultArg altitude 0.0<meter> }

                new (lat : decimal<rad>, lon : decimal<rad>) =
                    { Latitude = convert_decimal_arc_radian lat; Longitude = convert_decimal_arc_radian lon; Altitude = 0.0<meter> }

                static member MakeFromArcDegree(lat : Degree, lon : Degree) =
                    Coordinates(convert_arc_degree_to_radian lat, convert_arc_degree_to_radian lon)
                static member MakeFromArcDegree(lat : Degree, lon : Degree, altitude : Distance) =
                    Coordinates(convert_arc_degree_to_radian lat, convert_arc_degree_to_radian lon, altitude)
                static member MakeFromArcDegree(lat : Degree, lon : Degree, altitude : Nullable<Distance>) =
                    Coordinates(convert_arc_degree_to_radian lat, convert_arc_degree_to_radian lon, altitude)

                static member MakeFromArcDegreeNaked(lat : float, lon : float) =
                    Coordinates(lat |> Make.ArcDegree |> convert_arc_degree_to_radian, lon |> Make.ArcDegree |> convert_arc_degree_to_radian)
                static member MakeFromArcDegreeNaked(lat : float, lon : float, altitude : float) =
                    Coordinates(lat |> Make.ArcDegree |> convert_arc_degree_to_radian, lon |> Make.ArcDegree |> convert_arc_degree_to_radian, Make.Distance altitude)
                static member MakeFromArcDegreeNaked(lat : float, lon : float, altitude : Nullable<float>) =
                    let altitude = altitude |> Nullable.map Make.Distance
                    Coordinates(lat |> Make.ArcDegree |> convert_arc_degree_to_radian, lon |> Make.ArcDegree |> convert_arc_degree_to_radian, altitude)
                static member MakeFromArcDegreeNaked(lat : float, lon : float, altitude : float option) =
                    match altitude with
                    | None -> Coordinates(lat |> Make.ArcDegree |> convert_arc_degree_to_radian, lon |> Make.ArcDegree |> convert_arc_degree_to_radian)
                    | Some altitude -> Coordinates(lat |> Make.ArcDegree |> convert_arc_degree_to_radian, lon |> Make.ArcDegree |> convert_arc_degree_to_radian, Make.Distance altitude)

                static member MakeFromArcRadianNaked(lat : float, lon : float) =
                    Coordinates(lat |> Make.ArcRadian, lon |> Make.ArcRadian)
                static member MakeFromArcRadianNaked(lat : float, lon : float, altitude : float) =
                    Coordinates(lat |> Make.ArcRadian, lon |> Make.ArcRadian, Make.Distance altitude)
                static member MakeFromArcRadianNaked(lat : float, lon : float, altitude : Nullable<float>) =
                    let altitude = altitude |> Nullable.map Make.Distance
                    Coordinates(lat |> Make.ArcRadian, lon |> Make.ArcRadian, altitude)
                static member MakeFromArcRadianNaked(lat : float, lon : float, altitude : float option) =
                    match altitude with
                    | None -> Coordinates(lat |> Make.ArcRadian, lon |> Make.ArcRadian)
                    | Some altitude -> Coordinates(lat |> Make.ArcRadian, lon |> Make.ArcRadian, Make.Distance altitude)

                static member CompareLatitude(left : Coordinates, right : Coordinates) =
                    (decimal left.Latitude).CompareTo(right.Latitude)

                static member CompareLongitude(left : Coordinates, right : Coordinates) =
                    (decimal left.Longitude).CompareTo(right.Longitude)

                static member CompareAltitude(left : Coordinates, right : Coordinates) =
                    (decimal left.Altitude).CompareTo(right.Altitude)

                static member CompareDistanceToCenter(left : Coordinates, right : Coordinates) =
                    (decimal left.DistanceToCenter).CompareTo(right.DistanceToCenter)

                member this.Lat = this.Latitude
                member this.Lon = this.Longitude
                member this.LatDegrees = convert_arc_radian_to_degree this.Latitude
                member this.LonDegrees = convert_arc_radian_to_degree this.Longitude

                /// <summary>
                /// Computes the distance between two points on the surface of the earh
                /// (i.e. altitude is ignored).
                /// </summary>
                /// <param name="other">The position of the second point</param>
                member this.SurfaceDistanceTo(other : Coordinates) : Distance =
                    let latitude = this.Latitude - other.Latitude
                    let longitude = this.Longitude - other.Longitude
                    let hlat = haversine latitude |> float
                    let hlon = haversine longitude |> float
                    let lat2 = convert_arc_radian_to_float this.Latitude |> Math.Cos
                    let rhs = hlat + lat2 * lat2  * hlon
                    2.0 * (earth_radius_float) * Math.Asin(Math.Sqrt(rhs))
                    |> LanguagePrimitives.FloatWithMeasure

                /// <summary>
                /// Computes the distance between two points
                /// </summary>
                /// <param name="other">The position of the second point</param>
                member this.DistanceTo(other : Coordinates) : Distance =
                    if this.Altitude = ZeroDistance && other.Altitude = ZeroDistance then
                        this.SurfaceDistanceTo(other)
                    else
                        // TODO: Implement distance computation for points above the surface
                        raise (NotImplementedException("Distances between points not on the surface of the earth not implemented yet"))

                /// <summary>
                /// Compute the distance to the center of the earth
                /// </summary>
                member this.DistanceToCenter
                        with get () : Distance =
                            let hlat = haversine this.Latitude |> float
                            let hlon = haversine this.Longitude |> float
                            let rhs = hlat + Math.Cos(convert_arc_radian_to_float this.Latitude) * hlon
                            let surface : Distance =
                                2.0 * earth_radius_float * Math.Asin(Math.Sqrt(rhs))
                                |> LanguagePrimitives.FloatWithMeasure
                            // TODO: Check that the following is indeed correct
                            surface + this.Altitude

                /// <summary>
                /// Reduces the numerical precision of the coordinates.
                /// </summary>
                /// <param name="precision"></param>
                member this.Round (?precision : int) : Coordinates =
                    let precision = defaultArg precision 8
                    let latitude : Radian = Math.Round(float this.Latitude, precision) |> LanguagePrimitives.FloatWithMeasure
                    let longitude : Radian = Math.Round(float this.Longitude, precision) |> LanguagePrimitives.FloatWithMeasure
                    let altitude : Distance = Math.Round(float this.Altitude, precision) |> LanguagePrimitives.FloatWithMeasure
                    Coordinates(latitude, longitude, altitude)

                member this.StructuredFormatDisplay =
                    let ns = if this.Latitude >= 0.0<rad> then 'N' else 'S'
                    let ew = if this.Longitude >= 0.0<rad> then 'E' else 'W'

                    if this.Altitude = ZeroDistance then
                        Printf.sprintf "(%fº%c,%fº%c)"
                            (convert_arc_radian_to_degree this.Latitude) ns
                            (convert_arc_radian_to_degree this.Longitude) ew
                    else
                        Printf.sprintf "(%fº%c,%fº%c,%f)"
                            (convert_arc_radian_to_degree this.Latitude) ns
                            (convert_arc_radian_to_degree this.Longitude) ew
                            this.Altitude

                override this.ToString() = this.StructuredFormatDisplay

                /// Checks whether two elements are very close to each other,
                /// so that it will be safe to assume they are the same.
                member this.IsApproximateEqual (other : Coordinates) = this.DistanceTo other < error
            end

        /// <summary>
        /// Computes an intermediate point between two points which is at the specified fraction
        /// (of the distance between the two endpoints) from the first endpoint
        /// </summary>
        /// <param name="point1">The first endpoint</param>
        /// <param name="point2">The second endpoint</param>
        /// <param name="fraction">The fraction of the total distance that the new point will be from the first endpoint</param>
        let FindIntermediatePoint (point1: Coordinates, point2: Coordinates) (fraction : float) =
            // Computation from http://www.edwilliams.org/avform.htm#Intermediate

            let lat1 = point1.Lat |> float
            let lat2 = point2.Lat |> float
            let lon1 = point1.Lon |> float
            let lon2 = point2.Lon |> float
            let f = fraction

            //let d = sin(lat1) * sin(lat2) +
            //        cos(lat1) * cos(lat2) * cos(lon1 - lon2)
            //        |> acos
            // Better formula for short distances:
            let d = 2.0 * asin(sqrt((sin((lat1-lat2)/2.0))**2.0 + cos(lat1) * cos(lat2) * (sin((lon1-lon2)/2.0))**2.0))

            let A = sin((1.0-f)*d)/sin(d)
            let B = sin(f*d)/sin(d)
            let x = A*cos(lat1)*cos(lon1) + B*cos(lat2)*cos(lon2)
            let y = A*cos(lat1)*sin(lon1) + B*cos(lat2)*sin(lon2)
            let z = A*sin(lat1) + B * sin(lat2)
            let lat = atan2(z, sqrt(x**2.0 + y**2.0))
            let lon = atan2(y, x)

            // printfn "Finding: %A - %A : %f" point1 point2 fraction
            Coordinates.MakeFromArcRadianNaked(lat, lon)

    /// <summary>
    /// Base type used for arithmetic
    /// </summary>
    type BaseUnit =
#if GEOLOCATION_WITH_FLOATING_IMPLEMENTATION
            float
#else
            decimal
#endif

    /// <summary>
    /// The number represents angle measured in rad.
    /// </summary>
    type Radian =
#if GEOLOCATION_WITH_FLOATING_IMPLEMENTATION
            FloatImplementation.Radian
#else
            DecimalImplementation.Radian
#endif

    /// <summary>
    /// The number represents angle measured in arc degrees.
    /// </summary>
    type Degree =
#if GEOLOCATION_WITH_FLOATING_IMPLEMENTATION
            FloatImplementation.Degree
#else
            DecimalImplementation.Degree
#endif

    /// <summary>
    /// The number represents length measured in meters
    /// </summary>
    type Distance =
#if GEOLOCATION_WITH_FLOATING_IMPLEMENTATION
            FloatImplementation.Distance
#else
            DecimalImplementation.Distance
#endif

    /// <summary>
    /// Represents a distance of value 0 meters.
    /// </summary>
    let ZeroDistance =
#if GEOLOCATION_WITH_FLOATING_IMPLEMENTATION
            FloatImplementation.ZeroDistance
#else
            DecimalImplementation.ZeroDistance
#endif

    type Mk =
#if GEOLOCATION_WITH_FLOATING_IMPLEMENTATION
        static member Distance (distance : float)   = FloatImplementation.Make.Distance distance
        static member Distance (distance : decimal) = FloatImplementation.Make.Distance distance
        static member Distance (distance : float option) = FloatImplementation.Make.Distance distance
        static member Distance (distance : decimal option) = FloatImplementation.Make.Distance distance
        static member Distance (distance : Nullable<float>) = FloatImplementation.Make.Distance distance
        static member Distance (distance : Nullable<decimal>) = FloatImplementation.Make.Distance distance

        static member Radian (angle : float) = FloatImplementation.Make.ArcRadian angle
        static member Radian (angle : decimal) = FloatImplementation.Make.ArcRadian angle

        static member Degree (angle : float) = FloatImplementation.Make.ArcDegree angle
        static member Degree (angle : decimal) = FloatImplementation.Make.ArcDegree angle
        static member Degree (angle : int) = FloatImplementation.Make.ArcDegree (decimal angle)

        static member FloatFromDistance (distance : Distance) = float distance
        static member FloatFromDegree (angle : Degree) = float angle
        static member FloatFromRadian (rad : Radian) = float rad
        static member DecimalFromDistance (distance : Distance) = float distance |> decimal
        static member DecimalFromDegree (angle : Degree) = float angle |> decimal
        static member DecimalFromRadian (rad : Radian) = float radian |> decimal

        static member Convert (v : float) = v
        static member Convert (v : decimal) = float v
        static member Convert (v : int) = float v
#else
        static member Distance (distance : float)   = DecimalImplementation.Make.Distance distance
        static member Distance (distance : decimal) = DecimalImplementation.Make.Distance distance
        static member Distance (distance : float option) = DecimalImplementation.Make.Distance distance
        static member Distance (distance : decimal option) = DecimalImplementation.Make.Distance distance
        static member Distance (distance : Nullable<float>) = DecimalImplementation.Make.Distance distance
        static member Distance (distance : Nullable<decimal>) = DecimalImplementation.Make.Distance distance

        static member Radian (angle : float) = DecimalImplementation.Make.ArcRadian angle
        static member Radian (angle : decimal) = DecimalImplementation.Make.ArcRadian angle

        static member Degree (angle : float) = DecimalImplementation.Make.ArcDegree angle
        static member Degree (angle : decimal) = DecimalImplementation.Make.ArcDegree angle
        static member Degree (angle : int) = DecimalImplementation.Make.ArcDegree (decimal angle)

        static member FloatFromDistance (distance : Distance) = decimal distance |> float
        static member FloatFromDegree (angle : Degree) = decimal angle |> float
        static member FloatFromRadian (rad : Radian) = decimal rad |> float
        static member DecimalFromDistance (distance : Distance) = decimal distance
        static member DecimalFromDegree (angle : Degree) = decimal angle
        static member DecimalFromRadian (rad : Radian) = decimal rad

        static member Convert (v : float) = decimal v
        static member Convert (v : decimal) = v
        static member Convert (v : int) = decimal v
#endif

    /// <summary>
    /// Conversion functions
    /// </summary>
    type Convert =
        /// Convert angles from degrees to rads
        static member ArcDegreeToRad (v : FloatImplementation.Degree)   : FloatImplementation.Radian    = FloatImplementation.convert_arc_degree_to_radian v
        /// Convert angles from degrees to rads
        static member ArcDegreeToRad (v : DecimalImplementation.Degree) : DecimalImplementation.Radian  = DecimalImplementation.convert_arc_degree_to_radian v
        /// Convert angles from rads to degrees
        static member ArcRadToDegree (v : FloatImplementation.Radian)   : FloatImplementation.Degree    = FloatImplementation.convert_arc_radian_to_degree v
        /// Convert angles from rads to degrees
        static member ArcRadToDegree (v : DecimalImplementation.Radian) : DecimalImplementation.Degree  = DecimalImplementation.convert_arc_radian_to_degree v

    /// <summary>
    /// Common mathematical functions
    /// </summary>
    type Math =
        /// Returns the haversine of the specified angle (given in radians)
        static member Haversine (angle : DecimalImplementation.Radian)  = DecimalImplementation.haversine angle

        /// Returns the haversine of the specified angle (given in radians)
        static member Haversine (angle : FloatImplementation.Radian)    = FloatImplementation.haversine angle

        /// Returns the haversine of the specified angle (given in arc degrees)
        static member HaversineFromDegree (angle : DecimalImplementation.Degree) =
            let angle = DecimalImplementation.convert_arc_degree_to_radian angle
            DecimalImplementation.haversine angle

        /// Returns the haversine of the specified angle (given in arc degrees)
        static member HaversineFromDegree (angle : FloatImplementation.Degree) =
            let angle = FloatImplementation.convert_arc_degree_to_radian angle
            FloatImplementation.haversine angle

    type Coordinates =
#if GEOLOCATION_WITH_FLOATING_IMPLEMENTATION
        FloatImplementation.Coordinates
#else
        DecimalImplementation.Coordinates
#endif

    let AreEqualDistance =
#if GEOLOCATION_WITH_FLOATING_IMPLEMENTATION
        FloatImplementation.AreEqualDistance
#else
        DecimalImplementation.AreEqualDistance
#endif


    /// <summary>
    /// Represents an edge between two points in the coordinates space
    /// </summary>
    type Edge = Coordinates * Coordinates

    /// Helper functions that operate on edges
    module Edges =
        /// <summary>
        /// Checks whether a list of edges is connected,
        /// i.e. the endpoint of one edge is the head of the next.
        /// </summary>
        /// <param name="line">The list of edges</param>
        let IsConnected (line : Edge list) =
            let rec check (current : Edge option) (work : Edge list) =
                match current, work with
                | _, []             -> true
                | None, hd :: tl    -> check (Some hd) tl
                | Some (_, finish), ((start, _) as hd) :: tl ->
                    if finish.Equals(start) then check (Some hd) tl
                    else false

            check None line

        /// <summary>
        /// Compute the total length of a list of edges.
        /// </summary>
        /// <param name="edges"></param>
        let TotalLength (edges : Edge list) = edges |> List.sumBy (fun (x, y) -> x.DistanceTo y)
