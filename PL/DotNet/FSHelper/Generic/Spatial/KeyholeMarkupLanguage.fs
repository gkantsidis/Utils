namespace CGFSHelper.Spatial

(*
 * Requirements:
 * - Collections.fs
 * - GeoLocation.fs
 *)
module KeyholeMarkupLanguage =
    open CGFSHelper.Spatial.GeoLocation

    (* References:
     * - http://www.opengeospatial.org/standards/kml/
     * - https://developers.google.com/kml/documentation/kmlreference
     *)

    type Id = string option

    type AltitudeMode =
    | ClampToGround
    | RelativeToGround
    | Absolute
    | RelativeToSeaFloor
    | ClampToSeaFloor

    type RefreshMode =
    | OnChange
    | OnInterval
    | OnExpire

    type ViewRefreshMode =
    | Never
    | OnStop
    | OnRequest
    | OnRegion

    type Orientation = Orientation of Heading:Degree * Tilt:Degree * Roll:Degree
    type Scale = Scale of X:float * Y:float * Z:float

    type Link
    type Alias
    type ResourceMap = Alias list

    type Point = Point of Id:Id * Extrude:bool * AltitudeMode:AltitudeMode * Coordinates:Coordinates
    type LineString = LineString of Id:Id * Extrude:bool * Tesselate:bool * AltitudeMode:AltitudeMode * DrawOrder:int * Coordinates:(Coordinates list)
    type LinearRing = LinearRing of Id:Id * Extrude:bool * Tesselate:bool * AltitudeMode:AltitudeMode * Coordinates:(Coordinates list)
    
    type Polygon = Polygon of Id:Id * Extrude:bool * Tesselate:bool * AltitudeMode:AltitudeMode * OuterBoundaryIs:LinearRing * InnerBoundaryIs:(LinearRing list)
    type Model = Model of Id:Id * AltitudeMode:AltitudeMode * Location:Coordinates * Orientation:Orientation * Scale:Scale * Link:Link * ResourceMap:ResourceMap

    type Track
    type MultiTrack = MultiTrack of Id:Id * AltitudeMode:AltitudeMode * Interpolate:bool * Track:(Track list)

    type Geometry =
    | Point of Point
    | LineString of LineString
    | LinearRing of LinearRing
    | Polygon of Polygon
    | Track of Track
    | MultiTrack of MultiTrack
    | MultiGeometry of MultiGeometry
    and MultiGeometry = MultiGeometry of Geometry list
