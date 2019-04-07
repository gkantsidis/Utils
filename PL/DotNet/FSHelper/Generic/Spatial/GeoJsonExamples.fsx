#I @"../../../.paket/load/net47/"
#load @"FSharp.Data.fsx"

#load @"../Collections.fs"
#load "GeoLocation.fs"
#load "GeoJSON.fs"

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
