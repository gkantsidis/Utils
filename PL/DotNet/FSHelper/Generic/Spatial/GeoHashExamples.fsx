#load @"../Collections.fs"
#load "GeoLocation.fs"
#load "GeoHash.fs"

open CGFSHelper.Spatial.GeoLocation
open CGFSHelper.Spatial.GeoHash

// See also:
// - http://geohash.org/
// - http://geohash.co/
//
// See also https://www.latlong.net/ for a quick way to generate coordinates from points

// This should be: u4pruydqqvj
let jutlang = Coordinates.MakeFromArcDegreeNaked(57.64911, 10.40744)

// This should be: gcpvj07qeu9
let point1 = Coordinates.MakeFromArcDegreeNaked(51.50642, -0.12721)

// This should be: gcpvj07r4pj
let point2 = Coordinates.MakeFromArcDegreeNaked(51.50652, -0.12728)
printfn "Distance: %A" (point1.DistanceTo(point2))

let xx = GeoHash.Make (point1, 20)
let yy = GeoHash.Make (point2, 20)
let zz = GeoHash.Make (jutlang, 20)

xx.AsBitString
yy.AsBitString
zz.AsBitString

xx.AsTag
yy.AsTag
zz.AsTag

//
// Examples with the 180o meridian
//

let eastmost = Coordinates.MakeFromArcDegreeNaked(0.0, 179.771698)
let westmost = Coordinates.MakeFromArcDegreeNaked(0.0, -159.452690)
let crossing = Region.FindBoundingBox([eastmost; westmost])
// The following cannot be more than 12,756 km, which is the equatorial diameter of earth
crossing.Value.Horizontal

let crossingReverse = Region.FindBoundingBox([westmost; eastmost])
crossingReverse.Value.Horizontal

let crossingA = Coordinates.MakeFromArcDegreeNaked(0.0, 180.0)
let crossingB = Coordinates.MakeFromArcDegreeNaked(0.0, -180.0)
let bb1 = Region.FindBoundingBox([eastmost; crossingA])
let bb2 = Region.FindBoundingBox([crossingB; westmost])
bb1.Value.Horizontal + bb2.Value.Horizontal

let meridian = Coordinates.MakeFromArcDegreeNaked(0.0, 0.0)
let bb1' = Region.FindBoundingBox([westmost; meridian])
let bb2' = Region.FindBoundingBox([meridian; eastmost])
bb1'.Value.Horizontal + bb2'.Value.Horizontal

// The following should be he equatorial diameter?
bb1'.Value.Horizontal + bb2'.Value.Horizontal + crossing.Value.Horizontal
