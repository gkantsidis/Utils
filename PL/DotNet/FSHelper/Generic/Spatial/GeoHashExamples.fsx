#load @"../Collections.fs"
#load "GeoLocation.fs"
#load "GeoHash.fs"

open CGFSHelper.Spatial.GeoLocation
open CGFSHelper.Spatial.GeoHash

// See also:
// - http://geohash.org/
// - http://geohash.co/

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

xx.AsTag
yy.AsTag
zz.AsTag
