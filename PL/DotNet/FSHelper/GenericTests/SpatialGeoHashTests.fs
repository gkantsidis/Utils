module SpatialGeoHashTests

open Xunit
open CGFSHelper.Spatial.GeoLocation
open CGFSHelper.Spatial.GeoHash

// Computing GeoHash coordinates use:
// - http://geohash.org/
// - http://geohash.co/

[<Theory>]
[<InlineData("u4pruydqqvj", 57.64911, 10.40744)>]
[<InlineData("gcpvj07qeu9", 51.50642, -0.12721)>]
[<InlineData("gcpvj07r4pj", 51.50652, -0.12728)>]
[<InlineData("r3gx2g59npc3",-33.865143, 151.209900)>]  // Sydney
[<InlineData("69y7pdfgzcn", -34.61315, -58.37723)>]     // Buenos Aires
//[<InlineData("", "", "")>]
let ``GeoHash of known places should be correct`` (tag : string, lat: float, lon: float) =
    let location = Coordinates.MakeFromArcDegreeNaked(lat, lon)
    let hash = GeoHash.Make (location, 32)
    let (GeoTag tag') = hash.AsTag.Value
    Assert.Equal(tag, tag'.Substring(0, tag.Length))
