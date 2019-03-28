module SpatialGeoLocationTests

open System
open Xunit
open CGFSHelper.Spatial.GeoLocation

let private london = Coordinates.MakeFromArcDegreeNaked(51.509865, -0.118092)
let private cambridge = Coordinates.MakeFromArcDegreeNaked(52.294491, -0.048706)
let private paris = Coordinates.MakeFromArcDegreeNaked(48.864716, 2.349014)

// The following distances computed with http://www.evoxfacilities.co.uk/latlong.php
let private london_to_cambridge = Mk.Distance 87_450.0
let private london_to_paris     = Mk.Distance 342_840.0

/// Maximum error that we can tolerate: 1%
let private error_factor = Mk.Convert 0.01

[<Fact>]
let ``London to Cambridge distance`` () =
    let distance = london.DistanceTo cambridge
    let error = distance * error_factor
    Assert.InRange(distance, london_to_cambridge - error, london_to_cambridge + error)

[<Fact>]
let ``London to Paris distance`` () =
    let distance = london.DistanceTo paris
    let error = distance * error_factor
    Assert.InRange(distance, london_to_paris - error, london_to_paris + error)
