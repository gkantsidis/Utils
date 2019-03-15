namespace CGFSHelper.Units

module GeoLocation =
    open Microsoft.FSharp.Core

    module FloatImplemenation =
        let private error : float<m> = LanguagePrimitives.FloatWithMeasure 0.001
        let private earth_radius : float<m> = LanguagePrimitives.FloatWithMeasure 6371000.0

        [<Measure>] type radians
        [<Measure>] type degrees

        type Radians = float<radians>
