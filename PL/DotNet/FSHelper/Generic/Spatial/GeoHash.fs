namespace CGFSHelper.Spatial

module GeoHash =
    open System
    open System.Collections
    open GeoLocation

    let DecimalInternalString (v : decimal) =
        let parts = Decimal.GetBits(v)
        let isNegative = (parts.[3] &&& 0x80000000) <> 0
        let scale = ((parts.[3] >>> 16) &&& 0x7F)
        String.Format(
            "{0,10:X8}{1,10:X8}{2,10:X8}{3,10:X8}\tNegative:{4}\tScale:{5}",
            parts.[3], parts.[2], parts.[1], parts.[0], isNegative, scale
        )


    type GeoHash = | GeoHash of Latitude:BitArray * Longitude:BitArray
    with
        static member Make(coordinates : DecimalImplementation.Coordinates, resolution : int) =
            assert (coordinates.IsValid)
            assert (resolution > 0)

            let offset = Math.Pow(10.0, 10.0) |> decimal

            let getBitArray (angle : DecimalImplementation.Radian) (max : Radian) =
                let result = BitArray(resolution)

                let rec assign (index : int) (min : Radian, max : Radian) =
                    if index >= resolution then ()
                    else
                        let midpoint = (min + max) / 2.0m
                        if angle < min then
                            result.Set(index, false)
                            assign (index+1) (min, midpoint)
                        else
                            result.Set(index, true)
                            assign (index+1) (midpoint, max)

                assign 0 (-max, max)
                result

            let pi = GeoLocation.DecimalImplementation.Make.ArcRadian Math.PI
            let lat = getBitArray coordinates.Latitude (pi / 2.0m)
            let lon = getBitArray coordinates.Longitude pi

            GeoHash (lat, lon)

        member this.AsCoordinates =
            let (GeoHash (lat, lon)) = this
            raise (NotImplementedException())

    GeoHash.Make (Coordinates.MakeFromArcDegreeNaked(51.50642, -0.12721), 8)
    GeoHash.Make (Coordinates.MakeFromArcDegreeNaked(51.50652, -0.12728), 8)

