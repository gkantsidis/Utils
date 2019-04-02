namespace CGFSHelper.Spatial

module GeoHash =
    open System
    open System.Collections
    open System.Text
    open GeoLocation

    (*
    * Some introductory material:
    * - http://www.bigfastblog.com/geohash-intro
    *)

    exception GeoHashException of string

    let DecimalInternalString (v : decimal) =
        let parts = Decimal.GetBits(v)
        let isNegative = (parts.[3] &&& 0x80000000) <> 0
        let scale = ((parts.[3] >>> 16) &&& 0x7F)
        String.Format(
            "{0,10:X8}{1,10:X8}{2,10:X8}{3,10:X8}\tNegative:{4}\tScale:{5}",
            parts.[3], parts.[2], parts.[1], parts.[0], isNegative, scale
        )

    let mappings = [|
        '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
        'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'j'; 'k'; 'm';
        'n'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x';
        'y'; 'z'
    |]

    type GeoTag = | GeoTag of string

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
                        if angle < midpoint then
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

        member this.AsBitString =
            let sb = StringBuilder()
            let (GeoHash (lat, lon)) = this
            assert(lat.Count = lon.Count)
            for i = 0 to (lat.Count - 1) do
                match lat.[i], lon.[i] with
                | true, true    -> Printf.bprintf sb "00"
                | true, false   -> Printf.bprintf sb "01"
                | false, true   -> Printf.bprintf sb "10"
                | false, false  -> Printf.bprintf sb "11"

            sb.ToString()

        member this.AsTag =
            let sb = StringBuilder()
            let (GeoHash (lat, lon)) = this
            assert(lat.Count = lon.Count)

            let rec append (latIndex, lonIndex) (isLatNext : bool) (current : int, counter : int) =
                if counter = 5 then
                    sb.Append(mappings.[current]) |> ignore
                    append (latIndex, lonIndex) isLatNext (0, 0)
                elif counter > 5 then
                    raise (GeoHashException "Internal error: counter exceeded 5")
                elif latIndex = lat.Count && lonIndex = lon.Count then
                    if counter > 0 then sb.Append(mappings.[current]) |> ignore
                    ()
                elif latIndex = lat.Count && isLatNext then
                    // We are done with lat, proceed with lon
                    append (latIndex, lonIndex) false (current, counter)
                elif lonIndex = lon.Count && isLatNext = false then
                    // We are done with lon, proceed with lon
                    append (latIndex, lonIndex) true (current, counter)
                elif isLatNext then
                    let bit = lat.[latIndex]
                    let current = if bit then (current <<< 1) ||| 0x1 else (current <<< 1)
                    append (latIndex + 1, lonIndex) false (current, counter + 1)
                else
                    // IsLatNext = false
                    let bit = lon.[lonIndex]
                    let current = if bit then (current <<< 1) ||| 0x1 else (current <<< 1)
                    append (latIndex, lonIndex + 1) true (current, counter + 1)

            append (0, 0) false (0, 0)
            GeoTag (sb.ToString())

        member this.AsCoordinates =
            let (GeoHash (lat, lon)) = this
            raise (NotImplementedException())

