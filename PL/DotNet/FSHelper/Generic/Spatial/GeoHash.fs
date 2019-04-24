namespace CGFSHelper.Spatial

/// Types and methods for supporting hashes of geographical coordinates.
/// Currently, it supports the hash function developed by Gustavo Niemeyer
/// (see https://en.wikipedia.org/wiki/Geohash)
module GeoHash =
    open System
    open System.Collections
    open System.Text
    open GeoLocation

    (*
    * Some introductory material:
    * - http://www.bigfastblog.com/geohash-intro
    *)

    /// Exceptions thrown for internal errors
    exception GeoHashException of string

    /// Mapping of digits to characters for building the string representation.
    /// Observe that 5 bits are mapped to a character.
    let private mappings = [|
        '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
        'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'j'; 'k'; 'm';
        'n'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x';
        'y'; 'z'
    |]

    /// The reverse mapping of characters to bits.
    let private reverse =
        mappings
        |> Array.toList
        |> List.mapi (fun i c -> c, i)
        |> Map.ofList

    /// Represents geographical coordinates as string
    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type GeoTag = | GeoTag of string
    with
        member this.StructuredFormatDisplay = let (GeoTag tag) = this in tag
        override this.ToString() = this.StructuredFormatDisplay

    /// Represents an approximation of geographical approximation.
    /// The accurancy level can be controlled.
    /// (The approach is from Gustavo Niemeyer, see <a href="https://en.wikipedia.org/wiki/Geohash">GeoHash at Wikipedia</a>).
    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type GeoHash = GeoHash of Latitude:BitArray * Longitude:BitArray
    with
        ///<summary>
        /// Encodes geographical coordinates with the specified precision using the GeoHash algorithm.
        ///</summary>
        ///<param name="coordinates">The coordinates to encode</param>
        ///<param name="resolution">Number of bits to keep from the original coordinates</param>
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

        /// Gets the bits representing the interleaved sequence of bits from
        /// the lat and lon bit vectors.
        /// This is used for debugging.
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

        /// Get the string geo tag.
        member this.AsTag =
            lazy (
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
            )

        member this.StructuredFormatDisplay : string = let (GeoTag tag) = this.AsTag.Value in tag

        override this.ToString() = this.StructuredFormatDisplay

        member this.AsCoordinates =
            let (GeoHash (lat, lon)) = this
            raise (NotImplementedException())

