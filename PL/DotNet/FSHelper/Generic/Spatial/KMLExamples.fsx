#load @"../Collections.fs"
#load "GeoLocation.fs"
#load "KeyholeMarkupLanguage.fs"

open System.IO

let samples =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "..", "..", "Data", "KMLExamples")
    let info = DirectoryInfo(path)
    info.FullName
// assert(Directory.Exists(samples))

let content = File.ReadAllText(Path.Combine(samples, "KML_Samples.kml"))
