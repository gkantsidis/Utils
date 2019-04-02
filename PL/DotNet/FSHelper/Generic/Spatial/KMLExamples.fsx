#I @"../../../.paket/load/net47/"
#load @"FSharp.Data.fsx"

#load @"../Collections.fs"
#load "GeoLocation.fs"
#load "KeyholeMarkupLanguage.fs"

open System.IO
open FSharp.Data

let samples =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "..", "..", "Data", "KMLExamples")
    let info = DirectoryInfo(path)
    info.FullName
// assert(Directory.Exists(samples))

let content = File.ReadAllText(Path.Combine(samples, "KML_Samples.kml"))

type XHtml = XmlProvider<Schema="../../../../../Data/Schemas/xhtml.xsd">
//type Namespace = XmlProvider<Schema="../../../../../Data/Schemas/namespace.xsd">
//type Atom = XmlProvider<Schema="../../../../../Data/Schemas/atom3.xsd">
//type Schema = XmlProvider<Schema="../../../../../Data/Schemas/ogckml22.xsd">
// type Schema = XmlProvider<Schema="http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd">

