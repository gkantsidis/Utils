#I @"../../../.paket/load/net47/"
#load @"FSharp.Data.fsx"

#load @"../Collections.fs"
#load "GeoLocation.fs"
#load "KeyholeMarkupLanguage.fs"

open System.IO
open FSharp.Data
open CGFSHelper.Collections

let samples =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "..", "..", "Data", "KMLExamples")
    let info = DirectoryInfo(path)
    info.FullName
// assert(Directory.Exists(samples))

let content = File.ReadAllText(Path.Combine(samples, "KML_Samples.kml"))

//type XHtml = XmlProvider<Schema="../../../../../Data/Schemas/xhtml.xsd">
//type Atom = XmlProvider<Schema="http://schemas.opengis.net/kml/2.2.0/atom-author-link.xsd">

//type Schema = XmlProvider<Schema="../../../../../Data/Schemas/kk.xsd">
// type Schema = XmlProvider<Schema="http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd">

open System.Xml
open System.Xml.Schema
let set = XmlSchemaSet()
let atom = set.Add("http://www.w3.org/2005/Atom", "http://schemas.opengis.net/kml/2.2.0/atom-author-link.xsd")
let xal = set.Add("urn:oasis:names:tc:ciq:xsdschema:xAL:2.0", "http://docs.oasis-open.org/election/external/xAL.xsd")
let kml = set.Add("http://www.opengis.net/kml/2.2", "http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd")
let settings = set.CompilationSettings
settings.EnableUpaCheck <- true
set.CompilationSettings <- settings
set.Compile()
assert(set.IsCompiled)

let toXmlQualifiedName (o : obj) = o :?>  XmlQualifiedName
let toXmlSchemaType (o : obj) = o :?> XmlSchemaType

//type XmlSchemaType =
//| SimpleType of XmlSchemaSimpleType
//| ComplexType of XmlSchemaComplexType


//let xx = XmlSchema()
//let t1, t2 = let x = set.GlobalTypes.GetEnumerator() in ignore (x.MoveNext()); x.Key, x.Value
//t2.GetType().FullName
//let xxx = List.Cast.ofDictionary<XmlQualifiedName, XmlSchemaType>(set.GlobalTypes.GetEnumerator())

//xxx
//|> List.iter (
//    fun (k, v) ->
//        let item = XmlSchemaObject()
//        xx.Items.Add()
//)


//let final =
//    let s = set.Schemas("http://www.opengis.net/kml/2.2")
//    let e = s.GetEnumerator()
//    if e.MoveNext() then e.Current :?> XmlSchema |> Some
//    else None

//let result =
//    use writer = new StringWriter()
//    final.Value.Write(writer)
//    let text = writer.ToString()
//    File.WriteAllText(@"e:\temp\kk.xsd", text)
//    text

