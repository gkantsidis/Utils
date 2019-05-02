#I @"../../../.paket/load/net47/"
#load "YC.QuickGraph.fsx"
#load "NLog.fsx"

#load @"../Collections.fs"
#load @"../Graphs/QuickGraph.fs"
#load @"../Graphs/QuickGraphCompression.fs"
#load "GeoLocation.fs"
#load "GeoHash.fs"
#load "GeoGraph.fs"

open CGFSHelper.Graphs.QuickGraphCompression
open CGFSHelper.Spatial.GeoLocation
open CGFSHelper.Spatial.GeoHash
open CGFSHelper.Spatial.GeoGraph

let point1 = Coordinates.MakeFromArcDegreeNaked(51.50642, -0.12721)
let point2 = Coordinates.MakeFromArcDegreeNaked(51.50652, -0.12728)
let point3 = Coordinates.MakeFromArcDegreeNaked(57.64911, 10.40744)

let graph = SGeoGraph(20)
graph.AddEdge(point1, point2)
graph.AddEdge(point2, point3)

let collapsed = Transform.CollapseDegree2 graph
(collapsed :> IGeoMapping).Coordinates


//TGeoGraph.Make(graph, fun (edge : QuickGraph.UndirectedEdge<GeoTag>) -> CollapsedEdgeInfo.Simple edge)
//CollapseDegree2InPlace(graph :> ICollapsedGraph<GeoTag, QuickGraph.UndirectedEdge<GeoTag>>)

