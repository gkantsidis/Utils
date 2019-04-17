#I @"../../../.paket/load/net47/"
#load "NLog.fsx"
#load "YC.QuickGraph.fsx"

#load "../Collections.fs"
#load "QuickGraph.fs"
#load "QuickGraphCompression.fs"

open CGFSHelper.Collections
open CGFSHelper.Graphs.QuickGraph
open CGFSHelper.Graphs.QuickGraphCompression

let line = Mk.Line 10
let circle = Mk.Circle 10
line.Edges |> Seq.iter (printfn "%A")
circle.Edges |> Seq.iter (printfn "%A")

Test.IsConnected line

let line2 = Transform.Map(line, (fun i -> i + 10))

Transform.MergeInto (line, line2)
Ops.Add(line, 1, 11)
Ops.Add(line, 5, 15)

let graph = Mk.Line 6
Ops.Add (graph, 3, 7)
Ops.Add (graph, 7, 8)
Ops.Add (graph, 8, 9)
Ops.Add (graph, 9, 4)
let collapsed = CollapseDegree2 graph
CollapseDegree2InPlace collapsed
