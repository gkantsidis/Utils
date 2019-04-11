#I @"../../../.paket/load/net47/"
#load "NLog.fsx"
#load "YC.QuickGraph.fsx"

#load "../Collections.fs"
#load "QuickGraph.fs"

open CGFSHelper.Collections
open CGFSHelper.Graphs.QuickGraph

let line = Mk.Line 10
let circle = Mk.Circle 10
line.Edges |> Seq.iter (printfn "%A")
circle.Edges |> Seq.iter (printfn "%A")

Test.IsConnected line

let line2 = Transform.Map(line, (fun i -> i + 10))

Transform.MergeInto (line, line2)
Ops.Add(line, 1, 11)
Ops.Add(line, 5, 15)
