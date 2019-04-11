module GraphsQCCompressionTests

open Xunit
open CGFSHelper.Graphs.QuickGraph
open CGFSHelper.Graphs.QuickGraphCompression

[<Fact>]
let ``Compress line graph`` () =
    let nodes = 10
    let line = Mk.Line nodes
    let reducedLine = CollapseDegree2 line
    Assert.Equal(1, reducedLine.EdgeCount)
    Assert.Equal(2, reducedLine.VertexCount)
    Assert.True(Test.IsConnected reducedLine)

[<Fact>]
let ``Uncompress line graph`` () =
    let nodes = 10
    let line = Mk.Line nodes
    let reducedLine = CollapseDegree2 line
    let line' = Restore reducedLine
    Assert.True(Comparison.StructurallyEqual(line, line'))

[<Fact>]
let ``Skeleton of line graph`` () =
    let nodes = 10
    let line = Mk.Line nodes
    let reducedLine = CollapseDegree2 line
    let lineSkeleton = Ops.RemoveTag reducedLine
    Assert.Equal(1, lineSkeleton.EdgeCount)
    Assert.Equal(2, lineSkeleton.VertexCount)
    Assert.True(Test.IsConnected lineSkeleton)

[<Fact>]
let ``Compress circular graph`` () =
    let nodes = 10
    let circle = Mk.Circle nodes
    let reducedCircle = CollapseDegree2 circle
    Assert.Equal(1, reducedCircle.EdgeCount)
    Assert.Equal(2, reducedCircle.VertexCount)
    Assert.True(Test.IsConnected reducedCircle)
    let reducedCircleEdge = reducedCircle.Edges |> Seq.head
    Assert.True(reducedCircleEdge.Tag.IsParallelEdge)

[<Fact>]
let ``Uncompress circular graph`` () =
    let nodes = 10
    let circle = Mk.Circle nodes
    let reducedCircle = CollapseDegree2 circle
    let circle' = Restore reducedCircle
    Assert.True(Comparison.StructurallyEqual(circle, circle'))

[<Fact>]
let ``Skeleton of circular graph`` () =
    let nodes = 10
    let circle = Mk.Circle nodes
    let reducedCircle = CollapseDegree2 circle
    let circleSkeleton = Ops.RemoveTag reducedCircle
    Assert.Equal(1, circleSkeleton.EdgeCount)
    Assert.Equal(2, circleSkeleton.VertexCount)
    Assert.True(Test.IsConnected circleSkeleton)

let private ConstructLineWithDiamond () =
(* The graph is as follows:
3
     / \
1 - 2   5 - 6
     \ /
4
*)
    let graph = QuickGraph.UndirectedGraph<int, QuickGraph.UndirectedEdge<int>>()
    for i = 1 to 6 do Ops.Add(graph, i)
    Ops.Add(graph, 1, 2)
    Ops.Add(graph, 2, 3)
    Ops.Add(graph, 2, 4)
    Ops.Add(graph, 3, 5)
    Ops.Add(graph, 4, 5)
    Ops.Add(graph, 5, 6)
    graph

[<Fact>]
let ``Compress line graph with diamond in the middle`` () =
    let graph = ConstructLineWithDiamond ()
    let reducedGraph = CollapseDegree2 graph
    Assert.Equal(3, reducedGraph.EdgeCount)
    Assert.Equal(4, reducedGraph.VertexCount)
    Assert.True(Test.IsConnected reducedGraph)
    Assert.True(Ops.ContainsEdge(reducedGraph, 1, 2))
    Assert.True(Ops.ContainsEdge(reducedGraph, 2, 5))
    Assert.True(Ops.ContainsEdge(reducedGraph, 5, 6))
    Assert.True(Ops.ContainsEdge(reducedGraph, 2, 3) = false)
    Assert.True(Ops.ContainsEdge(reducedGraph, 3, 5) = false)
    Assert.True(Ops.ContainsEdge(reducedGraph, 5, 6))
    let _, reducedGraphEdge = reducedGraph.TryGetEdge(2, 5)
    Assert.True(reducedGraphEdge.Tag.IsParallelEdge)

    match reducedGraphEdge.Tag with
    | Parallel edges    -> Assert.Equal(2, edges.Length)
    | _                 -> Assert.True(false, "Unexpected type of collapsed edge; should have been parallel")

[<Fact>]
let ``Uncompress line graph with diamond in the middle`` () =
    let graph = ConstructLineWithDiamond ()
    let reducedGraph = CollapseDegree2 graph
    let graph' = Restore reducedGraph
    Assert.True(Comparison.StructurallyEqual(graph, graph'))

let private ConstructDiamondHungingFromLine () =
(* The graph is as follows:
1 ----- 2 ----- 3
       / \
      4  6
      \ /
       5
*)

    let diamond = QuickGraph.UndirectedGraph<int, QuickGraph.UndirectedEdge<int>>()
    for i = 1 to 6 do Ops.Add(diamond, i)
    Ops.Add(diamond, 1, 2)
    Ops.Add(diamond, 2, 3)
    Ops.Add(diamond, 2, 4)
    Ops.Add(diamond, 2, 6)
    Ops.Add(diamond, 4, 5)
    Ops.Add(diamond, 5, 6)
    diamond

[<Fact>]
let ``Compress diamond hanging of a line`` () =
    let diamond = ConstructDiamondHungingFromLine ()
    let reducedDiamond = CollapseDegree2 diamond
    Assert.Equal(3, reducedDiamond.EdgeCount)
    Assert.Equal(4, reducedDiamond.VertexCount)
    Assert.True(Test.IsConnected reducedDiamond)
    Assert.True(Ops.ContainsEdge(reducedDiamond, 1, 2))
    Assert.True(Ops.ContainsEdge(reducedDiamond, 2, 3))
    Assert.True(Ops.ContainsEdge(reducedDiamond, 2, 4) || Ops.ContainsEdge(reducedDiamond, 2, 5) || Ops.ContainsEdge(reducedDiamond, 2, 6))
    let reducedDiamondEdge =
        match reducedDiamond.TryGetEdge(2, 4), reducedDiamond.TryGetEdge(2, 5), reducedDiamond.TryGetEdge(2, 6) with
        | (true, edge), (false, _),   (false, _)
        | (false, _),   (true, edge), (false, _)
        | (false, _),   (false, _),   (true, edge)
            -> Some edge
        | _     -> None

    Assert.True(reducedDiamondEdge.IsSome, "Only one edge of (2,4), (2, 5), (2,6) should exist")
    Assert.True(reducedDiamondEdge.Value.Tag.IsParallelEdge)

    match reducedDiamondEdge.Value.Tag with
    | Parallel edges    -> Assert.Equal(2, edges.Length)
    | _                 -> Assert.True(false, "Unexpected type of edge; expecting Parallel")

[<Fact>]
let ``Restore diamond hanging of a line`` () =
    let diamond = ConstructDiamondHungingFromLine ()
    let reducedDiamond = CollapseDegree2 diamond
    let diamond' = Restore reducedDiamond
    Assert.True(Comparison.StructurallyEqual(diamond, diamond'))
