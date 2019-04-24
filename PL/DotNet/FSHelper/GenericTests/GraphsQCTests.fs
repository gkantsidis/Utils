module GraphsQCTests

open Xunit
open CGFSHelper.Graphs.QuickGraph

[<Fact>]
let ``Make line graph`` () =
    let nodes = 10
    let line = Mk.Line nodes
    Assert.Equal(nodes, line.VertexCount)
    Assert.Equal(nodes-1, line.EdgeCount)
    Assert.True(Test.IsConnected line)

[<Fact>]
let ``Make circle graph`` () =
    let nodes = 10
    let line = Mk.Circle nodes
    Assert.Equal(nodes, line.VertexCount)
    Assert.Equal(nodes, line.EdgeCount)
    Assert.True(Test.IsConnected line)
