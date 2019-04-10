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


type CollapsedEdgeInfo<'TVertex, 'TEdge when 'TEdge :> QuickGraph.IEdge<'TVertex>> =
| Simple    of 'TEdge
| Multiple  of 'TEdge list
| Parallel  of ('TEdge list) list
with
    static member Make (edge : 'TEdge) = Simple edge
    static member Make (edge : 'TEdge list) =
        match edge with
        | []            -> failwithf "Empty list of edges"
        | single :: []  -> Simple single
        | _             -> Multiple edge

    member this.IsSimpleEdge =
        match this with
        | Simple _  -> true
        | _         -> false

    member this.IsCollapsedEdge =
        match this with
        | Multiple _    -> true
        | _             -> false

    member this.IsParallelEdge =
        match this with
        | Parallel _    -> true
        | _             -> false

    member this.GetSimpleEdge() =
        match this with
        | Simple edge   -> edge
        | _             -> failwithf "XXXXXX"

    member this.Simplify() =
        let rec simplify (current : CollapsedEdgeInfo<'TVertex, 'TEdge>) =
            match current with
            | Simple _              -> current
            | Multiple []           -> failwithf "Invalid"
            | Multiple (hd :: [])   -> Simple hd
            | Multiple _            -> current
            | Parallel []           -> failwithf "Invalid"
            | Parallel (hd :: [])   ->
                let c = Multiple hd
                simplify c
            | Parallel _            -> current

        simplify this

    member this.Extend (other : CollapsedEdgeInfo<'TVertex, 'TEdge>) =
        match this, other with
        | Simple t, Simple o        -> Parallel [[t]; [o]]
        | Simple t, Multiple o      -> Parallel [[t]; o]
        | Simple t, Parallel o      -> Parallel ([t] :: o)
        | Multiple t, Simple o      -> Parallel [t; [o]]
        | Multiple t, Multiple o    -> Parallel [t; o]
        | Multiple t, Parallel o    -> Parallel (t :: o)
        | Parallel t, Simple o      -> Parallel (t @ [[o]])
        | Parallel t, Multiple o    -> Parallel (t @ [o])
        | Parallel t, Parallel o    -> Parallel (t @ o)

    static member Linearize (info : CollapsedEdgeInfo<'TVertex, 'TEdge> list) =
        let rec make worklist current =
            match worklist with
            | []                    -> Multiple current
            | (Simple info) :: tl   -> make tl (current @ [info])
            | (Multiple info) :: tl -> make tl (current @ info)
            | (Parallel _) :: _     -> failwithf "Cannot make linear"

        match info with
        | []        -> failwithf "Cannot make linear empty list"
        | hd :: []  -> hd
        | _         -> make info []

open QuickGraph

type CollapsedEdge<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> =
    TaggedUndirectedEdge<'TVertex, CollapsedEdgeInfo<'TVertex, 'TEdge>>
type CollapsedGraph<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> =
    UndirectedGraph<'TVertex, CollapsedEdge<'TVertex, 'TEdge>>

let InitializeCollapsed<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UndirectedGraph<'TVertex, 'TEdge>) =
    let graph' = CollapsedGraph(allowParallelEdges=false)

    graph.Vertices
    |> Seq.iter (fun v -> Ops.Add (graph', v, ignoreErrors=false))

    graph.Edges
    |> Seq.iter (
        fun e ->
            let edge = CollapsedEdge(e.Source, e.Target, Simple e)
            Ops.Add(graph', edge, ignoreErrors=false)
    )

    graph'

let Collapse<'TVertex, 'TEdge when 'TVertex : equality and 'TEdge :> IEdge<'TVertex>> (graph : UndirectedGraph<'TVertex, 'TEdge>)
    : CollapsedGraph<'TVertex, 'TEdge>
    =
        let graph = InitializeCollapsed graph

        let candidates =
            graph.Vertices
            |> Seq.filter (fun v -> graph.AdjacentDegree(v) = 2)
            |> Seq.toList

        printfn "Found %d candidates" candidates.Length

        let rec simplify (work : 'TVertex list) =
            match work with
            | []    -> ()
            | hd :: tl when graph.ContainsVertex(hd) = false    ->
                // Vertex has been simplified and removed from graph before
                simplify tl
            | hd :: tl when graph.AdjacentDegree(hd) <> 2       ->
                // The vertex changed degree due to a simplification (from a circle)
                simplify tl
            | hd :: tl ->
                // The vertex is still active and still has degree 2
                printfn "Examining node %A" hd

                let rec search (current : 'TVertex) (edge : CollapsedEdge<'TVertex, 'TEdge>) path =
                    let other = edge.GetOtherVertex(current)
                    if graph.AdjacentDegree(other) <> 2 || other = hd then other, edge :: path
                    else
                        let next =
                            let candidate = graph.AdjacentEdge(other, 0)
                            if candidate.GetOtherVertex(other) = current
                            then graph.AdjacentEdge(other, 1)
                            else candidate

                        search other next (edge :: path)

                let endpoint1, path1 = search hd (graph.AdjacentEdge(hd, 0)) []
                let endpoint2, path2 = search hd (graph.AdjacentEdge(hd, 1)) []
                assert(path1 |> List.forall (fun edge -> edge.Tag.IsSimpleEdge))
                assert(path2 |> List.forall (fun edge -> edge.Tag.IsSimpleEdge))

                // Remove existing edges from graph; we will merge those
                path1 |> List.iter (fun e -> Ops.Remove(graph, e, ignoreErrors=false))
                path2 |> List.iter (fun e -> Ops.Remove(graph, e, ignoreErrors=false))

                let deleteIfNeeded v =
                    if v <> endpoint1 && v <> endpoint2
                    then Ops.Remove (graph, v, ignoreErrors=true)

                if endpoint1 = endpoint2 && endpoint1 = hd then
                    // Found a circle, and there is no way out of the circle
                    printfn "Detected circle from %A" hd

                    let midpoint = (List.length path1) / 2
                    let path1, path2 = List.splitAt midpoint path1
                    let endpoint1 = (List.head path1).Source
                    let endpoint2 = (List.last path2).Target

                    printfn "Path 1: %A" path1
                    printfn "Path 2: %A" path2

                    path1 @ path2
                    |> List.collect(fun edge -> [edge.Source; edge.Target])
                    |> List.iter (
                        fun vertex ->
                            if vertex <> endpoint1 && vertex <> endpoint2 && graph.ContainsVertex(vertex) then
                                Ops.Remove(graph, vertex, ignoreErrors=false)
                    )

                    let info : 'TEdge list list =
                        [
                            path1 |> List.map (fun e -> e.Tag.GetSimpleEdge());
                            path2 |> List.map (fun e -> e.Tag.GetSimpleEdge())
                        ]
                    let info = Parallel info

                    Ops.Add(graph, endpoint1, endpoint2, info, ignoreErrors=false, createVertices=false) |> ignore

                elif endpoint1 = endpoint2 then
                    // Found a circle
                    let midpoint = (List.length path1 + List.length path2) / 2

                    failwith "Not implemented"
                else
                    let path = path1 @ (List.rev path2)

                    path
                    |> List.iter (
                        fun e ->
                            deleteIfNeeded e.Source
                            deleteIfNeeded e.Target
                    )

                    // Information for the next path that we need to add
                    let info =
                        let i = path |> List.map (fun p -> p.Tag) |> CollapsedEdgeInfo.Linearize
                        i.Simplify()

                    match graph.TryGetEdge(endpoint1, endpoint2) with
                    | false, _  ->
                        Ops.Add (graph, endpoint1, endpoint2, info, createVertices=false, ignoreErrors=false)
                    | true, edge ->
                        Ops.Remove(graph, edge, ignoreErrors = false)
                        let info = edge.Tag.Extend info
                        Ops.Add(graph, endpoint1, endpoint2, info, createVertices=false, ignoreErrors=false)

                simplify tl

        simplify candidates
        graph


let xxx = Collapse circle
printfn "%d, %d" xxx.VertexCount xxx.EdgeCount
let cl = xxx.Edges |> Seq.head
let (Parallel yy) = cl.Tag

