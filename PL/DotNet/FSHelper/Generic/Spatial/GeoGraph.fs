namespace CGFSHelper.Spatial

// TODO: The general structure and the interfaces here need a good rethink.

/// Graphs where vertices represent points in the (geographical) coordinate space.
/// The graphs are undirectional, and the vertices are approximated by their GeoHash
/// (i.e. a string that captures the most significant bits of the coordinates)
module GeoGraph =
    open System.Collections.Generic
    open System.Runtime.InteropServices
    open QuickGraph
    open GeoLocation
    open GeoHash
    open CGFSHelper.Graphs.QuickGraph
    open CGFSHelper.Graphs.QuickGraphCompression

#if DONT_USE_NLOG
    let inline private throw fmt = failwithf fmt
    let inline private display_message prefix message = Printf.printfn "[%s]: %s" prefix message
    let inline private warn fmt     = Printf.ksprintf  (display_message "WARNING") fmt
    let inline private debug fmt    = Printf.ksprintf (display_message "DEBUG") fmt
    let inline private error fmt    = Printf.ksprintf (fun msg -> raise (GraphsException msg)) fmt
#else
    open NLog

    /// Logger for this module
    let private _logger = LogManager.GetCurrentClassLogger()

    let inline private throw fmt =
        let do_throw (message : string) =
            _logger.Error message
            raise (GraphsException message)
        Printf.ksprintf do_throw fmt

    let inline private warn fmt = Printf.ksprintf _logger.Warn fmt
    let inline private debug fmt = Printf.ksprintf _logger.Debug fmt
    let inline private error fmt = Printf.ksprintf _logger.Error fmt

#if INTERACTIVE
    // The following are used only in interactive (fsi) to help with enabling disabling
    // logging for particular modules.

    type internal Marker = interface end
    let _full_name = typeof<Marker>.DeclaringType.FullName
    let _name = typeof<Marker>.DeclaringType.Name
#endif
#endif

    type OverlayGraph<'TVertex, 'TEdge, 'TGraph, 'TVertexBase when
            'TVertex : comparison and
            'TVertexBase : equality and
            'TEdge :> IEdge<'TVertex> and
            'TGraph :> IMutableUndirectedGraph<'TVertex, 'TEdge> and
            'TGraph : (new : unit -> 'TGraph)
            >
            (map: 'TVertexBase -> 'TVertex, mkEdge: ('TVertexBase * 'TVertex) -> ('TVertexBase * 'TVertex) -> 'TEdge)
        =
            let graph = new 'TGraph()
            let mapping = Dictionary<'TVertex, 'TVertexBase>()

            let index position =
                let tag = map position
                match mapping.TryGetValue(tag) with
                | false, _          -> mapping.Add(tag, position)
                | true, existing    ->
                    if existing <> position then
                        warn "Key already exists with value %A (new value: %A)" existing position

                tag

            let add_overlay_vertex vertex =
                if mapping.ContainsKey(vertex) = false then
                    error "Vertex %A not known" vertex
                    false
                else
                    Ops.Add(graph, vertex)
                    graph.ContainsVertex(vertex)

            let try_add_vertices_and_edge (edge : 'TEdge) =
                // TODO: Ideally we could try to recover the approximate position of the GeoTag and add it to the dictionary
                // This is not really what the user expects, since the expectation is that the call does not fail
                if mapping.ContainsKey(edge.Source) = false then false
                elif mapping.ContainsKey(edge.Target) = false then false
                else graph.AddVerticesAndEdge edge

            let edge_added = DelegateEvent<EdgeAction<'TVertex,'TEdge>>()
            let edge_removed = DelegateEvent<EdgeAction<'TVertex, 'TEdge>>()
            let vertex_added = DelegateEvent<VertexAction<'TVertex>>()
            let vertex_removed = DelegateEvent<VertexAction<'TVertex>>()

            member __.Index position = index position

            member __.IndexStrict (position: 'TVertexBase) =
                let tag = map position
                match mapping.TryGetValue(tag) with
                | false, _          ->
                    mapping.Add(tag, position)
                    Some tag
                | true, existing    ->
                    if existing <> position
                    then None
                    else Some tag

            member __.AddVertex(vertex: 'TVertexBase) = let tag = map vertex in Ops.Add(graph, tag)

            member __.AddEdge(source : 'TVertexBase, target : 'TVertexBase, ?replaceIfExists) =
                let replaceIfExists = defaultArg replaceIfExists false
                let src = index source
                let dst = index target
                if src = dst then
                    warn "Source and destination vertex are the same; ignoring edge"
                else
                    match graph.TryGetEdge(src, dst) with
                    | false, _      ->
                        let edge = mkEdge (source, src) (target, dst)
                        Ops.Add(graph, edge)
                    | true, edge    ->
                        if replaceIfExists then
                            Ops.Remove(graph, edge, ignoreErrors=false)
                            let edge' = mkEdge (source, src) (target, dst)
                            Ops.Add(graph, edge')
                        else
                            error "Edge %A<->%A already exists" src dst

            member this.RemoveEdge(source : 'TVertexBase, target : 'TVertexBase, ?ignoreErrors) =
                let ignoreErrors = defaultArg ignoreErrors true
                match this.IndexStrict source, this.IndexStrict target with
                | Some src, Some dst ->
                    if src = dst
                    then warn "Source and destination are the same vertex; ignoring operation"
                    else Ops.Remove(graph, src, dst, ignoreErrors=ignoreErrors)

                | _, _  ->
                    let src = index source
                    let dst = index target
                    Ops.Remove(graph, src, dst, ignoreErrors)

            interface IImplicitVertexSet<'TVertex> with
                override __.ContainsVertex vertex = graph.ContainsVertex vertex

            interface IVertexSet<'TVertex> with
                override __.IsVerticesEmpty = graph.IsVerticesEmpty
                override __.VertexCount = graph.VertexCount
                override __.Vertices = graph.Vertices

            interface IMutableVertexSet<'TVertex> with
                override __.AddVertex(vertex : 'TVertex) = add_overlay_vertex vertex

                override __.AddVertexRange (vertices: IEnumerable<'TVertex>) =
                    vertices
                    |> Seq.map (fun v -> add_overlay_vertex v)
                    |> Seq.filter id
                    |> Seq.length

                override __.RemoveVertex vertex = graph.RemoveVertex vertex
                override __.RemoveVertexIf predicate = graph.RemoveVertexIf predicate

                [<CLIEvent>] override __.VertexAdded = vertex_added.Publish
                [<CLIEvent>] override __.VertexRemoved = vertex_removed.Publish

            interface IEdgeSet<'TVertex, 'TEdge> with
                override __.IsEdgesEmpty = graph.IsEdgesEmpty
                override __.EdgeCount = graph.EdgeCount
                override __.Edges = graph.Edges
                override __.ContainsEdge edge = graph.ContainsEdge edge

            interface IGraph<'TVertex, 'TEdge> with
                override __.IsDirected = graph.IsDirected
                override __.AllowParallelEdges = graph.AllowParallelEdges

            interface IMutableGraph<'TVertex, 'TEdge> with
                override __.Clear() = graph.Clear()

            interface IMutableEdgeListGraph<'TVertex, 'TEdge> with
                override __.AddEdge edge = graph.AddEdge edge
                override __.AddEdgeRange edges = graph.AddEdgeRange edges
                override __.RemoveEdge edge = graph.RemoveEdge edge
                override __.RemoveEdgeIf (predicate) = graph.RemoveEdgeIf(predicate)
                [<CLIEvent>] override __.EdgeAdded = edge_added.Publish
                [<CLIEvent>] override __.EdgeRemoved = edge_removed.Publish

            interface IImplicitUndirectedGraph<'TVertex, 'TEdge> with
                override __.EdgeEqualityComparer = graph.EdgeEqualityComparer
                override __.AdjacentEdges vertex = graph.AdjacentEdges vertex
                override __.AdjacentDegree vertex = graph.AdjacentDegree vertex
                override __.IsAdjacentEdgesEmpty vertex = graph.IsAdjacentEdgesEmpty vertex
                override __.AdjacentEdge (vertex, index) = graph.AdjacentEdge (vertex, index)
                override __.ContainsEdge (source, target) = graph.ContainsEdge (source, target)
                override __.TryGetEdge (source, target, [<Out>] edge : byref<'TEdge>) =
                    let result, edge' = graph.TryGetEdge (source, target)
                    edge <- edge'
                    result

            interface IMutableVertexAndEdgeSet<'TVertex, 'TEdge> with
                override __.AddVerticesAndEdge edge = try_add_vertices_and_edge edge

                override __.AddVerticesAndEdgeRange (edges: IEnumerable<'TEdge>) =
                    edges
                    |> Seq.map try_add_vertices_and_edge
                    |> Seq.filter id
                    |> Seq.length

            interface IMutableUndirectedGraph<'TVertex, 'TEdge> with
                override __.RemoveAdjacentEdgeIf(vertex, predicate) = graph.RemoveAdjacentEdgeIf(vertex, predicate)
                override __.ClearAdjacentEdges(vertex) = graph.ClearAdjacentEdges(vertex)

    type IGeoMapping =
        abstract member Coordinates : IReadOnlyDictionary<GeoTag, Coordinates> with get

    type IGeoTagged<'TTag> =
        inherit IGraph<GeoTag, TaggedUndirectedEdge<GeoTag, 'TTag>>
        inherit IVertexSet<GeoTag>
        inherit IEdgeSet<GeoTag, TaggedUndirectedEdge<GeoTag, 'TTag>>
        inherit IGeoMapping

    /// <summary>
    /// Graph of geographical coordinates with edges between vertices (coordinate locations).
    /// The edges do not contain extra information.
    /// </summary>
    type SGeoGraph(resolution: int, allowParallelEdges, edgeEqualityComparer) =
        inherit SUGraph<GeoTag>(allowParallelEdges, edgeEqualityComparer)

        let coordinates = Dictionary<GeoTag, Coordinates>()

        let index position =
            let bits = GeoHash.Make(position, resolution)
            let tag = bits.AsTag.Value
            match coordinates.TryGetValue(tag) with
            | false, _          -> coordinates.Add(tag, position)
            | true, existing    ->
                if existing <> position then
                    let distance = position.DistanceTo(existing)
                    warn "Key already exists at distance %A" distance

            tag

        new (resolution) = SGeoGraph(resolution, false, EdgeExtensions.GetUndirectedVertexEquality())
        new (resolution, allowParallelEdges) = SGeoGraph(resolution, allowParallelEdges, EdgeExtensions.GetUndirectedVertexEquality())

        static member Make(resolution: int, edges : (Coordinates * Coordinates) seq) =
            let graph = SGeoGraph(resolution)

            edges
            |> Seq.iter (
                fun (source, target) ->
                    let src : GeoTag = graph.Index(source)
                    let dst : GeoTag = graph.Index(target)
                    if src = dst then
                        warn "Source and destination are mapped to the same node %A<->%A (node %A)" source target src
                        Ops.Add(graph, src)
                    else
                        Ops.Add(graph, src)
                        Ops.Add(graph, dst)
                        graph.AddEdge(src, dst, replaceIfExists=true)
            )

        member __.Resolution = resolution
        member this.Graph = this :> SUGraph<GeoTag>

        member __.Index position = index position
        member __.IndexStrict (position : Coordinates) =
            let bits = GeoHash.Make(position, resolution)
            let tag = bits.AsTag.Value
            match coordinates.TryGetValue(tag) with
            | false, _          ->
                coordinates.Add(tag, position)
                Some tag
            | true, existing    ->
                if existing <> position
                then None
                else Some tag

        member this.AddVertex(vertex : Coordinates) =
            let tag = this.Index(vertex)
            Ops.Add(this.Graph, tag)

        member this.AddEdge(source : Coordinates, target : Coordinates, ?replaceIfExists) =
            let replaceIfExists = defaultArg replaceIfExists false
            let src = index source
            let dst = index target
            if src = dst
            then warn "Source and destination map to same vertex %A<->%A (Vertex: %A)" source target src
            else
                Ops.Add(this.Graph, src, ignoreErrors = true)
                Ops.Add(this.Graph, dst, ignoreErrors = true)
                match this.Graph.TryGetEdge(src, dst) with
                | false, _      -> Ops.Add(this.Graph, src, dst)
                | true, edge    ->
                    if replaceIfExists then
                        Ops.Remove(this.Graph, edge, ignoreErrors=false)
                        Ops.Add(this.Graph, src, dst)
                    else
                        error "Edge %A<->%A already exists" src dst

        member this.AddEdge(source : GeoTag, target : GeoTag, replaceIfExists) =
            if source = target
            then warn "Source and destination are the same vertex; ignoring edge"
            else
                if coordinates.ContainsKey(source) = false then
                    error "Vertex %A not known" source
                if coordinates.ContainsKey(target) = false then
                    error "Vertex %A not known" target
                match this.Graph.TryGetEdge(source, target) with
                | false, _      -> Ops.Add(this.Graph, source, target)
                | true, edge    ->
                    if replaceIfExists then
                        Ops.Remove(this.Graph, edge, ignoreErrors=false)
                        Ops.Add(this.Graph, source, target)
                    else
                        error "Edge %A<->%A already exists" source target

        member this.RemoveEdge(source: GeoTag, target: GeoTag, ignoreErrors) =
            if source = target
            then warn "Source and destination are the same vertex; ignoring removal operation"
            else Ops.Remove(this.Graph, source, target, ignoreErrors=ignoreErrors)

        member this.RemoveEdge(source : Coordinates, target : Coordinates, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            match this.IndexStrict source, this.IndexStrict target with
            | Some src, Some dst ->
                if src = dst
                then warn "Source and destination are the same vertex; ignoring removal operation"
                else Ops.Remove(this.Graph, src, dst, ignoreErrors=ignoreErrors)
            | _, _  ->
                let src = this.Index source
                let dst = this.Index target
                this.RemoveEdge(src, dst, ignoreErrors)

        interface IGeoMapping with
            member __.Coordinates = coordinates :> IReadOnlyDictionary<GeoTag, Coordinates>

        member __.FromDictionary(other : IGeoMapping) =
            other.Coordinates
            |> Seq.iter (
                fun entry ->
                    if coordinates.ContainsKey(entry.Key)
                    then warn "Key %A already exists" entry.Key
                    else coordinates.Add(entry.Key, entry.Value)
            )


    /// <summary>
    /// Graph of geographical coordinates with edges between vertices (coordinate locations).
    /// Edges contain extra information.
    /// </summary>
    type TGeoGraph<'TTag> (resolution: int, allowParallelEdges, edgeEqualityComparer) =
        inherit TUGraph<GeoTag, 'TTag>(allowParallelEdges, edgeEqualityComparer)

        let coordinates = Dictionary<GeoTag, Coordinates>()

        let index position =
            let bits = GeoHash.Make(position, resolution)
            let tag = bits.AsTag.Value
            match coordinates.TryGetValue(tag) with
            | false, _          -> coordinates.Add(tag, position)
            | true, existing    ->
                if existing <> position then
                    let distance = position.DistanceTo(existing)
                    warn "Key already exists at distance %A" distance

            tag

        new (resolution) = TGeoGraph(resolution, false, EdgeExtensions.GetUndirectedVertexEquality())
        new (resolution, allowParallelEdges) = TGeoGraph(resolution, allowParallelEdges, EdgeExtensions.GetUndirectedVertexEquality())

        static member Make (resolution, edges : (Coordinates * Coordinates * 'TTag) seq) =
            let graph = TGeoGraph(resolution)

            edges
            |> Seq.iter (
                fun (source, target, info) ->
                    let src : GeoTag = graph.Index(source)
                    let dst : GeoTag = graph.Index(target)
                    if src = dst then
                        warn "Source and destination are mapped to the same node %A<->%A (node %A)" source target src
                        Ops.Add(graph, src)
                    else
                        Ops.Add(graph, src)
                        Ops.Add(graph, dst)
                        graph.AddEdge(src, dst, info, replaceIfExists=true)
            )

        static member Make(resolution, edges : (Coordinates * Coordinates) seq, info: 'TTag) =
            let graph = TGeoGraph(resolution)

            edges
            |> Seq.iter (
                fun (source, target) ->
                    let src : GeoTag = graph.Index(source)
                    let dst : GeoTag = graph.Index(target)
                    if src = dst then
                        warn "Source and destination are mapped to the same node %A<->%A (node %A)" source target src
                        Ops.Add(graph, src)
                    else
                        Ops.Add(graph, src)
                        Ops.Add(graph, dst)
                        graph.AddEdge(src, dst, info, replaceIfExists=true)
            )

        member __.Resolution = resolution
        member this.Graph = this :> TUGraph<GeoTag, 'TTag>

        member __.Index position = index position
        member __.IndexStrict (position : Coordinates) =
            let bits = GeoHash.Make(position, resolution)
            let tag = bits.AsTag.Value
            match coordinates.TryGetValue(tag) with
            | false, _          ->
                coordinates.Add(tag, position)
                Some tag
            | true, existing    ->
                if existing <> position
                then None
                else Some tag

        member this.AddVertex(vertex : Coordinates) =
            let tag = this.Index(vertex)
            Ops.Add(this.Graph, tag)

        member this.AddEdge(source : Coordinates, target : Coordinates, info : 'TTag, ?replaceIfExists) =
            let replaceIfExists = defaultArg replaceIfExists false
            let src = this.Index source
            let dst = this.Index target
            if src = dst
            then warn "Source and destination map to same vertex %A<->%A (Vertex: %A)" source target src
            else
                Ops.Add(this.Graph, src, ignoreErrors = true)
                Ops.Add(this.Graph, dst, ignoreErrors = true)
                match this.Graph.TryGetEdge(src, dst) with
                | false, _      -> Ops.Add(this.Graph, src, dst, info)
                | true, edge    ->
                    if replaceIfExists then
                        Ops.Remove(this.Graph, edge, ignoreErrors=false)
                        Ops.Add(this.Graph, src, dst, info)
                    else
                        error "Edge %A<->%A already exists" src dst

        member this.AddEdge(source : GeoTag, target : GeoTag, info : 'TTag, replaceIfExists) =
            if source = target
            then warn "Source and destination are the same vertex; ignoring edge"
            else
                if coordinates.ContainsKey(source) = false then
                    error "Vertex %A not known" source
                if coordinates.ContainsKey(target) = false then
                    error "Vertex %A not known" target
                match this.Graph.TryGetEdge(source, target) with
                | false, _      -> Ops.Add(this.Graph, source, target, info)
                | true, edge    ->
                    if replaceIfExists then
                        Ops.Remove(this.Graph, edge, ignoreErrors=false)
                        Ops.Add(this.Graph, source, target, info)
                    else
                        error "Edge %A<->%A already exists" source target

        member this.RemoveEdge(source: GeoTag, target: GeoTag, ignoreErrors) =
            if source = target
            then warn "Source and destination are the same vertex; ignoring removal operation"
            else Ops.Remove(this.Graph, source, target, ignoreErrors=ignoreErrors)

        member this.RemoveEdge(source : Coordinates, target : Coordinates, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            match this.IndexStrict source, this.IndexStrict target with
            | Some src, Some dst ->
                if src = dst
                then warn "Source and destination are the same vertex; ignoring removal operation"
                else Ops.Remove(this.Graph, src, dst, ignoreErrors=ignoreErrors)
            | _, _  ->
                let src = this.Index source
                let dst = this.Index target
                this.RemoveEdge(src, dst, ignoreErrors)

        interface IGeoMapping with
            member __.Coordinates = coordinates :> IReadOnlyDictionary<GeoTag, Coordinates>

        member __.FromDictionary(other : IGeoMapping) =
            other.Coordinates
            |> Seq.iter (
                fun entry ->
                    if coordinates.ContainsKey(entry.Key)
                    then warn "Key %A already exists" entry.Key
                    else coordinates.Add(entry.Key, entry.Value)
            )

        static member Make<'TTag>(graph: SGeoGraph, mkEdge: UndirectedEdge<GeoTag> -> 'TTag) =
            let result = TGeoGraph<'TTag>(graph.Resolution, graph.AllowParallelEdges)
            result.FromDictionary(graph :> IGeoMapping)

            let vertices = result.AddVertexRange(graph.Vertices)
            if vertices <> graph.VertexCount then
                error "Failed to add all vertices, added %d, expected to add %d" vertices graph.VertexCount

            let edges =
                graph.Edges
                |> Seq.map (
                    fun edge ->
                        let tag = mkEdge edge
                        TaggedUndirectedEdge(edge.Source, edge.Target, tag)
                )
                |> result.AddEdgeRange

            if edges <> graph.EdgeCount then
                error "Failed to add all edges, added %d, expected to add %d" edges graph.EdgeCount

            result

        static member Make<'TTag>(resolution, graph : IGeoTagged<'TTag>) =
            let result = TGeoGraph<'TTag>(resolution, graph.AllowParallelEdges)
            result.FromDictionary(graph :> IGeoMapping)

            let vertices = result.AddVertexRange(graph.Vertices)
            if vertices <> graph.VertexCount then
                error "Failed to add all vertices, added %d, expected to add %d" vertices graph.VertexCount

            let edges = graph.Edges |> result.AddEdgeRange
            if edges <> graph.EdgeCount then
                error "Failed to add all edges, added %d, expected to add %d" edges graph.EdgeCount

            result

        static member Make<'TTag>(resolution, dictionary : IGeoMapping, graph : ITUGraph<GeoTag, 'TTag>) =
            let result = TGeoGraph<'TTag>(resolution, graph.AllowParallelEdges)
            result.FromDictionary(dictionary)

            let vertices = result.AddVertexRange(graph.Vertices)
            if vertices <> graph.VertexCount then
                error "Failed to add all vertices, added %d, expected to add %d" vertices graph.VertexCount

            let edges = graph.Edges |> result.AddEdgeRange
            if edges <> graph.EdgeCount then
                error "Failed to add all edges, added %d, expected to add %d" edges graph.EdgeCount

            result

    type Transform =
        static member CollapseDegree2 (graph : SGeoGraph) =
            let collapsed = CollapseDegree2 graph
            TGeoGraph.Make(graph.Resolution, graph, collapsed)

        static member CollapseDegree2<'TTag> (graph : TGeoGraph<'TTag>) =
            let collapsed = CollapseDegree2 graph
            TGeoGraph.Make(graph.Resolution, graph, collapsed)
