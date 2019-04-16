namespace CGFSHelper.Spatial

module GeoGraph =
    open System.Collections.Generic
    open System.Runtime.InteropServices
    open QuickGraph
    open GeoLocation
    open GeoHash
    open CGFSHelper.Graphs.QuickGraph

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

    /// <summary>
    /// Graph of geographical coordinates with edges between vertices (coordinate locations).
    /// The edges do not contain extra information.
    /// </summary>
    type SGeoGraph (resolution, allowParallelEdges, edgeEqualityComparer) =
        let graph = SUGraph<GeoTag>(allowParallelEdges, edgeEqualityComparer)
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

        let add_edge (source, target) =
            if source = target then
                warn "Source and destination vertex are the same; ignoring edge"

            match graph.TryGetEdge(source, target) with
            | false, _      ->
                Ops.Add(graph, source, target)
            | true, edge    ->
                Ops.Remove(graph, edge, ignoreErrors=false)
                Ops.Add(graph, source, target)

        let edge_added = DelegateEvent<EdgeAction<GeoTag, UndirectedEdge<GeoTag>>>()
        let edge_removed = DelegateEvent<EdgeAction<GeoTag, UndirectedEdge<GeoTag>>>()
        let vertex_added = DelegateEvent<VertexAction<GeoTag>>()
        let vertex_removed = DelegateEvent<VertexAction<GeoTag>>()

        new(resolution) = SGeoGraph(resolution, false, EdgeExtensions.GetUndirectedVertexEquality())
        new(resolution, allowParallelEdges) = SGeoGraph(resolution, allowParallelEdges, EdgeExtensions.GetUndirectedVertexEquality())

        static member Make(resolution, edges : (Coordinates * Coordinates) list) =
            let this = SGeoGraph(resolution)
            let graph = this.Graph :> UGraph<GeoTag, UndirectedEdge<GeoTag>>

            edges
            |> Seq.iter (
                fun (source, target) ->
                    let src : GeoTag = this.Index source
                    let dst : GeoTag = this.Index target
                    if src = dst then
                        warn "Source and destination are mapped to the same node %A<->%A (node %A)" source target src
                        Ops.Add(graph, src)
                    else
                        Ops.Add(graph, src)
                        Ops.Add(graph, dst)
                        this.AddEdge (src, dst)
            )

            this

        member this.Resolution  = resolution
        member this.Graph       = graph
        member this.Coordinates = coordinates

        member this.Index(position : Coordinates) = index position

        member this.IndexStrict(position : Coordinates) =
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
            Ops.Add(graph, tag)

        member this.AddEdge(source : Coordinates, target : Coordinates, ?replaceIfExists) =
            let replaceIfExists = defaultArg replaceIfExists false
            let src = this.Index source
            let dst = this.Index target
            if src = dst then
                warn "Source and destination vertex are the same; ignoring edge"
            else
                match graph.TryGetEdge(src, dst) with
                | false, _      -> Ops.Add(graph, src, dst)
                | true, edge    ->
                    if replaceIfExists then
                        Ops.Remove(graph, edge, ignoreErrors=false)
                        Ops.Add(graph, src, dst)
                    else
                        error "Edge %A<->%A already exists" src dst

        member this.AddEdge(source : GeoTag, target : GeoTag, ?replaceIfExists) =
            if source = target
            then warn "Source and destination are the same vertex; ignoring edge"
            else
                let replaceIfExists = defaultArg replaceIfExists false
                if coordinates.ContainsKey(source) = false then
                    error "Vertex %A not known" source
                if coordinates.ContainsKey(target) = false then
                    error "Vertex %A not known" target
                match graph.TryGetEdge(source, target) with
                | false, _      -> Ops.Add(graph, source, target)
                | true, edge    ->
                    if replaceIfExists then
                        Ops.Remove(graph, edge, ignoreErrors=false)
                        Ops.Add(graph, source, target)
                    else
                        error "Edge %A<->%A already exists" source target

        member this.RemoveEdge(source: GeoTag, target: GeoTag, ?ignoreErrors) =
            if source = target
            then warn "Source and destination are the same vertex; ignoring operation"
            else
                let ignoreErrors = defaultArg ignoreErrors true
                Ops.Remove(graph, source, target, ignoreErrors=ignoreErrors)

        member this.RemoveEdge(source : Coordinates, target : Coordinates, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            match this.IndexStrict source, this.IndexStrict target with
            | Some src, Some dst ->
                if src = dst
                then warn "Source and destination are the same vertex; ignoring operation"
                else Ops.Remove(graph, src, dst, ignoreErrors=ignoreErrors)

            | _, _  ->
                let src = this.Index source
                let dst = this.Index target
                this.RemoveEdge(src, dst, ignoreErrors)

        interface IMutableGraph<GeoTag, UndirectedEdge<GeoTag>> with
            override this.Clear() = this.Graph.Clear()

        interface IMutableEdgeListGraph<GeoTag, UndirectedEdge<GeoTag>> with
            override this.AddEdge edge = graph.AddEdge edge
            override this.AddEdgeRange edges = graph.AddEdgeRange edges
            override this.RemoveEdge edge = graph.RemoveEdge edge
            override this.RemoveEdgeIf (predicate) = graph.RemoveEdgeIf(predicate)
            [<CLIEvent>] override this.EdgeAdded = edge_added.Publish
            [<CLIEvent>] override this.EdgeRemoved = edge_removed.Publish

        interface IImplicitVertexSet<GeoTag> with
            override this.ContainsVertex vertex = graph.ContainsVertex vertex

        interface IVertexSet<GeoTag> with
            override this.IsVerticesEmpty = graph.IsVerticesEmpty
            override this.VertexCount = graph.VertexCount
            override this.Vertices = graph.Vertices

        interface IMutableVertexSet<GeoTag> with
            override this.AddVertex(vertex : GeoTag) =
                if coordinates.ContainsKey(vertex) = false then
                    error "Vertex %A not known" vertex
                    false
                else
                    Ops.Add(graph, vertex)
                    graph.ContainsVertex(vertex)

            override this.AddVertexRange (vertices: IEnumerable<GeoTag>) =
                vertices
                |> Seq.map (fun v -> (this :> IMutableVertexSet<GeoTag>).AddVertex v)
                |> Seq.filter id
                |> Seq.length

            override this.RemoveVertex vertex = graph.RemoveVertex vertex
            override this.RemoveVertexIf predicate = graph.RemoveVertexIf predicate

            [<CLIEvent>] override this.VertexAdded = vertex_added.Publish
            [<CLIEvent>] override this.VertexRemoved = vertex_removed.Publish

        interface IImplicitUndirectedGraph<GeoTag, UndirectedEdge<GeoTag>> with
            override this.EdgeEqualityComparer = graph.EdgeEqualityComparer
            override this.AdjacentEdges vertex = graph.AdjacentEdges vertex
            override this.AdjacentDegree vertex = graph.AdjacentDegree vertex
            override this.IsAdjacentEdgesEmpty vertex = graph.IsAdjacentEdgesEmpty vertex
            override this.AdjacentEdge (vertex, index) = graph.AdjacentEdge (vertex, index)
            override this.ContainsEdge (source, target) = graph.ContainsEdge (source, target)
            override this.TryGetEdge (source, target, [<Out>] edge : byref<UndirectedEdge<GeoTag>>) =
                let result, edge' = graph.TryGetEdge (source, target)
                edge <- edge'
                result

        interface IGraph<GeoTag, UndirectedEdge<GeoTag>> with
            override this.IsDirected = graph.IsDirected
            override this.AllowParallelEdges = graph.AllowParallelEdges

        interface IEdgeSet<GeoTag, UndirectedEdge<GeoTag>> with
            override this.IsEdgesEmpty = graph.IsEdgesEmpty
            override this.EdgeCount = graph.EdgeCount
            override this.Edges = graph.Edges
            override this.ContainsEdge edge = graph.ContainsEdge edge

        interface IMutableVertexAndEdgeSet<GeoTag, UndirectedEdge<GeoTag>> with
            override this.AddVerticesAndEdge edge =
                // TODO: Ideally we could try to recover the approximate position of the GeoTag and add it to the dictionary
                // This is not really what the user expects, since the expectation is that the call does not fail
                if coordinates.ContainsKey(edge.Source) = false then false
                elif coordinates.ContainsKey(edge.Target) = false then false
                else graph.AddVerticesAndEdge edge

            override this.AddVerticesAndEdgeRange (edges: IEnumerable<UndirectedEdge<GeoTag>>) =
                edges
                |> Seq.map (fun e -> (this :> IMutableVertexAndEdgeSet<GeoTag, UndirectedEdge<GeoTag>>).AddVerticesAndEdge e)
                |> Seq.filter id
                |> Seq.length

        interface IMutableUndirectedGraph<GeoTag, UndirectedEdge<GeoTag>> with
            override this.RemoveAdjacentEdgeIf(vertex, predicate) = graph.RemoveAdjacentEdgeIf(vertex, predicate)
            override this.ClearAdjacentEdges(vertex) = graph.ClearAdjacentEdges(vertex)

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
        member __.Coordinates = coordinates
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
                if this.Coordinates.ContainsKey(source) = false then
                    error "Vertex %A not known" source
                if this.Coordinates.ContainsKey(target) = false then
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
