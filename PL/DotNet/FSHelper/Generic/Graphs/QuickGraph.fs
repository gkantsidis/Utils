namespace CGFSHelper.Graphs

/// Helper methods and extensions for the QuickGraph library
module QuickGraph =
    open System
    open QuickGraph

    type GraphsException (message:string, ?innerException:exn) =
        inherit ApplicationException(
            message,
            match innerException with | Some ex -> ex | _ -> null)

#if DONT_USE_NLOG
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

    /// Undirected graph with nodes of integer identity and edges with decimal weights
    type UIDGraph = UndirectedGraph<int, TaggedEdge<int, decimal>>
    /// Undirected graph with nodes of integer identity and edges with double weights
    type UIFGraph = UndirectedGraph<int, TaggedEdge<int, float>>

    (*
    * The following types are used as short versions for the following code, but they can be useful in general
    *)

    /// Generic undirected graph
    type UGraph<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> = UndirectedGraph<'TVertex, 'TEdge>
    /// Simple undirected graph, with no edge information
    type SUGraph<'TVertex> = UndirectedGraph<'TVertex, UndirectedEdge<'TVertex>>
    /// Undirected graph with typed information on edges
    type TUGraph<'TVertex, 'TTag> = UndirectedGraph<'TVertex, TaggedUndirectedEdge<'TVertex, 'TTag>>
    /// Generic directed graph
    type DGraph<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> = AdjacencyGraph<'TVertex, 'TEdge>
    /// Simple directed graph, with no edge information
    type SDGraph<'TVertex> = AdjacencyGraph<'TVertex, Edge<'TVertex>>
    /// Directed graph with typed information on edges
    type TDGraph<'TVertex, 'TTag> = AdjacencyGraph<'TVertex, TaggedEdge<'TVertex, 'TTag>>

    /// In-place operations on graphs

    type Ops =
        static member inline private AddVertex<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: UGraph<'TVertex, 'TEdge>, vertex, ignoreErrors) =
            if graph.AddVertex(vertex) = false then
                if ignoreErrors then
                    warn "Failed to add vertex %A [ignoring]" vertex
                else
                    error "Failed to add vertex"

        static member inline private AddVertex<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: DGraph<'TVertex, 'TEdge>, vertex, ignoreErrors) =
            if graph.AddVertex(vertex) = false then
                if ignoreErrors then
                    warn "Failed to add vertex %A [ignoring]" vertex
                else
                    error "Failed to add vertex"

        static member inline private CheckAndAddVertex<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: UGraph<'TVertex, 'TEdge>, vertex, ignoreErrors) =
            if graph.ContainsVertex(vertex) = false then
                if graph.AddVertex(vertex) = false then
                    if ignoreErrors then
                        warn "Failed to add vertex %A [ignoring]" vertex
                    else
                        error "Failed to add vertex"

        static member inline private CheckAndAddVertex<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: DGraph<'TVertex, 'TEdge>, vertex, ignoreErrors) =
            if graph.ContainsVertex(vertex) = false then
                if graph.AddVertex(vertex) = false then
                    if ignoreErrors then
                        warn "Failed to add vertex %A [ignoring]" vertex
                    else
                        error "Failed to add vertex"

        static member inline private AddEdge<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: UGraph<'TVertex, 'TEdge>, edge, ignoreErrors) =
            if graph.AddEdge(edge) = false then
                if ignoreErrors then
                    warn "Failed to add edge %A [ignoring]" edge
                else
                    error "Failed to add edge"

        static member inline private AddEdge<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: DGraph<'TVertex, 'TEdge>, edge, ignoreErrors) =
            if graph.AddEdge(edge) = false then
                if ignoreErrors then
                    warn "Failed to add edge %A [ignoring]" edge
                else
                    error "Failed to add edge"

        static member Add<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UGraph<'TVertex, 'TEdge>, vertex, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            Ops.AddVertex(graph, vertex, ignoreErrors)

        static member Add<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: DGraph<'TVertex, 'TEdge>, vertex, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            Ops.AddVertex(graph, vertex, ignoreErrors)

        static member Add<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: UGraph<'TVertex, 'TEdge>, edge: 'TEdge, ?createVertices, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            let createVertices = defaultArg createVertices true

            if createVertices then
                Ops.CheckAndAddVertex(graph, edge.Source, ignoreErrors)
                Ops.CheckAndAddVertex(graph, edge.Target, ignoreErrors)

            Ops.AddEdge(graph, edge, ignoreErrors)

        static member Add<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: DGraph<'TVertex, 'TEdge>, edge: 'TEdge, ?createVertices, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            let createVertices = defaultArg createVertices true

            if createVertices then
                Ops.CheckAndAddVertex(graph, edge.Source, ignoreErrors)
                Ops.CheckAndAddVertex(graph, edge.Target, ignoreErrors)

            Ops.AddEdge(graph, edge, ignoreErrors)

        static member Add<'TVertex> (graph: SUGraph<'TVertex>, source, target, ?createVertices, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            let createVertices = defaultArg createVertices true

            if createVertices then
                Ops.CheckAndAddVertex(graph, source, ignoreErrors)
                Ops.CheckAndAddVertex(graph, target, ignoreErrors)

            let edge = UndirectedEdge<'TVertex>(source, target)
            Ops.AddEdge(graph, edge, ignoreErrors)

        static member Add<'TVertex> (graph: SDGraph<'TVertex>, source, target, ?createVertices, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            let createVertices = defaultArg createVertices true

            if createVertices then
                Ops.CheckAndAddVertex(graph, source, ignoreErrors)
                Ops.CheckAndAddVertex(graph, target, ignoreErrors)

            let edge = Edge<'TVertex>(source, target)
            Ops.AddEdge(graph, edge, ignoreErrors)

        static member Add<'TVertex, 'TTag> (graph: TUGraph<'TVertex, 'TTag>, source, target, tag, ?createVertices, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            let createVertices = defaultArg createVertices true

            if createVertices then
                Ops.CheckAndAddVertex(graph, source, ignoreErrors)
                Ops.CheckAndAddVertex(graph, target, ignoreErrors)

            let edge = TaggedUndirectedEdge(source, target, tag)
            Ops.AddEdge(graph, edge, ignoreErrors)

        static member Add<'TVertex, 'TTag> (graph: TDGraph<'TVertex, 'TTag>, source, target, tag, ?createVertices, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            let createVertices = defaultArg createVertices true

            if createVertices then
                Ops.CheckAndAddVertex(graph, source, ignoreErrors)
                Ops.CheckAndAddVertex(graph, target, ignoreErrors)

            let edge = TaggedEdge(source, target, tag)
            Ops.AddEdge(graph, edge, ignoreErrors)

        static member Remove<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: UGraph<'TVertex, 'TEdge>, vertex, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            if graph.ContainsVertex vertex = false then
                if ignoreErrors then
                    warn "Vertex %A is not in the graph" vertex
                else
                    error "Failed to remove vertex %A" vertex
            else
                let edges = graph.AdjacentEdges(vertex) |> Seq.toList
                edges
                |> List.iter (
                    fun e ->
                        let ok = graph.RemoveEdge(e)
                        if ok then ()
                        elif ignoreErrors
                        then warn "Failed to remove adjacent edge %A for vertex %A" e vertex
                        else error "Failed to remove adjacent edge %A for vertex %A" e vertex
                )

                let ok = graph.RemoveVertex(vertex)
                if ok then ()
                elif ignoreErrors
                then warn "Failed to remove vertex %A" vertex
                else error "Failed to remove vertex %A" vertex

        static member Remove<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: DGraph<'TVertex, 'TEdge>, vertex, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            if graph.ContainsVertex vertex = false then
                if ignoreErrors then
                    warn "Vertex %A is not in the graph" vertex
                else
                    error "Failed to remove vertex %A" vertex
            else
                let ok = graph.RemoveVertex(vertex)
                if ok then ()
                elif ignoreErrors
                then warn "Failed to remove vertex %A" vertex
                else error "Failed to remove vertex %A" vertex

        static member Remove<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: UGraph<'TVertex, 'TEdge>, edge, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true

            if graph.ContainsEdge(edge) = false then
                if ignoreErrors
                then warn "Edge %A does not exist when trying to remove" edge
                else error "Edge %A does not exist when trying to remove" edge
            else
                let ok = graph.RemoveEdge(edge)
                if ok = false then
                    if ignoreErrors
                    then warn "Cannot remove edge %A as requested (ignoring)" edge
                    else error "Cannot remove edge %A" edge

        static member Remove<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: DGraph<'TVertex, 'TEdge>, edge, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true

            if graph.ContainsEdge(edge) = false then
                if ignoreErrors
                then warn "Edge %A does not exist when trying to remove" edge
                else error "Edge %A does not exist when trying to remove" edge
            else
                let ok = graph.RemoveEdge(edge)
                if ok = false then
                    if ignoreErrors
                    then warn "Cannot remove edge %A as requested (ignoring)" edge
                    else error "Cannot remove edge %A" edge

        static member Remove<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: UGraph<'TVertex, 'TEdge>, source, target, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            match graph.TryGetEdge(source, target) with
            | true, edge    ->
                let ok = graph.RemoveEdge(edge)
                if ok = false then
                    if ignoreErrors
                    then warn "Cannot remove edge %A<->%A as requested (ignoring)" source target
                    else error "Cannot remove edge %A<->%A" source target
            | false, _      ->
                if ignoreErrors
                then warn "Edge %A<->%A does not exist and cannot be removed" source target
                else error "Edge %A<->%A does not exist and cannot be removed" source target

        static member Remove<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph: DGraph<'TVertex, 'TEdge>, source, target, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            match graph.TryGetEdge(source, target) with
            | true, edge    ->
                let ok = graph.RemoveEdge(edge)
                if ok = false then
                    if ignoreErrors
                    then warn "Cannot remove edge %A<->%A as requested (ignoring)" source target
                    else error "Cannot remove edge %A<->%A" source target
            | false, _      ->
                if ignoreErrors
                then warn "Edge %A<->%A does not exist and cannot be removed" source target
                else error "Edge %A<->%A does not exist and cannot be removed" source target

        static member RemoveTag<'TVertex, 'TTag> (graph: TUGraph<'TVertex, 'TTag>) : SUGraph<'TVertex> =
            let output = UndirectedGraph<'TVertex, UndirectedEdge<'TVertex>>()
            graph.Vertices |> Seq.iter (fun v -> Ops.Add(output, v, ignoreErrors = false))
            graph.Edges    |> Seq.iter (fun edge -> Ops.Add(output, edge.Source, edge.Target))
            output

        static member RemoveTag<'TVertex, 'TTag> (graph : TDGraph<'TVertex, 'TTag>) : SDGraph<'TVertex> =
            let output = SDGraph<'TVertex>()
            graph.Vertices |> Seq.iter (fun v -> Ops.Add(output, v, ignoreErrors = false))
            graph.Edges    |> Seq.iter (fun edge -> Ops.Add(output, edge.Source, edge.Target))
            output

        static member ContainsEdge<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UGraph<'TVertex, 'TEdge>, nodeA, nodeB) =
            graph.ContainsVertex(nodeA) && graph.ContainsVertex(nodeB) && graph.ContainsEdge(nodeA, nodeB)

        static member ContainsEdge<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : DGraph<'TVertex, 'TEdge>, nodeA, nodeB) =
            graph.ContainsVertex(nodeA) && graph.ContainsVertex(nodeB) && graph.ContainsEdge(nodeA, nodeB)

    type Transform =
        static member inline Map<'TVertex, 'TVertex2>(graph : SUGraph<'TVertex>, vertexMap : 'TVertex -> 'TVertex2)
            : SUGraph<'TVertex2>
            =
                let graph' = SUGraph<'TVertex2>(graph.AllowParallelEdges)

                graph.Vertices
                |> Seq.iter (
                    fun v ->
                        let v' = vertexMap v
                        Ops.Add(graph', v', ignoreErrors=false)
                )

                graph.Edges
                |> Seq.iter (
                    fun e ->
                        let e' = UndirectedEdge(vertexMap e.Source, vertexMap e.Target)
                        Ops.Add(graph', e', createVertices=false, ignoreErrors=false)
                )

                graph'

        static member inline Map<'TVertex, 'TVertex2>(graph : SDGraph<'TVertex>, vertexMap : 'TVertex -> 'TVertex2)
            : SDGraph<'TVertex2>
            =
                let graph' = SDGraph<'TVertex2>(graph.AllowParallelEdges)

                graph.Vertices
                |> Seq.iter (
                    fun v ->
                        let v' = vertexMap v
                        Ops.Add(graph', v', ignoreErrors=false)
                )

                graph.Edges
                |> Seq.iter (
                    fun e ->
                        let e' = Edge(vertexMap e.Source, vertexMap e.Target)
                        Ops.Add(graph', e', createVertices=false, ignoreErrors=false)
                )

                graph'

        static member inline Map<'TVertex, 'TVertex2, 'TTag, 'TTag2>
                                (graph : TUGraph<'TVertex, 'TTag>, vertexMap : 'TVertex -> 'TVertex2, tagMap : 'TTag -> 'TTag2)
            : TUGraph<'TVertex2, 'TTag2>
            =
                let graph' = TUGraph<'TVertex2, 'TTag2>(graph.AllowParallelEdges)

                graph.Vertices
                |> Seq.iter (
                    fun v ->
                        let v' = vertexMap v
                        Ops.Add(graph', v', ignoreErrors=false)
                )

                graph.Edges
                |> Seq.iter (
                    fun e ->
                        let e' = TaggedUndirectedEdge(vertexMap e.Source, vertexMap e.Target, tagMap e.Tag)
                        Ops.Add(graph', e', createVertices=false, ignoreErrors=false)
                )

                graph'

        static member inline Map<'TVertex, 'TVertex2, 'TTag, 'TTag2>
                                (graph : TDGraph<'TVertex, 'TTag>, vertexMap : 'TVertex -> 'TVertex2, tagMap : 'TTag -> 'TTag2)
            : TDGraph<'TVertex2, 'TTag2>
            =
                let graph' = TDGraph<'TVertex2, 'TTag2>(graph.AllowParallelEdges)

                graph.Vertices
                |> Seq.iter (
                    fun v ->
                        let v' = vertexMap v
                        Ops.Add(graph', v', ignoreErrors=false)
                )

                graph.Edges
                |> Seq.iter (
                    fun e ->
                        let e' = TaggedEdge(vertexMap e.Source, vertexMap e.Target, tagMap e.Tag)
                        Ops.Add(graph', e', createVertices=false, ignoreErrors=false)
                )

                graph'

        /// <summary>
        /// Merges the second graph into the first.
        /// A
        /// </summary>
        /// <param name="graph"></param>
        /// <param name="other"></param>
        static member inline MergeInto<'TVertex>(graph : SUGraph<'TVertex>, other : SUGraph<'TVertex>) =
            other.Vertices
            |> Seq.filter (fun v -> graph.ContainsVertex(v) = false)
            |> Seq.iter (fun v -> Ops.Add(graph, v, ignoreErrors=false))

            other.Edges
            |> Seq.filter (fun e -> graph.ContainsEdge(e.Source, e.Target) = false)
            |> Seq.iter (fun e -> Ops.Add(graph, e.Source, e.Target, createVertices=false, ignoreErrors=false))

            graph

        static member inline MergeInto<'TVertex>(graph : SDGraph<'TVertex>, other : SDGraph<'TVertex>) =
            other.Vertices
            |> Seq.filter (fun v -> graph.ContainsVertex(v) = false)
            |> Seq.iter (fun v -> Ops.Add(graph, v, ignoreErrors=false))

            other.Edges
            |> Seq.filter (fun e -> graph.ContainsEdge(e.Source, e.Target) = false)
            |> Seq.iter (fun e -> Ops.Add(graph, e.Source, e.Target, createVertices=false, ignoreErrors=false))

            graph

        static member inline MergeInto<'TVertex, 'TTag>
                                (graph : TUGraph<'TVertex, 'TTag>, other : TUGraph<'TVertex, 'TTag>,
                                 merge : 'TTag -> 'TTag -> 'TTag)
            =
                other.Vertices
                |> Seq.filter (fun v -> graph.ContainsVertex(v) = false)
                |> Seq.iter (fun v -> Ops.Add(graph, v, ignoreErrors=false))

                other.Edges
                |> Seq.iter (
                    fun e ->
                        let tag =
                            match graph.TryGetEdge(e.Source, e.Target) with
                            | false, _  -> e.Tag
                            | true, eg  -> merge eg.Tag e.Tag

                        let edge = TaggedUndirectedEdge(e.Source, e.Target, tag)
                        Ops.Add(graph, edge, createVertices=false, ignoreErrors=false)
                )

                graph

        static member inline MergeInto<'TVertex, 'TTag>
                                (graph : TDGraph<'TVertex, 'TTag>, other : TDGraph<'TVertex, 'TTag>,
                                 merge : 'TTag -> 'TTag -> 'TTag)
            =
                other.Vertices
                |> Seq.filter (fun v -> graph.ContainsVertex(v) = false)
                |> Seq.iter (fun v -> Ops.Add(graph, v, ignoreErrors=false))

                other.Edges
                |> Seq.iter (
                    fun e ->
                        let tag =
                            match graph.TryGetEdge(e.Source, e.Target) with
                            | false, _  -> e.Tag
                            | true, eg  -> merge eg.Tag e.Tag

                        let edge = TaggedEdge(e.Source, e.Target, tag)
                        Ops.Add(graph, edge, createVertices=false, ignoreErrors=false)
                )

                graph

        static member inline ToAdjacency<'TVertex>(graph : SUGraph<'TVertex>) =
            let directed = SDGraph<'TVertex>()
            graph.Vertices
            |> Seq.iter (fun v -> Ops.Add(directed, v, ignoreErrors=false))

            graph.Edges
            |> Seq.iter (
                fun edge ->
                    Ops.Add(directed, edge.Source, edge.Target, createVertices=false, ignoreErrors=false)
            )

        static member inline ToAdjacency<'TVertex, 'TTag>(graph : TUGraph<'TVertex, 'TTag>) =
            let directed = TDGraph<'TVertex, 'TTag>()
            graph.Vertices
            |> Seq.iter (fun v -> Ops.Add(directed, v, ignoreErrors=false))

            graph.Edges
            |> Seq.iter (
                fun edge ->
                    Ops.Add(directed, edge.Source, edge.Target, edge.Tag, createVertices=false, ignoreErrors=false)
            )

    type Comparison =
        /// <summary>
        /// Checks whether two graphs are exactly identical (not only in shape, but also in vertex labels).
        /// </summary>
        /// <param name="graph1">The first graph to compare for equality</param>
        /// <param name="graph2">The second graph to compare for equality</param>
        static member inline StructurallyEqual<'TVertex> (graph1: SUGraph<'TVertex>, graph2: SUGraph<'TVertex>) =
            let compareEdges () =
                graph1.Edges    |> Seq.forall (fun v -> graph2.ContainsEdge(v))
            let compareVertices () =
                graph1.Vertices |> Seq.forall (fun v -> graph2.ContainsVertex(v))

            graph1.VertexCount = graph2.VertexCount &&
            graph1.EdgeCount = graph2.EdgeCount &&
            compareVertices() &&
            compareEdges()

        static member inline StructurallyEqual<'TVertex> (graph1: SDGraph<'TVertex>, graph2: SDGraph<'TVertex>) =
            let compareEdges () =
                graph1.Edges    |> Seq.forall (fun v -> graph2.ContainsEdge(v))
            let compareVertices () =
                graph1.Vertices |> Seq.forall (fun v -> graph2.ContainsVertex(v))

            graph1.VertexCount = graph2.VertexCount &&
            graph1.EdgeCount = graph2.EdgeCount &&
            compareVertices() &&
            compareEdges()

        static member inline StructurallyEqual<'TVertex, 'TTag when 'TTag: equality> (graph1: TUGraph<'TVertex, 'TTag>, graph2: TUGraph<'TVertex, 'TTag>) =
            let compareEdges () =
                graph1.Edges
                |> Seq.forall (
                    fun e ->
                        match graph2.TryGetEdge(e.Source, e.Target) with
                        | true, edge    -> edge.Tag = e.Tag
                        | false, _      -> false
                )
            let compareVertices () =
                graph1.Vertices |> Seq.forall (fun v -> graph2.ContainsVertex(v))

            graph1.VertexCount = graph2.VertexCount &&
            graph1.EdgeCount = graph2.EdgeCount &&
            compareVertices() &&
            compareEdges()

    type Mk =
        static member Line nodes =
            let graph = SUGraph<int>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                if i > 1 then Ops.Add(graph, i-1, i, createVertices=false)

            graph

        static member Line<'TWeight> (nodes, weight : int -> int -> 'TWeight) =
            let graph = TUGraph<int, 'TWeight>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                let w = weight (i-1) i
                if i > 1 then Ops.Add(graph, i-1, i, w, createVertices=false)

            graph

        static member LineSimple<'TWeight> (nodes, weight : 'TWeight) =
            let graph = TUGraph<int, 'TWeight>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                if i > 1 then Ops.Add(graph, i-1, i, weight, createVertices=false)

            graph

        static member Circle nodes =
            let graph = SUGraph<int>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                if i > 1 then Ops.Add(graph, i-1, i, createVertices=false)

            Ops.Add(graph, nodes, 1, createVertices=false)

            graph

        static member Circle<'TWeight> (nodes, weight : int -> int -> 'TWeight) =
            let graph = TUGraph<int, 'TWeight>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                let w = weight (i-1) i
                if i > 1 then Ops.Add(graph, i-1, i, w, createVertices=false)

            let w = weight nodes 1
            Ops.Add(graph, nodes, 1, w, createVertices=false)
            graph

        static member CircleSimple<'TWeight> (nodes, weight : 'TWeight) =
            let graph = TUGraph<int, 'TWeight>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                if i > 1 then Ops.Add(graph, i-1, i, weight, createVertices=false)

            Ops.Add(graph, nodes, 1, weight, createVertices=false)
            graph

        static member inline Clone<'TVertex> (graph : SUGraph<'TVertex>) =
            let graph' = SUGraph<'TVertex>()
            graph.Vertices |> Seq.iter (fun v -> Ops.Add(graph', v, ignoreErrors=false))
            graph.Edges |> Seq.iter (fun e -> Ops.Add(graph', e.Source, e.Target, createVertices=false, ignoreErrors=false))
            graph'

        static member inline Clone<'TVertex, 'TTag when 'TTag : struct> (graph : TUGraph<'TVertex, 'TTag>) =
            let graph' = TUGraph<'TVertex, 'TTag>()
            graph.Vertices |> Seq.iter (fun v -> Ops.Add(graph', v, ignoreErrors=false))
            graph.Edges |> Seq.iter (fun e -> Ops.Add(graph', e.Source, e.Target, e.Tag, createVertices=false, ignoreErrors=false))
            graph'

        static member inline Clone<'TVertex, 'TTag, 'TTag2> (graph : TUGraph<'TVertex, 'TTag>, mkTag : 'TTag -> 'TTag2) =
            let graph' = TUGraph<'TVertex, 'TTag2>()
            graph.Vertices |> Seq.iter (fun v -> Ops.Add(graph', v, ignoreErrors=false))
            graph.Edges
            |> Seq.iter (
                fun e ->
                    let tag = mkTag e.Tag
                    Ops.Add(graph', e.Source, e.Target, tag, createVertices=false, ignoreErrors=false)
            )
            graph'

    type Test =
        static member inline IsConnected<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UGraph<'TVertex, 'TEdge>) =
            let sp = Algorithms.ConnectedComponents.ConnectedComponentsAlgorithm(graph)
            sp.Compute()
            sp.ComponentCount = 1
