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

    /// In-place operations on graphs

    type Ops =
        static member inline private AddVertex<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UndirectedGraph<'TVertex, 'TEdge>, vertex : 'TVertex, ignoreErrors) =
            if graph.AddVertex(vertex) = false then
                if ignoreErrors then
                    warn "Failed to add vertex %A [ignoring]" vertex
                else
                    error "Failed to add vertex"

        static member inline private CheckAndAddVertex<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UndirectedGraph<'TVertex, 'TEdge>, vertex : 'TVertex, ignoreErrors) =
            if graph.ContainsVertex(vertex) = false then
                if graph.AddVertex(vertex) = false then
                    if ignoreErrors then
                        warn "Failed to add vertex %A [ignoring]" vertex
                    else
                        error "Failed to add vertex"

        static member inline private AddEdge<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UndirectedGraph<'TVertex, 'TEdge>, edge : 'TEdge, ignoreErrors) =
            if graph.AddEdge(edge) = false then
                if ignoreErrors then
                    warn "Failed to add edge %A [ignoring]" edge
                else
                    error "Failed to add edge"

        static member Add<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UndirectedGraph<'TVertex, 'TEdge>, vertex : 'TVertex, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            Ops.AddVertex(graph, vertex, ignoreErrors)

        static member Add<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UndirectedGraph<'TVertex, 'TEdge>, edge : 'TEdge, ?createVertices, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            let createVertices = defaultArg createVertices true

            if createVertices then
                Ops.CheckAndAddVertex(graph, edge.Source, ignoreErrors)
                Ops.CheckAndAddVertex(graph, edge.Target, ignoreErrors)

            Ops.AddEdge(graph, edge, ignoreErrors)

        static member Add<'TVertex> (graph : UndirectedGraph<'TVertex, UndirectedEdge<'TVertex>>, source : 'TVertex, target : 'TVertex, ?createVertices, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            let createVertices = defaultArg createVertices true

            if createVertices then
                Ops.CheckAndAddVertex(graph, source, ignoreErrors)
                Ops.CheckAndAddVertex(graph, target, ignoreErrors)

            let edge = UndirectedEdge<'TVertex>(source, target)
            Ops.AddEdge(graph, edge, ignoreErrors)

        static member Add<'TVertex, 'TTag> (graph : UndirectedGraph<'TVertex, TaggedUndirectedEdge<'TVertex, 'TTag>>, source : 'TVertex, target : 'TVertex, tag : 'TTag, ?createVertices, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            let createVertices = defaultArg createVertices true

            if createVertices then
                Ops.CheckAndAddVertex(graph, source, ignoreErrors)
                Ops.CheckAndAddVertex(graph, target, ignoreErrors)

            let edge = TaggedUndirectedEdge(source, target, tag)
            Ops.AddEdge(graph, edge, ignoreErrors)

        static member Remove<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UndirectedGraph<'TVertex, 'TEdge>, vertex : 'TVertex, ?ignoreErrors) =
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

        static member Remove<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UndirectedGraph<'TVertex, 'TEdge>, edge : 'TEdge, ?ignoreErrors) =
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

    type Transform =
        static member inline Map<'TVertex, 'TVertex2>
                                (graph : UndirectedGraph<'TVertex, UndirectedEdge<'TVertex>>,
                                 vertexMap : 'TVertex -> 'TVertex2)
                                : UndirectedGraph<'TVertex2, UndirectedEdge<'TVertex2>>
            =
                let graph' = UndirectedGraph<'TVertex2, UndirectedEdge<'TVertex2>>(graph.AllowParallelEdges)

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

        static member inline Map<'TVertex, 'TVertex2, 'TTag, 'TTag2>
                                (graph : UndirectedGraph<'TVertex, TaggedUndirectedEdge<'TVertex, 'TTag>>,
                                 vertexMap : 'TVertex -> 'TVertex2,
                                 tagMap : 'TTag -> 'TTag2)
                                : UndirectedGraph<'TVertex2, TaggedUndirectedEdge<'TVertex2, 'TTag2>>
            =
                let graph' = UndirectedGraph<'TVertex2, TaggedUndirectedEdge<'TVertex2, 'TTag2>>(graph.AllowParallelEdges)

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

        /// <summary>
        /// Merges the second graph into the first.
        /// A
        /// </summary>
        /// <param name="graph"></param>
        /// <param name="other"></param>
        static member inline MergeInto<'TVertex>
                                (graph : UndirectedGraph<'TVertex, UndirectedEdge<'TVertex>>,
                                 other : UndirectedGraph<'TVertex, UndirectedEdge<'TVertex>>)
            =
                other.Vertices
                |> Seq.filter (fun v -> graph.ContainsVertex(v) = false)
                |> Seq.iter (fun v -> Ops.Add(graph, v, ignoreErrors=false))

                other.Edges
                |> Seq.filter (fun e -> graph.ContainsEdge(e.Source, e.Target) = false)
                |> Seq.iter (fun e -> Ops.Add(graph, e.Source, e.Target, createVertices=false, ignoreErrors=false))

                graph

        static member inline MergeInto<'TVertex, 'TTag>
                                (graph : UndirectedGraph<'TVertex, TaggedUndirectedEdge<'TVertex, 'TTag>>,
                                 other : UndirectedGraph<'TVertex, TaggedUndirectedEdge<'TVertex, 'TTag>>,
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



    /// Undirected graph with nodes of integer identity and edges with decimal weights
    type UIDGraph = UndirectedGraph<int, TaggedEdge<int, decimal>>
    /// Undirected graph with nodes of integer identity and edges with double weights
    type UIFGraph = UndirectedGraph<int, TaggedEdge<int, float>>

    type Mk =
        static member Line nodes =
            let graph = UndirectedGraph<int, UndirectedEdge<int>>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                if i > 1 then Ops.Add(graph, i-1, i, createVertices=false)

            graph

        static member Line<'TWeight> (nodes, weight : int -> int -> 'TWeight) =
            let graph = UndirectedGraph<int, TaggedUndirectedEdge<int, 'TWeight>>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                let w = weight (i-1) i
                if i > 1 then Ops.Add(graph, i-1, i, w, createVertices=false)

            graph

        static member LineSimple<'TWeight> (nodes, weight : 'TWeight) =
            let graph = UndirectedGraph<int, TaggedUndirectedEdge<int, 'TWeight>>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                if i > 1 then Ops.Add(graph, i-1, i, weight, createVertices=false)

            graph

        static member Circle nodes =
            let graph = UndirectedGraph<int, UndirectedEdge<int>>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                if i > 1 then Ops.Add(graph, i-1, i, createVertices=false)

            Ops.Add(graph, nodes, 1, createVertices=false)

            graph

        static member Circle<'TWeight> (nodes, weight : int -> int -> 'TWeight) =
            let graph = UndirectedGraph<int, TaggedUndirectedEdge<int, 'TWeight>>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                let w = weight (i-1) i
                if i > 1 then Ops.Add(graph, i-1, i, w, createVertices=false)

            let w = weight nodes 1
            Ops.Add(graph, nodes, 1, w, createVertices=false)
            graph

        static member CircleSimple<'TWeight> (nodes, weight : 'TWeight) =
            let graph = UndirectedGraph<int, TaggedUndirectedEdge<int, 'TWeight>>(false)
            for i = 1 to nodes do
                Ops.Add(graph, i, false)
                if i > 1 then Ops.Add(graph, i-1, i, weight, createVertices=false)

            Ops.Add(graph, nodes, 1, weight, createVertices=false)
            graph

        static member inline Clone<'TVertex> (graph : UndirectedGraph<'TVertex, UndirectedEdge<'TVertex>>) =
            let graph' = UndirectedGraph()
            graph.Vertices |> Seq.iter (fun v -> Ops.Add(graph', v, ignoreErrors=false))
            graph.Edges |> Seq.iter (fun e -> Ops.Add(graph', e.Source, e.Target, createVertices=false, ignoreErrors=false))
            graph'

        static member inline Clone<'TVertex, 'TTag when 'TTag : struct> (graph : UndirectedGraph<'TVertex, TaggedUndirectedEdge<'TVertex, 'TTag>>) =
            let graph' = UndirectedGraph()
            graph.Vertices |> Seq.iter (fun v -> Ops.Add(graph', v, ignoreErrors=false))
            graph.Edges |> Seq.iter (fun e -> Ops.Add(graph', e.Source, e.Target, e.Tag, createVertices=false, ignoreErrors=false))
            graph'

        static member inline Clone<'TVertex, 'TTag, 'TTag2> (graph : UndirectedGraph<'TVertex, TaggedUndirectedEdge<'TVertex, 'TTag>>, mkTag : 'TTag -> 'TTag2) =
            let graph' = UndirectedGraph()
            graph.Vertices |> Seq.iter (fun v -> Ops.Add(graph', v, ignoreErrors=false))
            graph.Edges
            |> Seq.iter (
                fun e ->
                    let tag = mkTag e.Tag
                    Ops.Add(graph', e.Source, e.Target, tag, createVertices=false, ignoreErrors=false)
            )
            graph'

    type Test =
        static member inline IsConnected<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : UndirectedGraph<'TVertex, 'TEdge>) =
            let sp = Algorithms.ConnectedComponents.ConnectedComponentsAlgorithm(graph)
            sp.Compute()
            sp.ComponentCount = 1
