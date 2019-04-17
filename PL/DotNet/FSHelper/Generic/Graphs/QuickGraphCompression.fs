namespace CGFSHelper.Graphs

/// Algorithms to transform graphs with the goal of reducing their size
module QuickGraphCompression =
    open QuickGraph

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
    /// Captures a set of edges that will be considered as one in the aggregated graph.
    /// (Assumes undirected graph.)
    /// </summary>
    type CollapsedEdgeInfo<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> =
    /// The aggregate edge represents just a single edge
    | Simple        of 'TEdge
    /// The aggregate edge represents a set of sequential edges
    | Sequential    of 'TEdge list
    /// The aggregate edge represents a parallel set of sequential edges
    | Parallel      of ('TEdge list) list
    with
        static member Make (edge : 'TEdge) = Simple edge
        static member Make (edge : 'TEdge list) =
            match edge with
            | []            -> throw "Empty list of edges"
            | single :: []  -> Simple single
            | _             -> Sequential edge

        member this.IsSimpleEdge =
            match this with
            | Simple _  -> true
            | _         -> false

        member this.IsCollapsedEdge =
            match this with
            | Sequential _    -> true
            | _             -> false

        member this.IsParallelEdge =
            match this with
            | Parallel _    -> true
            | _             -> false

        member this.GetSimpleEdge() =
            match this with
            | Simple edge   -> edge
            | _             -> throw "Attempting to access info assuming a simple edge; it is %A" this

        member this.Simplify() =
            let rec simplify (current : CollapsedEdgeInfo<'TVertex, 'TEdge>) =
                match current with
                | Simple _              -> current
                | Sequential []         -> throw "Encountered aggregate edge with empty set of edges"
                | Sequential (hd :: []) -> Simple hd
                | Sequential _          -> current
                | Parallel []           -> throw "Encountered aggregate edge with empty set of edges"
                | Parallel (hd :: [])   ->
                    let c = Sequential hd
                    simplify c
                | Parallel _            -> current

            simplify this

        member this.Extend (other : CollapsedEdgeInfo<'TVertex, 'TEdge>) =
            match this, other with
            | Simple t, Simple o        -> Parallel [[t]; [o]]
            | Simple t, Sequential o    -> Parallel [[t]; o]
            | Simple t, Parallel o      -> Parallel ([t] :: o)
            | Sequential t, Simple o    -> Parallel [t; [o]]
            | Sequential t, Sequential o-> Parallel [t; o]
            | Sequential t, Parallel o  -> Parallel (t :: o)
            | Parallel t, Simple o      -> Parallel (t @ [[o]])
            | Parallel t, Sequential o  -> Parallel (t @ [o])
            | Parallel t, Parallel o    -> Parallel (t @ o)

        static member Linearize (info : CollapsedEdgeInfo<'TVertex, 'TEdge> list) =
            let rec make worklist current =
                match worklist with
                | []                        -> Sequential current
                | (Simple info) :: tl       -> make tl (current @ [info])
                | (Sequential info) :: tl   -> make tl (current @ info)
                | (Parallel _) :: _         ->
                    let str = info |> List.map (sprintf "(%A)") |> String.concat ","
                    debug "Error when trying to linearize: %A" str
                    throw "Cannot make linear aggregate info from parallel edges"

            match info with
            | []        -> throw "Cannot make aggregate info edge from empty list"
            | hd :: []  -> hd
            | _         -> make info []

    let (|Sequential|_|) = function
    | Sequential edges  -> Some edges
    | _                 -> None

    let (|ParallelEdge|_|) = function
    | Parallel edges    -> Some edges
    | _                 -> None

    let (|SimpleEdge|_|) = function
    | Simple edge       -> Some edge
    | _                 -> None

    /// <summary>
    /// Type for collapsed edges (assumes undirected graph)
    /// </summary>
    type CollapsedEdge<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> =
        TaggedUndirectedEdge<'TVertex, CollapsedEdgeInfo<'TVertex, 'TEdge>>

    /// <summary>
    /// Type for collapsed graph (assumes undirected graph)
    /// </summary>
    type CollapsedGraph<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> =
        UndirectedGraph<'TVertex, CollapsedEdge<'TVertex, 'TEdge>>

    type ICollapsedGraph<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> =
        IMutableUndirectedGraph<'TVertex, CollapsedEdge<'TVertex, 'TEdge>>

    /// <summary>
    /// Create a collapsed graph from an undirected graph
    /// </summary>
    /// <param name="graph"></param>
    let InitializeCollapsed<'TVertex, 'TEdge when 'TEdge :> IEdge<'TVertex>> (graph : IUGraph<'TVertex, 'TEdge>) =
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

    /// <summary>
    /// Creates a collapsed graph by eliminating (i.e. joining together) vertices of degree 2.
    /// However, to keep the structure of the graph, the algorithm retains degree 2 vertices when detecting cycles.
    /// For cycles one or two vertices will be retained to maintain the structure of the graph.
    /// </summary>
    /// <param name="graph">The input (undirected) graph</param>
    let CollapseDegree2InPlace<'TVertex, 'TEdge when 'TVertex : equality and 'TEdge :> IEdge<'TVertex>>
            (graph : ICollapsedGraph<'TVertex, 'TEdge>)
        =
            let candidates =
                graph.Vertices
                |> Seq.filter (fun v -> graph.AdjacentDegree(v) = 2)
                |> Seq.toList

            debug "Found %d candidates" candidates.Length

            let hasAdjacentParallelEdge (vertex : 'TVertex) =
                let noParallelPresent =
                    graph.AdjacentEdges(vertex)
                    |> Seq.forall(fun edge -> edge.Tag.IsParallelEdge = false)
                noParallelPresent = false

            let rec simplify (work : 'TVertex list) =
                match work with
                | []    -> ()
                | hd :: tl when graph.ContainsVertex(hd) = false    ->
                    // Vertex has been simplified and removed from graph before
                    simplify tl
                | hd :: tl when graph.AdjacentDegree(hd) <> 2       ->
                    // The vertex changed degree due to a simplification (from a circle)
                    simplify tl
                | hd :: tl when hasAdjacentParallelEdge(hd)         ->
                    // The vertex has degree two, but one of its edges is parallel
                    simplify tl
                | hd :: tl ->
                    // The vertex is still active and still has degree 2
                    debug "Examining node %A" hd

                    let rec search (current : 'TVertex) (edge : CollapsedEdge<'TVertex, 'TEdge>) path =
                        let other = edge.GetOtherVertex(current)
                        if graph.AdjacentDegree(other) <> 2 || other = hd   then other, edge :: path
                        elif hasAdjacentParallelEdge other                  then other, edge :: path
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

                    let findCommon (edge1 : CollapsedEdge<'TVertex, 'TEdge>) (edge2 : CollapsedEdge<'TVertex, 'TEdge>) =
                        if edge1.Source = edge2.Source then edge1.Source
                        elif edge1.Source = edge2.Target then edge1.Source
                        elif edge1.Target = edge2.Source then edge1.Target
                        elif edge1.Target = edge2.Target then edge1.Target
                        else failwithf "Cannot find common"

                    if endpoint1 = endpoint2 && endpoint1 = hd then
                        // Found a circle, and there is no way out of the circle
                        debug "Detected circle from %A" hd

                        let midpoint = (List.length path1) / 2
                        let path1, path2 = List.splitAt midpoint path1
                        let endpoint1 = findCommon (List.head path1) (List.last path2)
                        let endpoint2 = findCommon (List.last path1) (List.head path2)

                        debug "Endoints: %A, %A" endpoint1 endpoint2
                        debug "Path 1: %A" path1
                        debug "Path 2: %A" path2

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
                        // Found a circle, which is connected to the rest of the graph
                        let path = path1 @ (List.rev path2)
                        let midpoint = List.length path / 2
                        let path1, path2 = List.splitAt midpoint path
                        let endpoint = endpoint1
                        let endpoint1 = findCommon (List.head path1) (List.last path2)
                        let endpoint2 = findCommon (List.last path1) (List.head path2)
                        assert(endpoint = endpoint1 || endpoint = endpoint2)

                        debug "Endpoints: %A, %A" endpoint1 endpoint2
                        debug "Path 1: %A" path1
                        debug "Path 2: %A" path2

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

    let CollapseDegree2<'TVertex, 'TEdge when 'TVertex : equality and 'TEdge :> IEdge<'TVertex>>
            (graph : IUGraph<'TVertex, 'TEdge>)
        : CollapsedGraph<'TVertex, 'TEdge>
        =
            let graph = InitializeCollapsed graph
            CollapseDegree2InPlace graph
            graph

    /// <summary>
    /// Restores the original graph from a compressed graph.
    /// </summary>
    /// <param name="graph">The input compressed graph</param>
    /// <param name="target">The output graph</param>
    let RestoreInPlace<'TVertex, 'TEdge when 'TVertex : equality and 'TEdge :> IEdge<'TVertex>>
            (graph : ICollapsedGraph<'TVertex, 'TEdge>,
             target : IUGraph<'TVertex, 'TEdge>)
        =
            let expandEdge (edge : 'TEdge) =
                Ops.Add(target, edge, createVertices=true, ignoreErrors=false)

            let expandTag (tag : CollapsedEdgeInfo<'TVertex, 'TEdge>) =
                match tag with
                | CollapsedEdgeInfo.Simple edge       -> expandEdge edge
                | CollapsedEdgeInfo.Sequential edges  -> edges |> List.iter expandEdge
                | CollapsedEdgeInfo.Parallel edges    -> edges |> List.collect id |> List.iter expandEdge

            let expand (edge : CollapsedEdge<'TVertex, 'TEdge>) = expandTag edge.Tag

            graph.Vertices |> Seq.iter (fun v -> Ops.Add(target, v, ignoreErrors = false))
            graph.Edges |> Seq.iter expand

    /// <summary>
    /// Restores the original graph from a compressed graph.
    /// </summary>
    /// <param name="graph">The input compressed graph</param>
    let Restore<'TVertex, 'TEdge when 'TVertex : equality and 'TEdge :> IEdge<'TVertex>> (graph : CollapsedGraph<'TVertex, 'TEdge>)
        : UGraph<'TVertex, 'TEdge>
        =
            let target = UGraph<'TVertex, 'TEdge>()
            RestoreInPlace(graph, target)
            target

     /// <summary>
     /// Simplify a collapsed graph by removing the collapsed edges, and transforming them to an edge tag
     /// </summary>
     /// <param name="map">Function to extract edge information from a collapsed edge</param>
     /// <param name="graph">The collapsed graph</param>
    let SimplifyInPlace<'TVertex, 'TEdge, 'TTag when 'TVertex : equality and 'TEdge :> IEdge<'TVertex>>
           (map : CollapsedEdgeInfo<'TVertex, 'TEdge> -> 'TTag)
           (graph : ICollapsedGraph<'TVertex, 'TEdge>, output: ITUGraph<'TVertex, 'TTag>)
         =
            graph.Vertices |> Seq.iter (fun v -> Ops.Add(output, v, ignoreErrors = false))
            graph.Edges
            |> Seq.iter (
                fun edge ->
                    let tag = map edge.Tag
                    let edge' = TaggedUndirectedEdge(edge.Source, edge.Target, tag)
                    Ops.Add(output, edge')
            )

    /// <summary>
    /// Simplify a collapsed graph by removing the collapsed edges, and transforming them to an edge tag
    /// </summary>
    /// <param name="map">Function to extract edge information from a collapsed edge</param>
    /// <param name="graph">The collapsed graph</param>
    let Simplify<'TVertex, 'TEdge, 'TTag when 'TVertex : equality and 'TEdge :> IEdge<'TVertex>>
        (map : CollapsedEdgeInfo<'TVertex, 'TEdge> -> 'TTag)
        (graph : ICollapsedGraph<'TVertex, 'TEdge>)
        : TUGraph<'TVertex, 'TTag>
        =
            let output = UndirectedGraph<'TVertex, TaggedUndirectedEdge<'TVertex, 'TTag>>()
            SimplifyInPlace map (graph, output)
            output
