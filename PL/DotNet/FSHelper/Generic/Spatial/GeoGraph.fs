namespace CGFSHelper.Spatial

module GeoGraph =
    open System.Collections.Generic
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
    type SGeoGraph = {
        Graph       : SUGraph<GeoTag>
        Coordinates : Dictionary<GeoTag, Coordinates>
        Resolution  : int
    }
    with
        static member Make(resolution) = {
            Graph       = SUGraph<GeoTag>()
            Coordinates = Dictionary()
            Resolution  = resolution
        }

        static member Make(resolution, edges : (Coordinates * Coordinates) seq) =
            let graph = SGeoGraph.Make(resolution)
            edges
            |> Seq.iter (
                fun (source, target) ->
                    let src : GeoTag = graph.Index(source)
                    let dst : GeoTag = graph.Index(target)
                    Ops.Add(graph.Graph, src)
                    Ops.Add(graph.Graph, dst)
                    graph.AddEdge(src, dst)
            )

        member this.Index(position : Coordinates) =
            let bits = GeoHash.Make(position, this.Resolution)
            let tag = bits.AsTag.Value
            match this.Coordinates.TryGetValue(tag) with
            | false, _          -> this.Coordinates.Add(tag, position)
            | true, existing    ->
                if existing <> position then
                    let distance = position.DistanceTo(existing)
                    warn "Key already exists at distance %A" distance

            tag

        member this.IndexStrict(position : Coordinates) =
            let bits = GeoHash.Make(position, this.Resolution)
            let tag = bits.AsTag.Value
            match this.Coordinates.TryGetValue(tag) with
            | false, _          ->
                this.Coordinates.Add(tag, position)
                Some tag
            | true, existing    ->
                if existing <> position
                then None
                else Some tag

        member this.AddVertex(vertex : Coordinates) =
            let tag = this.Index(vertex)
            Ops.Add(this.Graph, tag)

        member this.AddVertex(vertex : GeoTag) =
            if this.Coordinates.ContainsKey(vertex) = false then
                error "Vertex %A not known" vertex

        member this.AddEdge(source : Coordinates, target : Coordinates, ?replaceIfExists) =
            let replaceIfExists = defaultArg replaceIfExists false
            let src = this.Index source
            let dst = this.Index target
            match this.Graph.TryGetEdge(src, dst) with
            | false, _      -> Ops.Add(this.Graph, src, dst)
            | true, edge    ->
                if replaceIfExists then
                    Ops.Remove(this.Graph, edge, ignoreErrors=false)
                    Ops.Add(this.Graph, src, dst)
                else
                    error "Edge %A<->%A already exists" src dst

        member this.AddEdge(source : GeoTag, target : GeoTag, ?replaceIfExists) =
            let replaceIfExists = defaultArg replaceIfExists false
            if this.Coordinates.ContainsKey(source) = false then
                error "Vertex %A not known" source
            if this.Coordinates.ContainsKey(target) = false then
                error "Vertex %A not known" target
            match this.Graph.TryGetEdge(source, target) with
            | false, _      -> Ops.Add(this.Graph, source, target)
            | true, edge    ->
                if replaceIfExists then
                    Ops.Remove(this.Graph, edge, ignoreErrors=false)
                    Ops.Add(this.Graph, source, target)
                else
                    error "Edge %A<->%A already exists" source target

        member this.RemoveEdge(source: GeoTag, target: GeoTag, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            Ops.Remove(this.Graph, source, target, ignoreErrors=ignoreErrors)

        member this.RemoveEdge(source : Coordinates, target : Coordinates, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            match this.IndexStrict source, this.IndexStrict target with
            | Some src, Some dst    -> Ops.Remove(this.Graph, src, dst, ignoreErrors=ignoreErrors)
            | _, _  ->
                let src = this.Index source
                let dst = this.Index target
                this.RemoveEdge(src, dst, ignoreErrors)

    /// <summary>
    /// Graph of geographical coordinates with edges between vertices (coordinate locations).
    /// Edges contain extra information.
    /// </summary>
    type GeoGraph<'TTag> = {
        Graph       : TUGraph<GeoTag, 'TTag>
        Coordinates : Dictionary<GeoTag, Coordinates>
        Resolution  : int
    }
    with
        static member Make(resolution) = {
            Graph       = TUGraph<GeoTag, 'TTag>()
            Coordinates = Dictionary()
            Resolution  = resolution
        }

        static member Make(resolution, edges : (Coordinates * Coordinates * 'TTag) seq) =
            let graph = GeoGraph.Make(resolution)
            edges
            |> Seq.iter (
                fun (source, target, info) ->
                    let src : GeoTag = graph.Index(source)
                    let dst : GeoTag = graph.Index(target)
                    Ops.Add(graph.Graph, src)
                    Ops.Add(graph.Graph, dst)
                    graph.AddEdge(src, dst, info)
            )

        static member Make(resolution, edges : (Coordinates * Coordinates) seq, info: 'TTag) =
            let graph = GeoGraph.Make(resolution)
            edges
            |> Seq.iter (
                fun (source, target) ->
                    let src : GeoTag = graph.Index(source)
                    let dst : GeoTag = graph.Index(target)
                    Ops.Add(graph.Graph, src)
                    Ops.Add(graph.Graph, dst)
                    graph.AddEdge(src, dst, info)
            )

        member this.Index(position : Coordinates) =
            let bits = GeoHash.Make(position, this.Resolution)
            let tag = bits.AsTag.Value
            match this.Coordinates.TryGetValue(tag) with
            | false, _          -> this.Coordinates.Add(tag, position)
            | true, existing    ->
                if existing <> position then
                    let distance = position.DistanceTo(existing)
                    warn "Key already exists at distance %A" distance

            tag

        member this.IndexStrict(position : Coordinates) =
            let bits = GeoHash.Make(position, this.Resolution)
            let tag = bits.AsTag.Value
            match this.Coordinates.TryGetValue(tag) with
            | false, _          ->
                this.Coordinates.Add(tag, position)
                Some tag
            | true, existing    ->
                if existing <> position
                then None
                else Some tag

        member this.AddVertex(vertex : Coordinates) =
            let tag = this.Index(vertex)
            Ops.Add(this.Graph, tag)

        member this.AddVertex(vertex : GeoTag) =
            if this.Coordinates.ContainsKey(vertex) = false then
                error "Vertex %A not known" vertex

        member this.AddEdge(source : Coordinates, target : Coordinates, info : 'TTag, ?replaceIfExists) =
            let replaceIfExists = defaultArg replaceIfExists false
            let src = this.Index source
            let dst = this.Index target
            match this.Graph.TryGetEdge(src, dst) with
            | false, _      -> Ops.Add(this.Graph, src, dst, info)
            | true, edge    ->
                if replaceIfExists then
                    Ops.Remove(this.Graph, edge, ignoreErrors=false)
                    Ops.Add(this.Graph, src, dst, info)
                else
                    error "Edge %A<->%A already exists" src dst

        member this.AddEdge(source : GeoTag, target : GeoTag, info : 'TTag, ?replaceIfExists) =
            let replaceIfExists = defaultArg replaceIfExists false
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

        member this.RemoveEdge(source: GeoTag, target: GeoTag, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            Ops.Remove(this.Graph, source, target, ignoreErrors=ignoreErrors)

        member this.RemoveEdge(source : Coordinates, target : Coordinates, ?ignoreErrors) =
            let ignoreErrors = defaultArg ignoreErrors true
            match this.IndexStrict source, this.IndexStrict target with
            | Some src, Some dst    -> Ops.Remove(this.Graph, src, dst, ignoreErrors=ignoreErrors)
            | _, _  ->
                let src = this.Index source
                let dst = this.Index target
                this.RemoveEdge(src, dst, ignoreErrors)
