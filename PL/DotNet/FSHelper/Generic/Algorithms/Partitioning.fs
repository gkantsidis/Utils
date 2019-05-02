namespace CGFSHelper.Algorithms

module Partitioning =
    open System
    open System.Collections.Generic

    open CGFSHelper.Collections

    type PartitioningException (message:string, ?innerException:exn) =
        inherit ApplicationException(
            message,
            match innerException with | Some ex -> ex | _ -> null)

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
            raise (PartitioningException message)
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
    /// An extensible algorithm for partitioning using k-means.
    /// This version is flexible w.r.t. the distance metric, types, etc, but not necessarily very performant.
    /// (I.e. it has not been tested on large data-sets)
    /// </summary>
    /// <param name="distance">Function to compute the distance from a point to a candidate center</param>
    /// <param name="center">Function to compute a candidate center given a set of points (e.g. the center of the points in most cases)</param>
    /// <param name="maxCount">Maximum number of iterations</param>
    /// <param name="min_improvement_ratio">Minimum progress required to continue searching. The progress is measured as the ratio of the reduction on global distance over the new distance.</param>
    /// <param name="initial">Initial choice of centers</param>
    /// <param name="points">Points to partition, i.e. assign to centers</param>
    /// <returns>It returns a tuple of the computed centers, the sum of distances to the centers, the assignment of points to centers</returns>
    let KMeans<'TPoint, 'TCenter when 'TCenter : equality> (distance : 'TPoint -> 'TCenter -> float, center : 'TPoint list -> 'TCenter) (maxCount: int, min_improvement_ratio: float) (initial: 'TCenter list) (points : 'TPoint list) =
        if List.length initial = 0  then invalidArg "initial"   "The set of initial centers cannot be empty"
        if List.length points = 0   then invalidArg "points"    "The set of points to partition cannot be empty"

        if List.allUnique initial = false then throw "Initial set of centers contains duplicates"

        let assign (candidates : 'TCenter list) =
            let assignments =
                points
                |> List.map (
                    fun point ->
                        let best = candidates |> List.minBy (fun c -> distance point c)
                        let d = distance point best
                        best, d
                )

            let total = assignments |> List.sumBy snd
            total, assignments |> List.map fst

        let rec search (iteration, total_distance) (candidates : 'TCenter list) =
            let new_total_distance, assignment = assign candidates

            let should_continue =
                let good_progress =
                    match total_distance with
                    | None      -> true
                    | Some td   ->
                        let improvement_ratio = (td - new_total_distance) / new_total_distance
                        debug "K-means improvement ratio in iteration %d is %6.4f" iteration improvement_ratio
                        improvement_ratio > min_improvement_ratio
                iteration <= maxCount && good_progress

            if should_continue = false then candidates, total_distance, (List.zip points assignment)
            else
                let groups = List.zip assignment points |> List.groupBy fst
                let new_assignments =
                    groups
                    |> List.map snd
                    |> List.map (
                        fun group ->
                            let points' = group |> List.map snd
                            center points'
                    )

                assert (List.length new_assignments = List.length points)
                search (iteration+1, Some new_total_distance) new_assignments

        let result = search (0, None) initial
        let (centers, _, _) = result
        assert(centers.Length = initial.Length)
        result

    /// <summary>
    /// Simple heuristic to partition a set of points by heuristically allocating points of the smaller distance
    /// to the same partition, until the required number of partitions has been constructed.
    /// The partitions are not balanced, and they may not be even good (no effort to maximize any global metric).
    /// However, they can be used as starting points for more advanved algorithms (e.g. k-means).
    /// </summary>
    /// <param name="distance">Function to compute the distance between two points</param>
    /// <param name="points">Set of points to partition</param>
    /// <param name="n">Number of partitions to generate</param>
    let PartitionByDistance<'T when 'T:equality> (distance : 'T -> 'T -> float) (points : 'T list) n =
        if List.length points < n then invalidArg "n" (sprintf "Number of required partitions (%d) less than the number of points (%d)" n (List.length points))
        elif List.length points = n then
            warn "Set of points equals to required number of partitions; no further processing"
            points |> List.map (fun v -> [v])
        else
            let assignment = Dictionary<'T, int>(points.Length)
            let parent = Dictionary<int, int>(points.Length)

            points
            |> List.iteri (
                fun i p ->
                    assignment.Add(p, i)
                    parent.Add(i, i)
            )

            let rec findParent i    = let j = parent.[i] in if i = j then i else findParent j
            let rec makeParent i p  =
                if i <> p then
                    let j = parent.[i]
                    parent.[i] <- p
                    makeParent j p

            let mutable current = 1

            let rec reduce remaining (pairs : ('T * 'T) list) =
                match remaining, pairs with
                | 0, _
                | _, []         -> ()
                | _, (a, b)::tl     ->
                    if assignment.[a] = assignment.[b] then
                        reduce remaining tl
                    else
                        let index1 = findParent assignment.[a]
                        let index2 = findParent assignment.[b]
                        if index1 = index2 then
                            makeParent assignment.[a] index1
                            makeParent assignment.[b] index1
                            assignment.[a] <- index1
                            assignment.[b] <- index1
                            reduce remaining tl
                        else
                            let parent = min index1 index2
                            makeParent assignment.[a] parent
                            makeParent assignment.[b] parent
                            assignment.[a] <- parent
                            assignment.[b] <- parent
                            reduce (remaining - 1) tl

            let sorted = List.uniquePairs points |> List.sortBy (fun (a, b) -> distance a b)
            reduce (assignment.Count - n) sorted

            points
            |> List.iter (
                fun p ->
                    match assignment.TryGetValue p with
                    | false, _  ->
                        assignment.Add(p, current)
                        parent.Add(current, current)
                        current <- current + 1
                    | true, parent  ->
                        let parent' = findParent parent
                        if parent <> parent' then assignment.[p] <- parent'
            )

            assignment
            |> Seq.groupBy (fun kv -> kv.Value)
            |> Seq.map snd
            |> Seq.map (fun v -> v |> Seq.map (fun kv -> kv.Key) |> Seq.toList)
            |> Seq.toList

