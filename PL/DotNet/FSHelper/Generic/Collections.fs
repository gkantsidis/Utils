namespace CGFSHelper

/// Extensions and helper methods for some of the built-in collection types.
module Collections =
    open System

    module Comparison =
        /// <summary>
        /// Compares two elements using multiple functions to break ties
        /// </summary>
        /// <param name="fn">List of functions to use for comparison</param>
        /// <param name="item1">The left item to use for comparison</param>
        /// <param name="item2">The right item to use for comparison</param>
        let compareMany<'T> (fn : ('T * 'T -> int) list) (item1 : 'T, item2 : 'T) =
            let rec compare (work : ('T * 'T -> int) list) =
                match work with
                | []        -> 0
                | hd :: tl  ->
                    match hd (item1, item2) with
                    | 0     -> compare tl
                    | v     -> v

            compare fn

    /// Extensions to the Nullable type
    module Nullable =
        let inline map<'T1, 'T2 when
                                            'T1 : (new : unit -> 'T1) and 'T1 : struct and 'T1 :> ValueType
                                        and 'T2 : (new : unit -> 'T2) and 'T2 : struct and 'T2 :> ValueType>
                            (fn : 'T1 -> 'T2) (v : Nullable<'T1>)
                            : Nullable<'T2>
            =
                if v.HasValue then Nullable(fn v.Value)
                else Nullable()

    /// Extensions for lists.
    module List =

        /// <summary>
        /// Folds over the cross product of two lists
        /// </summary>
        /// <param name="folder">The folder function</param>
        /// <param name="initial">Initial state</param>
        /// <param name="list1">First list</param>
        /// <param name="list2">Second list</param>
        let crossFold (folder : 'State -> 'T1 -> 'T2 -> 'State) (initial : 'State) (list1 : 'T1 list) (list2 : 'T2 list) =
            let mutable s = initial

            for i = 0 to (list1.Length - 1) do
                for j = 0 to (list2.Length - 1) do
                    let item1 = list1.Item i
                    let item2 = list2.Item j
                    s <- folder s item1 item2

            s

        /// <summary>
        /// Generates all unique pairs of items from a list.
        /// (The order in the pair is not important.)
        /// </summary>
        /// <param name="list">The input list</param>
        let uniquePairs (list : 'T1 list) =
            seq {
                for i = 0 to (list.Length - 1) do
                    for j = (i+1) to (list.Length - 1) do
                        yield (List.item i list, List.item j list)
            }
            |> Seq.toList

        /// <summary>
        /// Generates all unique pairs of items from a list.
        /// (The order in the pair is not important.)
        /// </summary>
        /// <param name="list">The input list</param>
        let uniquePairsi (list : 'T1 list) =
            seq {
                for i = 0 to (list.Length - 1) do
                    for j = (i+1) to (list.Length - 1) do
                        yield (i, List.item i list, j, List.item j list)
            }
            |> Seq.toList

        /// <summary>
        /// For all pairs of items from two lists, compute a function on them
        /// and return the minimum value.
        /// </summary>
        /// <param name="f">The function to compute on the items</param>
        /// <param name="list1">First list</param>
        /// <param name="list2">Second list</param>
        let crossMinBy (f : 'T1 -> 'T2 -> 'T) (list1 : 'T1 list) (list2 : 'T2 list) =
            let mutable minimum : 'T option = None

            for i = 0 to (list1.Length - 1) do
                for j = 0 to (list2.Length - 1) do
                    let item1 = list1.Item i
                    let item2 = list2.Item j
                    let value = f item1 item2
                    if minimum.IsNone
                    then minimum <- Some value
                    else minimum <- min minimum.Value value |> Some

            // If any list is empty, the following should throw an exception
            minimum.Value

        /// <summary>
        /// Generate a new list, where each new element is pairs with its predecessor.
        /// (The predecessor of the first element is None.)
        /// </summary>
        /// <param name="list">The input list</param>
        let withPrevious (list : 'T list) =
            match list with
            | []        -> []
            | hd :: tl  ->
                (None, hd) :: (tl |> List.pairwise |> List.map (fun (previous, current) -> (Some previous, current)))

        /// <summary>
        /// Apply a filter on a list, but also include in the filtering function the value of the predecessor
        /// in the input list (the predecessor for the first item is None).
        /// </summary>
        /// <param name="fn">The filtering function</param>
        /// <param name="list">The input list</param>
        let filterWithPrevious (fn : 'T option * 'T -> bool) (list : 'T list) =
            list |> withPrevious |> List.filter fn |> List.map snd
