﻿namespace CGFSHelper

/// Extensions and helper methods for some of the built-in collection types.
module Collections =
    open System

    /// Helpers for comparisons
    module Comparison =
        /// <summary>
        /// Compares two elements using multiple functions to break ties
        /// </summary>
        /// <param name="fn">List of functions to use for comparison</param>
        /// <param name="item1">The left item to use for comparison</param>
        /// <param name="item2">The right item to use for comparison</param>
        let inline compareMany< ^T> (fn : (^T * ^T -> int) list) (item1 : ^T, item2 : ^T) =
            let rec compare (work : (^T * ^T -> int) list) =
                match work with
                | []        -> 0
                | hd :: tl  ->
                    match hd (item1, item2) with
                    | 0     -> compare tl
                    | v     -> v

            compare fn

        /// <summary>
        /// Orders two items into a pair, largest first.
        /// </summary>
        /// <param name="first">The first item to order</param>
        /// <param name="second">The second item to order</param>
        let inline pairByLargest< ^T when ^T : comparison> (first : ^T) (second : ^T) =
            if first > second
            then first, second
            else second, first

        /// <summary>
        /// Orders two items into a pair, smallest first.
        /// </summary>
        /// <param name="first">The first item to order</param>
        /// <param name="second">The second item to order</param>
        let inline pairBySmallest< ^T when ^T : comparison> (first : ^T) (second : ^T) =
            if first > second
            then second, first
            else first, second


    /// Extensions for lists.
    module List =
        open System.Collections
        open System.Collections.Generic

        /// <summary>
        /// Checks whether all elements of a list are unique, with respect to their default equality operator
        /// </summary>
        /// <param name="list">The input list</param>
        let allUnique<'T when 'T : equality> (list : 'T list) =
            let identities = HashSet<'T>()
            list
            |> List.forall (
                fun item ->
                    if identities.Contains item
                    then false
                    else identities.Add item
            )

        /// <summary>
        /// Folds over the cross product of two lists
        /// </summary>
        /// <param name="folder">The folder function</param>
        /// <param name="initial">Initial state</param>
        /// <param name="list1">First list</param>
        /// <param name="list2">Second list</param>
        let inline crossFold< ^T1, ^T2, ^State> (folder : ^State -> ^T1 -> ^T2 -> ^State) (initial : ^State) (list1 : ^T1 list) (list2 : ^T2 list) =
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
        let inline uniquePairs (input : 'T list) =
            seq {
                for i = 0 to (input.Length - 1) do
                    for j = (i+1) to (input.Length - 1) do
                        yield (List.item i input, List.item j input)
            }
            |> Seq.toList

        /// <summary>
        /// Generates all unique pairs of items from a list.
        /// (The order in the pair is not important.)
        /// </summary>
        /// <param name="list">The input list</param>
        let inline uniquePairsi< ^T> (list : ^T list) =
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
        let inline crossMinBy< ^T1, ^T2, ^T when ^T : comparison> (f : ^T1 -> ^T2 -> ^T) (list1 : ^T1 list) (list2 : ^T2 list) =
            let mutable minimum : ^T option = None

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
        let inline withPrevious< ^T> (list : ^T list) =
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
        let inline filterWithPrevious< ^T> (fn : ^T option * ^T -> bool) (list : ^T list) =
            list |> withPrevious |> List.filter fn |> List.map snd

        let inline ofDictionaryWithTypes< ^TKey, ^TValue> (enumerator : IDictionaryEnumerator) =
            let rec construct result =
                if enumerator.MoveNext() then
                    let key = enumerator.Key :?> ^TKey
                    let value = enumerator.Value :?> ^TValue

                    (key, value) :: result
                else
                    List.rev result

            construct []

        let inline ofDictionaryWithConverters< ^TKey, ^TValue> (key : obj -> ^TKey) (value : obj -> ^TValue) (enumerator : IDictionaryEnumerator) =
                let rec construct result =
                    if enumerator.MoveNext() then
                        let key = key enumerator.Key
                        let value = value enumerator.Value

                        (key, value) :: result
                    else
                        List.rev result

                construct []

        let last (list : 'T list) =
            match list with
            | []
            | _ :: []   -> List.head list
            | _         -> List.item (list.Length - 1) list

        /// <summary>
        /// Reduces a list to contain only unique items.
        /// (Unique w.r.t. the default equality function for the type.)
        /// </summary>
        /// <param name="list">The list of items to filter.</param>
        let unique<'T when 'T : equality> (list : 'T list) =
            let identities = HashSet<'T>()
            list
            |> List.choose (
                fun item ->
                    if identities.Contains item
                    then None
                    else
                        identities.Add item |> ignore
                        Some item
            )

        type Cast =
            static member inline ofDictionary< ^TKey, ^TValue> (enumerator : IDictionaryEnumerator) = ofDictionaryWithTypes enumerator
            static member inline ofDictionary< ^TKey, ^TValue> (enumerator : IDictionaryEnumerator, key : obj -> ^TKey, value : obj -> ^TValue) =
                ofDictionaryWithConverters key value enumerator

    /// Extensions for the Map collection
    module Map =
        open System.Collections

        /// Casts to Map from various system types
        type Cast =
            /// Create a map from an untyped dictionary
            static member inline ofDictionary< ^TKey, ^TValue when ^TKey : comparison> (enumerator : IDictionaryEnumerator) : Map< ^TKey, ^TValue> =
                List.ofDictionaryWithTypes(enumerator) |> Map.ofList

            static member inline ofDictionary< ^TKey, ^TValue when ^TKey : comparison> (enumerator : IDictionaryEnumerator, key : obj -> ^TKey, value : obj -> ^TValue) =
                List.ofDictionaryWithConverters key value enumerator

    /// Extensions to the Nullable type
    module Nullable =
        let inline map< ^T1, ^T2 when
                                            ^T1 : (new : unit -> ^T1) and ^T1 : struct and ^T1 :> ValueType
                                        and ^T2 : (new : unit -> ^T2) and ^T2 : struct and ^T2 :> ValueType>
                            (fn : ^T1 -> ^T2) (v : Nullable< ^T1>)
                            : Nullable< ^T2>
            =
                if v.HasValue then Nullable(fn v.Value)
                else Nullable()

    /// Extensions to the seq type
    module Seq =
        open System.Collections

        /// Casting between seq and other collections
        type Cast =
            /// Casting from an untyped dictionary enumerator
            static member inline ofDictionary< ^TKey, ^TValue> (enumerator : IDictionaryEnumerator) =
                seq {
                    while enumerator.MoveNext() do
                        let key = enumerator.Key :?> ^TKey
                        let value = enumerator.Value :?> ^TValue
                        yield (key, value)
                }

            /// Casting from an untyped dictionary enumerator, with user provided functions to map keys and values
            static member inline ofDictionary< ^TKey, ^TValue>(enumerator : IDictionaryEnumerator, key : obj -> ^TKey, value : obj -> ^TValue) =
                seq {
                    while enumerator.MoveNext() do
                        let key = key enumerator.Key
                        let value = value enumerator.Value
                        yield (key, value)
                }
