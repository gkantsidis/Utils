namespace CGFSHelper

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

