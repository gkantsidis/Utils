namespace CGFSHelper

/// Helper methods and data structures for primitive arithmetic types
module Arithmetic =
    open System

    module Decimal =
        /// Used mostly for debugging decimal numbers. It exposes the internal representation of a decimal
        let AsInternalString (v : decimal) =
            let parts = Decimal.GetBits(v)
            let isNegative = (parts.[3] &&& 0x80000000) <> 0
            let scale = ((parts.[3] >>> 16) &&& 0x7F)
            String.Format(
                "{0,10:X8}{1,10:X8}{2,10:X8}{3,10:X8}\tNegative:{4}\tScale:{5}",
                parts.[3], parts.[2], parts.[1], parts.[0], isNegative, scale
            )
