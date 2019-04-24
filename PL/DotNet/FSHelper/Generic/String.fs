namespace CGFSHelper

/// Extensions and helper methods for strings.
module String =
    open System

    /// <summary>
    /// Case insensitive comparison of two strings
    /// </summary>
    /// <param name="left">The first string</param>
    /// <param name="right">The second string</param>
    let iequals left right = String.Equals(left, right, StringComparison.InvariantCultureIgnoreCase)

    /// <summary>
    /// Checks whether a string ends with the given suffix.
    /// The comparison is case insensitive.
    /// </summary>
    /// <param name="suffix">The suffix</param>
    /// <param name="str">The input string</param>
    let iEndsWith suffix (str : string) =
        if isNull str && isNull suffix then true
        elif isNull str then false
        else str.EndsWith(suffix, StringComparison.InvariantCultureIgnoreCase)

    /// <summary>
    /// Checks whether a string starts with the given prefix.
    /// The comparison is case insensitive.
    /// </summary>
    /// <param name="prefix">The prefix</param>
    /// <param name="str">The input string</param>
    let iStartsWith prefix (str : string) =
        if isNull str && isNull prefix then true
        elif isNull str then false
        else str.StartsWith(prefix, StringComparison.InvariantCultureIgnoreCase)

    /// Convert a string to option type; empty strings are converted to None
    let toOptional input = if System.String.IsNullOrWhiteSpace(input) then None else Some input
