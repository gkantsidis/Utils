namespace CGFSHelper

/// Helper methods for running methods repeatively.
module Runner =
    /// <summary>
    /// Run a function a given number of iterations.
    /// (Typically, the function will change some form of state, or you just want to time it.)
    /// </summary>
    /// <param name="iterations">The number of iterations</param>
    /// <param name="fn">The function to run</param>
    let repeat iterations (fn : unit -> unit) =
        for i = 0 to (iterations - 1) do
            fn ()

    /// <summary>
    /// Run a function a given number of iterations, and collect the results to a list.
    /// (Typically, the function will change some form of state.)
    /// </summary>
    /// <param name="iterations">The number of iterations</param>
    /// <param name="fn">The function to run</param>
    let repeatT<'T> iterations (fn : unit -> 'T) = List.init iterations (fun _ -> fn ())

    /// <summary>
    /// Run a boolean function multiple times, and count the number of times the function returns true.
    /// (Typically, the function will change some form of state.)
    /// </summary>
    /// <param name="iterations">The number of iterations</param>
    /// <param name="fn">The function to run</param>
    let countSuccess<'T> iterations (fn : unit -> bool) =
        List.init iterations (fun _ -> fn ())
        |> List.fold (fun (success, fail) result -> if result then (success+1, fail) else (success, fail+1)) (0, 0)

    /// <summary>
    /// Run a function that returns option multiple times.
    /// It returns the number of times that the function returned a value, the number of times it returned None,
    /// as well as all proper values generated.
    /// (Typically, the function will change some form of state.)
    /// </summary>
    /// <param name="iterations"></param>
    /// <param name="fn"></param>
    let countSuccessT<'T> iterations (fn : unit -> 'T option) =
        let result = List.init iterations (fun _ -> fn ()) |> List.choose id
        let success = result.Length
        (success, iterations - success), result
