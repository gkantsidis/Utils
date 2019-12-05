namespace CGFSHelper

/// Helper methods for two dimensional arrays
module Array2D =
    open System.Diagnostics.Contracts
    open System.Runtime.InteropServices

    let inline forall< ^T> (predicate: ^T -> bool) (array: ^T[,]): bool =
        let mutable i = array.GetLowerBound(0)
        let mutable j = array.GetLowerBound(1)
        let mutable is_predicate_valid = true

        while i <= array.GetUpperBound(0) && j <= array.GetUpperBound(1) && is_predicate_valid do
            let element = Array2D.get array i j
            is_predicate_valid <- predicate element

            j <- j + 1
            if j > array.GetUpperBound(1) then
                i <- i + 1
                j <- array.GetLowerBound(1)

        is_predicate_valid

    let inline foralli< ^T> (predicate: int -> int -> ^T -> bool) (array: ^T[,]): bool =
        let mutable i = array.GetLowerBound(0)
        let mutable j = array.GetLowerBound(1)
        let mutable is_predicate_valid = true

        while i <= array.GetUpperBound(0) && j <= array.GetUpperBound(1) && is_predicate_valid do
            let element = Array2D.get array i j
            is_predicate_valid <- predicate i j element

            j <- j + 1
            if j > array.GetUpperBound(1) then
                i <- i + 1
                j <- array.GetLowerBound(1)

        is_predicate_valid

    let inline exists< ^T> (predicate: ^T -> bool) (array: ^T[,]): bool =
        let mutable i = array.GetLowerBound(0)
        let mutable j = array.GetLowerBound(1)
        let mutable found = false

        while i <= array.GetUpperBound(0) && j <= array.GetUpperBound(1) && found = false do
            let element = Array2D.get array i j
            found <- predicate element

            j <- j + 1
            if j > array.GetUpperBound(1) then
                i <- i + 1
                j <- array.GetLowerBound(1)

        found

    let inline existsi< ^T> (predicate: int -> int -> ^T -> bool) (array: ^T[,]): bool =
        let mutable i = array.GetLowerBound(0)
        let mutable j = array.GetLowerBound(1)
        let mutable found = false

        while i <= array.GetUpperBound(0) && j <= array.GetUpperBound(1) && found = false do
            let element = Array2D.get array i j
            found <- predicate i j element

            j <- j + 1
            if j > array.GetUpperBound(1) then
                i <- i + 1
                j <- array.GetLowerBound(1)

        found

    let inline indices< ^T> (array: ^T[,]): (int * int) seq = seq {
        let mutable i = array.GetLowerBound(0)
        let mutable j = array.GetLowerBound(1)

        while i <= array.GetUpperBound(0) && j <= array.GetUpperBound(1) do
            yield (i, j)
            j <- j + 1
            if j > array.GetUpperBound(1) then
                i <- i + 1
                j <- array.GetLowerBound(1)
    }

    let inline fold< ^State, ^T> (folder: ^State -> ^T -> ^State) (state: ^State) (array: ^T[,]): ^State =
        let mutable i = array.GetLowerBound(0)
        let mutable j = array.GetLowerBound(1)
        let mutable state = state

        while i <= array.GetUpperBound(0) && j <= array.GetUpperBound(1) do
            let element = Array2D.get array i j
            state <- folder state element

            j <- j + 1
            if j > array.GetUpperBound(1) then
                i <- i + 1
                j <- array.GetLowerBound(1)

        state

    let inline sumBy< ^T, ^U
                        when ^U : (static member Zero   : ^U)
                         and ^U : (static member ( + )  : ^U -> ^U -> ^U)
                    >
                (projection : ^T -> ^U) (array: ^T[,]): ^U
        =
            // unclear why we need the following, but it does not compile otherwise
            let inline add (x: 'b) (y: 'b) : 'b = (+) x y

            let initial = LanguagePrimitives.GenericZero< ^U>
            fold (
                fun cumulative element ->
                    let value = projection element
                    add cumulative value
            ) initial array

    let inline sumRowBy< ^T, ^U
                        when ^U : (static member Zero   : ^U)
                         and ^U : (static member ( + )  : ^U -> ^U -> ^U)
                    >
                (projection : ^T -> ^U) (row: int) (array: ^T[,]): 'U
        =
            // unclear why we need the following, but it does not compile otherwise
            let inline add (x: 'b) (y: 'b) : 'b = (+) x y
            let get  = Array2D.get array

            let mutable cumulative = LanguagePrimitives.GenericZero< ^U>
            for j in array.GetLowerBound(1) .. array.GetUpperBound(1) do
                let value = projection (get row j)
                cumulative <- add cumulative value

            cumulative

    let inline sumColumnBy< ^T, ^U
                        when ^U : (static member Zero   : ^U)
                         and ^U : (static member ( + )  : ^U -> ^U -> ^U)
                    >
                (projection : ^T -> ^U) (column: int) (array: ^T[,]): ^U
        =
            // unclear why we need the following, but it does not compile otherwise
            let inline add (x: 'b) (y: 'b) : 'b = (+) x y
            let get  = Array2D.get array

            let mutable cumulative = LanguagePrimitives.GenericZero< ^U>
            for i in array.GetLowerBound(0) .. array.GetUpperBound(0) do
                let value = projection (get i column)
                cumulative <- add cumulative value

            cumulative

    let inline length< ^T> (array: ^T[,]): int = (Array2D.length1 array) * (Array2D.length2 array)

    let inline setRow< ^T> (row: int) (value: ^T) (array: ^T[,]): unit =
        for j in array.GetLowerBound(1) .. array.GetUpperBound(1) do
            Array2D.set array row j value

    let inline setColumn< ^T> (column: int) (value: ^T) (array: ^T[,]): unit =
        for i in array.GetLowerBound(0) .. array.GetUpperBound(0) do
            Array2D.set array i column value

    let inline zero< ^T when ^T : (static member Zero : ^T)> (array: ^T[,]): unit =
        let z = LanguagePrimitives.GenericZero< ^T>
        Array2D.iteri (fun i j _ -> Array2D.set array i j z) array

    let inline zeroRow< ^T when ^T : (static member Zero : ^T)> (row : int) (array: ^T[,]): unit =
        let z = LanguagePrimitives.GenericZero< ^T>
        setRow row z array

    let inline isSquare< ^T> (array: ^T[,]) : bool =
        Array2D.length1 array = Array2D.length2 array

    let inline isDiagonal< ^T when ^T : (static member Zero : ^T) and ^T : equality > (array: ^T[,]) : bool =
        let z = LanguagePrimitives.GenericZero< ^T>
        foralli (fun i j v -> i = j || v = z) array

    let zip2<'T1, 'T2> (arr1 : 'T1[,]) (arr2 : 'T2[,]) =
        if arr1.Length <> arr2.Length then
            raise (invalidArg "arr2" "Arrays do not agree in length")
        if arr1.GetLowerBound(0) <> arr2.GetLowerBound(0) then
            raise (invalidArg "arr2" "Arrays have different lower bounds in the first dimension")
        if arr1.GetLowerBound(1) <> arr2.GetLowerBound(1) then
            raise (invalidArg "arr2" "Arrays have different lower bounds in the second dimension")
        if arr1.GetUpperBound(0) <> arr2.GetUpperBound(0) then
            raise (invalidArg "arr2" "Arrays have different upper bounds in the first dimension")
        if arr1.GetUpperBound(1) <> arr2.GetUpperBound(1) then
            raise (invalidArg "arr2" "Arrays have different upper bounds in the second dimension")
        Contract.EndContractBlock()

        let upper0 = arr1.GetUpperBound(0) + 1
        let upper1 = arr1.GetUpperBound(1) + 1

        Array2D.init upper0 upper1 (fun i j -> arr1.[i, j], arr2.[i, j])

    let zip3<'T1, 'T2, 'T3> (arr1 : 'T1[,]) (arr2 : 'T2[,]) (arr3 : 'T3[,]) =
        if arr1.Length <> arr2.Length then
            raise (invalidArg "arr2" "Arrays do not agree in length")
        if arr1.Length <> arr3.Length then
            raise (invalidArg "arr3" "Arrays do not agree in length")
        if arr1.GetLowerBound(0) <> arr2.GetLowerBound(0) then
            raise (invalidArg "arr2" "Arrays have different lower bounds in the first dimension")
        if arr1.GetLowerBound(0) <> arr3.GetLowerBound(0) then
            raise (invalidArg "arr3" "Arrays have different lower bounds in the first dimension")
        if arr1.GetLowerBound(1) <> arr2.GetLowerBound(1) then
            raise (invalidArg "arr2" "Arrays have different lower bounds in the second dimension")
        if arr1.GetLowerBound(1) <> arr3.GetLowerBound(1) then
            raise (invalidArg "arr3" "Arrays have different lower bounds in the second dimension")
        if arr1.GetUpperBound(0) <> arr2.GetUpperBound(0) then
            raise (invalidArg "arr2" "Arrays have different upper bounds in the first dimension")
        if arr1.GetUpperBound(0) <> arr3.GetUpperBound(0) then
            raise (invalidArg "arr3" "Arrays have different upper bounds in the first dimension")
        if arr1.GetUpperBound(1) <> arr2.GetUpperBound(1) then
            raise (invalidArg "arr2" "Arrays have different upper bounds in the second dimension")
        if arr1.GetUpperBound(1) <> arr3.GetUpperBound(1) then
            raise (invalidArg "arr3" "Arrays have different upper bounds in the second dimension")
        Contract.EndContractBlock()

        let upper0 = arr1.GetUpperBound(0) + 1
        let upper1 = arr1.GetUpperBound(1) + 1

        Array2D.init upper0 upper1 (fun i j -> arr1.[i, j], arr2.[i, j], arr3.[i, j])

    let filter<'T> (fn : 'T -> bool) (arr : 'T[,]) =
        let low0 = arr.GetLowerBound(0) + 1
        let low1 = arr.GetLowerBound(1) + 1
        let upper0 = arr.GetUpperBound(0) + 1
        let upper1 = arr.GetUpperBound(1) + 1

        let rec find i j result =
            if i = upper0 then result
            elif j = upper1 then find (i+1) low1 result
            else
                let elem = arr.[i, j]
                if fn elem then find i (j + 1) (((i, j), elem) :: result)
                else find i (j + 1) result

        find 0 0 [] |> List.rev

    let filteri<'T> (fn : int -> int -> 'T -> bool) (arr : 'T[,]) =
        let low0 = arr.GetLowerBound(0) + 1
        let low1 = arr.GetLowerBound(1) + 1
        let upper0 = arr.GetUpperBound(0) + 1
        let upper1 = arr.GetUpperBound(1) + 1

        let rec find i j result =
            if i = upper0 then result
            elif j = upper1 then find (i+1) low1 result
            else
                let elem = arr.[i, j]
                if fn i j elem then find i (j + 1) (((i, j), elem) :: result)
                else find i (j + 1) result

        find 0 0 [] |> List.rev

    let filter2<'T1, 'T2> (fn : 'T1 -> 'T2 -> bool) (arr1 : 'T1[,]) (arr2 : 'T2[,]) =
        if arr1.Length <> arr2.Length then
            raise (invalidArg "arr2" "Arrays do not agree in length")
        if arr1.GetLowerBound(0) <> arr1.GetLowerBound(0) then
            raise (invalidArg "arr2" "Arrays have different lower bounds in the first dimension")
        if arr1.GetUpperBound(0) <> arr1.GetUpperBound(0) then
            raise (invalidArg "arr2" "Arrays have different upper bounds in the first dimension")
        if arr1.GetLowerBound(1) <> arr1.GetLowerBound(1) then
            raise (invalidArg "arr2" "Arrays have different lower bounds in the first dimension")
        if arr1.GetUpperBound(1) <> arr1.GetUpperBound(1) then
            raise (invalidArg "arr2" "Arrays have different upper bounds in the first dimension")
        Contract.EndContractBlock()

        let low0 = arr1.GetLowerBound(0) + 1
        let low1 = arr1.GetLowerBound(1) + 1
        let upper0 = arr1.GetUpperBound(0) + 1
        let upper1 = arr1.GetUpperBound(1) + 1

        let rec find i j result =
            if i = upper0 then result
            elif j = upper1 then find (i+1) low1 result
            else
                let elem1 = arr1.[i, j]
                let elem2 = arr2.[i, j]
                if fn elem1 elem2 then find i (j + 1) (((i, j), elem1, elem2) :: result)
                else find i (j + 1) result

        find 0 0 [] |> List.rev

    let linearize_by_row<'T> (arr : 'T[,]) =
        let rows = arr.GetUpperBound(1) + 1
        Array.init arr.Length (
            fun i ->
                let row = i / rows
                let column = i % rows
                arr.[row, column]
        )

    let linearize_by_row_fn<'T1, 'T2> (fn : int -> int -> 'T1 -> 'T2) (arr : 'T1[,]) =
        let rows = arr.GetUpperBound(1) + 1
        Array.init arr.Length (
            fun i ->
                let row = i / rows
                let column = i % rows
                let v = arr.[row, column]
                fn row column v
        )

    let subset<'T> (filter : (int * int)[,]) (target : 'T[,]) =
        Array2D.init (filter.GetUpperBound(0) + 1) (filter.GetUpperBound(1) + 1) (
            fun i j ->
                let (row, column) = filter.[i, j]
                target.[row, column]
        )

