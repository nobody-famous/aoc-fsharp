module Aoc.Year2019.Day4.Utils

let splitInput (input: int array array) = (input.[0], input.[1])

let rec updateRest (arr: int array) (ch, ndx) =
    if ndx < arr.Length then
        arr.[ndx] <- ch
        updateRest arr <| (ch, ndx + 1)

let findFirst (low: int array, high) =
    let rec init ndx =
        if low.[ndx] < low.[ndx - 1] then
            (low.[ndx - 1], ndx)
        else
            init <| ndx + 1

    init 1 |> updateRest low

    (low, high)

let inc (arr: int array) =
    let rec loop ndx =
        arr.[ndx] <- arr.[ndx] + 1

        if arr.[ndx] >= 10 then
            arr.[ndx] <- 0
            loop (ndx - 1)
        else
            updateRest arr (arr.[ndx], ndx + 1)
            arr

    loop <| arr.Length - 1

let lessThan (arr1: int array) (arr2: int array) =
    let rec loop ndx =
        if ndx >= arr1.Length || arr1.[ndx] > arr2.[ndx] then
            false
        else if arr1.[ndx] < arr2.[ndx] then
            true
        else
            loop <| ndx + 1

    loop 0

let countValid isValid (low, high) =
    let rec loop num count =
        if lessThan low high then
            loop (inc num)
            <| if isValid low then count + 1 else count
        else
            count

    loop low 0
