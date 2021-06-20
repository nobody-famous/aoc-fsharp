module Aoc.Year2019.Day4.Part1

let isValid (arr: int array) =
    let rec loop ndx =
        if ndx >= arr.Length then
            false
        else if arr.[ndx - 1] = arr.[ndx] then
            true
        else
            loop <| ndx + 1

    loop 1

let run fileName =
    Parser.parseInput fileName
    |> Utils.splitInput
    |> Utils.findFirst
    |> Utils.countValid isValid
