module Aoc.Year2019.Day4.Part2

let isValid (arr: int array) =
    let rec loop ndx count =
        if ndx >= arr.Length then
            count = 2
        else if arr.[ndx - 1] = arr.[ndx] then
            loop <| ndx + 1 <| count + 1
        else if count = 2 then
            true
        else
            loop <| ndx + 1 <| 1

    loop 1 1

let run fileName =
    Parser.parseInput fileName
    |> Utils.splitInput
    |> Utils.findFirst
    |> Utils.countValid isValid
