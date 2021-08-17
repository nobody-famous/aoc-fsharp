module Aoc.Year2019.Day4.Part2

open Aoc.Year2019.Day4.Utils

let hasPair (num: int []) =
    let rec loop ndx count =
        if ndx >= num.Length then
            count = 2
        elif num.[ndx - 1] = num.[ndx] then
            loop (ndx + 1) (count + 1)
        elif count = 2 then
            true
        else
            loop (ndx + 1) 1

    loop 1 1

let run exp fileName =
    Parser.parseInput fileName
    |> createState
    |> findFirst
    |> countMatches hasPair
    |> Aoc.Utils.Run.checkResult exp
