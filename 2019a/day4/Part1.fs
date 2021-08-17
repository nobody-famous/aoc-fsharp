module Aoc.Year2019.Day4.Part1

open Aoc.Year2019.Day4.Utils

let hasPair (num: int []) =
    let mutable found = false

    for ndx in 1 .. num.Length - 1 do
        found <- found || num.[ndx] = num.[ndx - 1]

    found

let run exp fileName =
    Parser.parseInput fileName
    |> createState
    |> findFirst
    |> countMatches hasPair
    |> Aoc.Utils.Run.checkResult exp
