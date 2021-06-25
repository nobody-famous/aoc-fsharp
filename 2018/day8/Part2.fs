module Aoc.Year2018.Day8.Part2

open Aoc.Year2018.Day8.Utils

let run exp fileName =
    Parser.parseInput fileName
    |> Array.toList
    |> parseTree
    |> ignore
