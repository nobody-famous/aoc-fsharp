module Aoc.Year2018.Day8.Part2

open Aoc.Year2018.Day8.Utils

let nodeValue node = node.Value

let run exp fileName =
    Parser.parseInput fileName
    |> Array.toList
    |> parseTree
    |> nodeValue
    |> Aoc.Utils.Run.checkResult exp
