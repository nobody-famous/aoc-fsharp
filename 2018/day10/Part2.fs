module Aoc.Year2018.Day10.Part2

open Aoc.Year2018.Day10.Utils

let run exp fileName =
    Parser.parseInput fileName
    |> findMinGrid
    |> Aoc.Utils.Run.checkResult exp
