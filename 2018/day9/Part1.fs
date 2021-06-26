module Aoc.Year2018.Day9.Part1

open Aoc.Year2018.Day9.Utils

let run exp fileName =
    Parser.parseInput fileName
    |> playGame
    |> Aoc.Utils.Run.checkResult exp
