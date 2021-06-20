module Aoc.Year2018.Day1.Part1

let run exp fileName =
    Parser.parseInput fileName
    |> Array.sum
    |> Utils.Run.checkResult exp
