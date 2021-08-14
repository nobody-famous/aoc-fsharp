module Aoc.Year2019.Day1.Part1

let run exp fileName =
    Parser.parseInput fileName
    |> Array.map Utils.calcFuel
    |> Array.sum
    |> Aoc.Utils.Run.checkResult exp
