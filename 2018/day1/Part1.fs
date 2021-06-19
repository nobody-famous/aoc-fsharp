module Aoc.Year2018.Day1.Part1

let run fileName =
    Parser.parseInput fileName |> Array.sum
