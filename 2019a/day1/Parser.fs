module Aoc.Year2019.Day1.Parser

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map int
