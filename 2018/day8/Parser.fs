module Aoc.Year2018.Day8.Parser

let parseInput fileName =
    let lines = Aoc.Utils.Parser.readLines fileName

    lines.[0].Split ' ' |> Array.map int
