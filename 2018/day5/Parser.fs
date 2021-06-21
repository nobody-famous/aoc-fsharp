module Aoc.Year2018.Day5.Parser

let parseInput fileName =
    let lines = Aoc.Utils.Parser.readLines fileName
    lines.[0]
