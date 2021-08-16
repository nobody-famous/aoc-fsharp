module Aoc.Year2019.Day4.Parser

let parseLine (line: string) = line.Split '-' |> Array.map Seq.toArray

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Aoc.Utils.Parser.grabFirst
    |> parseLine
