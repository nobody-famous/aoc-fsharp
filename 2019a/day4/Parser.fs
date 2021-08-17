module Aoc.Year2019.Day4.Parser

let parseLine (line: string) =
    line.Split '-'
    |> Array.map Seq.toArray
    |> Array.map (fun s -> Array.map (fun ch -> int ch - int '0') s)

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Aoc.Utils.Parser.grabFirst
    |> parseLine
