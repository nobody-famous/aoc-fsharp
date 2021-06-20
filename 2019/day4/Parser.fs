module Aoc.Year2019.Day4.Parser

let parseRange (line: string) =
    line.Split '-'
    |> Array.map
        (fun s ->
            s.ToCharArray()
            |> Array.map (fun ch -> int ch - int '0'))

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Aoc.Utils.Parser.grabFirst
    |> parseRange
