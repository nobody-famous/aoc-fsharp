module Aoc.Year2019.Day2.Parser

let getFirst (arr: 'a array) = arr.[0]

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map (fun (line: string) -> line.Split ',' |> Array.map int)
    |> getFirst
