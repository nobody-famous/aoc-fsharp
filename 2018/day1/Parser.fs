module Aoc.Year2018.Day1.Parser

let parseChange (line: string) =
    let n = line.Substring 1 |> int

    match line.[0] with
    | '+' -> n
    | '-' -> (-n)
    | _ -> failwith $"Invalid sign {line.[0]}"

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map parseChange
