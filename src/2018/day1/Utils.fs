module Aoc.Year2018.Day1.Utils

let parse (input: string) =
    input.Split '\n'
    |> Array.filter (fun s -> s.Trim().Length > 0)
    |> Array.map (fun s -> int s)
    |> Array.toList
