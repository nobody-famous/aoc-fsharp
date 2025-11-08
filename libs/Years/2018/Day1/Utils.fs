module Aoc.Year2018.Day1.Utils

let parse (input: string list) =
    input
    |> List.filter (fun s -> s.Trim().Length > 0)
    |> List.map (fun s -> int s)
