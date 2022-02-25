module Aoc.Year2018.Day22.Part1

module U = Aoc.Year2018.Day22.Utils

let riskLevel (grid: U.Grid) =
    grid.Values
    |> Seq.sumBy (fun r ->
        match r.Type with
        | U.Rocky -> 0
        | U.Wet -> 1
        | U.Narrow -> 2)

let run (input: string) =
    U.parse input |> U.buildGrid |> riskLevel
