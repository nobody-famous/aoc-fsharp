module Aoc.Year2018.Day18.Part1

module U = Aoc.Year2018.Day18.Utils

let run (input: string) =
    let grid = U.parse input

    let rec loop count curGrid =
        match count with
        | 10 -> curGrid
        | _ -> loop (count + 1) (U.runMinute curGrid)

    grid |> loop 0 |> U.getResourceValue
