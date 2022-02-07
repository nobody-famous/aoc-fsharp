module Aoc.Year2018.Day18.Part1

module U = Aoc.Year2018.Day18.Utils

let run (input: string) =
    let board = U.parse input

    let rec loop count curBoard =
        match count with
        | 10 -> curBoard
        | _ -> loop (count + 1) (U.runMinute curBoard)

    board |> loop 0 |> U.getResourceValue
