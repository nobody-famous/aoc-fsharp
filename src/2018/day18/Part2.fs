module Aoc.Year2018.Day18.Part2

module U = Aoc.Year2018.Day18.Utils

let run (input: string) =
    let grid = U.parse input

    let seen =
        System.Collections.Generic.HashSet<int>()

    let rec loop count curGrid =
        let answer = U.getAnswer curGrid

        if seen.Contains answer then
            printfn $"REPEAT {answer}"
            curGrid
        else
            seen.Add answer |> ignore

            match count with
            | 500 -> curGrid
            | _ -> loop (count + 1) (U.runMinute curGrid)

    grid |> loop 0 |> U.getAnswer
