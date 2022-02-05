module Aoc.Year2018.Day18.Part2

module U = Aoc.Year2018.Day18.Utils

let run (input: string) =
    let grid = U.parse input

    let rec loop count (seen: Map<int, int * Map<int * int, U.Piece>>) curGrid =
        let answer = U.getAnswer curGrid

        if Map.containsKey answer seen then
            let (oldCount, oldGrid) = Map.find answer seen
            printfn $"REPEAT {answer} {oldGrid = curGrid} {oldCount} -> {count}"
            curGrid
        else
            let newSeen =
                Map.add (U.getAnswer curGrid) (count, curGrid) seen

            match count with
            | 500 -> curGrid
            | _ -> loop (count + 1) newSeen (U.runMinute curGrid)

    grid |> loop 0 Map.empty |> U.getAnswer
