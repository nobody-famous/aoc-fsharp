module Aoc.Year2018.Day18.Part2

module U = Aoc.Year2018.Day18.Utils

type GridMap = System.Collections.Generic.Dictionary<int, int>

let hashPoint (x, y) = x + y

let hashGrid (grid: U.Grid) =
    grid
    |> Seq.sumBy (fun kv ->
        match kv.Value with
        | U.Empty -> hashPoint kv.Key
        | U.Tree -> hashPoint (fst kv.Key * 100, snd kv.Key * 100)
        | U.Yard -> hashPoint (fst kv.Key * 1000, snd kv.Key * 1000))

let run (input: string) =
    let grid = U.parse input

    let target = 1000000000

    let rec loop (seen: GridMap) count endCount curGrid =
        let hash = hashGrid curGrid

        if count = endCount then
            (0, curGrid)
        else if seen.ContainsKey hash then
            let start = seen.[hash]
            let newTarget = target - start
            let cycle = count - start
            let cycleCount = newTarget / cycle
            let remSteps = newTarget - (cycle * cycleCount)

            (remSteps, curGrid)
        else
            seen.[hash] <- count

            loop seen (count + 1) endCount (U.runMinute curGrid)

    grid
    |> loop (GridMap()) 0 target
    ||> loop (GridMap()) 0
    |> snd
    |> U.getResourceValue
