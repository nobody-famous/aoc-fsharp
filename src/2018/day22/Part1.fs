module Aoc.Year2018.Day22.Part1

module U = Aoc.Year2018.Day22.Utils
module G = Aoc.Utils.Geometry

let riskLevel (grid: U.Grid) (target: G.Point) =
    grid
    |> Seq.sumBy (fun kv ->
        let pt = kv.Key
        let r = kv.Value

        if pt.X > target.X || pt.Y > target.Y then
            0
        else
            match r.Type with
            | U.Rocky -> 0
            | U.Wet -> 1
            | U.Narrow -> 2)

let run (input: string) =
    let cfg = U.parse input
    let grid = U.buildGrid cfg

    riskLevel grid cfg.Target
