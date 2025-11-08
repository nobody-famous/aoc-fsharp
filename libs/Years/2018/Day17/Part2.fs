module Aoc.Year2018.Day17.Part2

module U = Aoc.Year2018.Day17.Utils
module G = Aoc.Utils.Geometry

let nukeWater startPt grid =
    let rec removePts dx curPt (curGrid: U.Grid) =
        if not (Map.containsKey curPt curGrid.Pieces) then
            curGrid
        else
            match Map.find curPt curGrid.Pieces with
            | U.FillWater ->
                removePts
                    dx
                    { curPt with G.X = curPt.X + dx }
                    { curGrid with U.Pieces = Map.remove curPt curGrid.Pieces }
            | _ -> curGrid

    grid
    |> removePts 1 startPt
    |> removePts -1 { startPt with G.X = startPt.X - 1 }

let removeWaterPt pt grid =
    let abovePt = { pt with G.Y = pt.Y - 1 }

    grid |> nukeWater abovePt |> nukeWater pt

let removeWater (grid: U.Grid) =
    grid.Pieces
    |> Seq.map (fun kv -> (kv.Key, kv.Value))
    |> Seq.filter (fun (_, piece) ->
        match piece with
        | U.DropWater -> true
        | _ -> false)
    |> Seq.map (fun (pt, _) -> pt)
    |> Seq.fold (fun oldGrid pt -> removeWaterPt pt { oldGrid with U.Pieces = Map.remove pt oldGrid.Pieces }) grid

let run (input: string list) =
    let grid = U.parse input

    grid
    |> U.fillWater { G.X = 500; G.Y = 0 }
    |> removeWater
    |> U.sumWater
