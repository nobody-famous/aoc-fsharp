module aoc.year2019.day3.part1

open utils.geometry

let run fileName =
    let wires = parser.parseInput fileName

    utils.findAllCrosses wires.[0] wires.[1]
    |> List.map (fun cross -> manDist origin cross)
    |> List.fold (fun acc d -> if d <> 0 && d < acc then d else acc) System.Int32.MaxValue
