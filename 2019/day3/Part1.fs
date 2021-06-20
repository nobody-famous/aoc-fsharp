module Aoc.Year2019.Day3.Part1

open Utils.Geometry

let run fileName =
    let wires = Parser.parseInput fileName

    Utils.findAllCrosses wires.[0] wires.[1]
    |> List.map (fun cross -> manDist origin cross)
    |> List.fold (fun acc d -> if d <> 0 && d < acc then d else acc) System.Int32.MaxValue
