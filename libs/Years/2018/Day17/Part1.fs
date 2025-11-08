module Aoc.Year2018.Day17.Part1

module U = Aoc.Year2018.Day17.Utils
module G = Aoc.Utils.Geometry

let run (input: string list) =
    let grid = U.parse input

    grid
    |> U.fillWater { G.X = 500; G.Y = 0 }
    |> U.sumWater
