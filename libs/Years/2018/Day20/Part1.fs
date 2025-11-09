module Aoc.Year2018.Day20.Part1

module U = Aoc.Year2018.Day20.Utils
module G = Aoc.Utils.Geometry

let run (input: string list) =
    U.parse input
    |> U.calcDists
    |> Seq.map (fun kv -> kv.Value)
    |> Seq.max
