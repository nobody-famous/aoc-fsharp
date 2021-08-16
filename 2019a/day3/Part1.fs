module Aoc.Year2019.Day3.Part1

open Aoc.Utils.Geometry

let run exp fileName =
    Parser.parseInput fileName
    |> Utils.findCrosses
    |> List.map (fun pt -> manDist origin pt)
    |> List.min
    |> Aoc.Utils.Run.checkResult exp
