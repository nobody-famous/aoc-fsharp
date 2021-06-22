module Aoc.Year2018.Day6.Part2

open Aoc.Utils.Geometry

let totalDist point points =
    Array.fold (fun d pt -> d + manDist point pt) 0 points

let countPoints target points =
    let (minPt, maxPt) = findBounds points
    let mutable safe = 0

    for x in minPt.X .. maxPt.X do
        for y in minPt.Y .. maxPt.Y do
            match totalDist { X = x; Y = y } points with
            | d when d < target -> safe <- safe + 1
            | _ -> ()

    safe

let run exp fileName =
    Parser.parseInput fileName
    |> countPoints 10000
    |> Aoc.Utils.Run.checkResult exp
