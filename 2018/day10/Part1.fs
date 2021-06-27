module Aoc.Year2018.Day10.Part1

open System.Collections.Generic
open Aoc.Year2018.Day10.Utils
open Aoc.Utils.Geometry

let buildGrid points =
    let grid = Dictionary<Point, bool>()

    Array.iter (fun pt -> grid.[pt.Pos] <- true) points

    grid

let printGrid points =
    let grid = buildGrid points
    let (minPt, maxPt) = findMinMax points

    for y in minPt.Y .. maxPt.Y do
        for x in minPt.X .. maxPt.X do
            if grid.ContainsKey { X = x; Y = y } then
                printf "#"
            else
                printf "."

        printfn ""

    printfn ""

let run exp fileName =
    Parser.parseInput fileName
    |> findMinGrid
    |> ignore

    Aoc.Utils.Run.checkResult exp "EJZEAAPE"
