module Aoc.Year2018.Day6.Parser

open Aoc.Utils.Geometry

let lineToPoint (line: string) =
    let parts =
        line.Split ','
        |> Array.map (fun (s: string) -> int (s.Trim()))

    { X = parts.[0]; Y = parts.[1] }

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map lineToPoint
