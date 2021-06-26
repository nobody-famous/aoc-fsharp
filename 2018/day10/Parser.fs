module Aoc.Year2018.Day10.Parser

open Aoc.Year2018.Day10.Utils
open Aoc.Utils.Geometry
open Aoc.Utils.Regex

let regex =
    @"position=<\s*([-]?[0-9]+),\s*([-]?[0-9]+)> velocity=<\s*([-]?[0-9]+),\s*([-]?[0-9]+)>"

let parseLine line =
    match line with
    | MatchPattern regex [ px; py; vx; vy ] ->
        { Pos = { X = int px; Y = int py }
          Vel = { X = int vx; Y = int vy } }
    | _ -> failwith $"Invalid line: {line}"

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map parseLine
