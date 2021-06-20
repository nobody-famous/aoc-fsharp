module Aoc.Year2018.Day3.Parser

open Aoc.Utils.Geometry
open Aoc.Year2018.Day3.Utils

let parseId (str: string) = str.Trim().Substring(1) |> int

let parseLoc (str: string) =
    let parts = str.Trim().Split ","
    { X = int parts.[0]; Y = int parts.[1] }

let parseDims (str: string) =
    let parts = str.Trim().Split "x"

    { Width = int parts.[0]
      Height = int parts.[1] }

let parseRect (line: string) =
    let parts = line.Split "@"
    let rectParts = parts.[1].Split ":"

    { Id = parseId parts.[0]
      Loc = parseLoc rectParts.[0]
      Dims = parseDims rectParts.[1] }

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map parseRect
