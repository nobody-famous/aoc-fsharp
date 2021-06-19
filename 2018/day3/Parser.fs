module Aoc.Year2018.Day3.Parser

open utils.geometry

type dimensions = { width: int; height: int }

type rectangle =
    { id: int
      loc: Point
      dims: dimensions }

let parseId (str: string) = str.Trim().Substring(1) |> int

let parseLoc (str: string) =
    let parts = str.Trim().Split ","
    { x = int parts.[0]; y = int parts.[1] }

let parseDims (str: string) =
    let parts = str.Trim().Split "x"

    { width = int parts.[0]
      height = int parts.[1] }

let parseRect (line: string) =
    let parts = line.Split "@"
    let rectParts = parts.[1].Split ":"

    { id = parseId parts.[0]
      loc = parseLoc rectParts.[0]
      dims = parseDims rectParts.[1] }

let parseInput fileName =
    utils.parser.readLines fileName
    |> Array.map parseRect
