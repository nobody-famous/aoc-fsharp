module Aoc.Year2018.Day22.Part1

module G = Aoc.Utils.Geometry
module S = Aoc.Utils.String

type RegionType =
    | Rocky
    | Narrow
    | Wet

type Region =
    { Type: RegionType
      Erosion: int
      GeoIndex: int }

type Grid = System.Collections.Generic.Dictionary<G.Point, Region>

type Config = { Depth: int; Target: G.Point }

let parse (input: string) =
    let parseDepth (line: string) =
        let parts = line.Split ':'

        parts.[1].Trim() |> int

    let parseTarget (line: string) =
        let parts = line.Split ':'
        let ptParts = parts.[1].Split ','

        { G.X = ptParts.[0].Trim() |> int
          G.Y = ptParts.[1].Trim() |> int }

    let lines =
        input.Split '\n' |> S.trimIndent |> Array.toList

    { Depth = parseDepth lines.[0]
      Target = parseTarget lines.[1] }

let run (input: string) =
    let cfg = parse input

    printfn $"{cfg}"
    0
