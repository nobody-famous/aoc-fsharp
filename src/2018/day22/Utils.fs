module Aoc.Year2018.Day22.Utils

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

let geoToErosion depth geoNdx = (depth + geoNdx) % 20183

let calcGeoNdx (cfg: Config) (grid: Grid) (pt: G.Point) =
    if pt.X = 0 && pt.Y = 0
       || pt.X = cfg.Target.X && pt.Y = cfg.Target.Y then
        0
    else if pt.Y = 0 then
        pt.X * 16807
    else if pt.X = 0 then
        pt.Y * 48271
    else
        let left = grid.[{ pt with G.X = pt.X - 1 }]
        let above = grid.[{ pt with G.Y = pt.Y - 1 }]

        left.Erosion * above.Erosion


let calcRegion (cfg: Config) (grid: Grid) (pt: G.Point) =
    let geoNdx = calcGeoNdx cfg grid pt
    let level = (geoNdx + cfg.Depth) % 20183

    let regType =
        match level % 3 with
        | 0 -> Rocky
        | 1 -> Wet
        | 2 -> Narrow
        | n -> failwith $"Should not be here {n}"

    { Type = regType
      Erosion = level
      GeoIndex = geoNdx }

let buildGrid (cfg: Config) =
    let grid = Grid()

    for y in 0 .. 800 do
        for x in 0 .. 800 do
            let pt = { G.X = x; G.Y = y }
            grid.[pt] <- calcRegion cfg grid pt

    grid

let printGrid (grid: Grid) (target: G.Point) =
    let (minPt, maxPt) = G.findBounds (grid.Keys |> Seq.toList)

    for y in minPt.Y .. maxPt.Y do
        for x in minPt.X .. maxPt.X do
            let pt = { G.X = x; G.Y = y }

            if pt = target then
                printf "T"
            else
                match grid.[pt].Type with
                | Rocky -> printf "."
                | Wet -> printf "="
                | Narrow -> printf "|"

        printfn ""

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
