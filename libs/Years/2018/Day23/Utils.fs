module Aoc.Year2018.Day23.Utils

module S = Aoc.Utils.String
module G = Aoc.Utils.Geometry

type Nanobot = { Pos: G.Point3d; Radius: int }

type Regex = System.Text.RegularExpressions.Regex

let parseLine (line: string) =
    let rx =
        Regex(@"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)")

    let matches = rx.Matches(line)

    matches
    |> Seq.fold
        (fun acc m ->
            { Pos = G.makePoint3d (int m.Groups.[1].Value) (int m.Groups.[2].Value) (int m.Groups.[3].Value)
              Radius = int m.Groups.[4].Value }
            :: acc)
        []
    |> List.head

let parse (input: string list) =
    input
    |> List.toArray
    |> S.trimIndent
    |> Array.map parseLine
    |> Array.toList
