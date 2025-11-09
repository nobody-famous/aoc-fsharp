module Aoc.Year2018.Day23.Part1

module U = Aoc.Year2018.Day23.Utils
module G = Aoc.Utils.Geometry

let run (input: string list) =
    let bots = U.parse input

    let largest =
        bots |> List.maxBy (fun bot -> bot.Radius)

    let inRange =
        bots
        |> List.filter (fun bot ->
            (G.manDist3d bot.Pos largest.Pos)
            <= largest.Radius)

    List.length inRange
