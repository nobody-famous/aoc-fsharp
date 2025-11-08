module Aoc.Year2018.Day9.Part2

let updateLastMarble config =
    { config with Utils.LastMarble = config.LastMarble * 100 }

let run (input: string list) =
    Utils.parse input
    |> updateLastMarble
    |> Utils.playGame
