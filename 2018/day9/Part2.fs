module Aoc.Year2018.Day9.Part2

open Aoc.Year2018.Day9.Utils

let updateLastMarble game =
    { game with
          LastMarble = game.LastMarble * 100 }

let run exp fileName =
    Parser.parseInput fileName
    |> updateLastMarble
    |> playGame
    |> Aoc.Utils.Run.checkResult exp
