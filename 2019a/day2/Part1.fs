module Aoc.Year2019.Day2.Part1

open Aoc.Year2019.Utils.Intcode

let run exp fileName =
    parseInput fileName
    |> machine
    |> setDebug false
    |> setPosition 1 12
    |> setPosition 2 2
    |> execAll
    |> getPosition 0
    |> Aoc.Utils.Run.checkResult exp
