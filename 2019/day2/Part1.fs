module Aoc.Year2019.Day2.Part1

open Aoc.Year2019.Intcode

let run fileName =
    parseInput fileName
    |> newMachine
    |> setAddr 1 12
    |> setAddr 2 2
    |> Utils.runMachine
    |> getAddr 0
