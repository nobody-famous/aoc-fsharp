module Aoc.Year2018.Day7.Part2

open Aoc.Year2018.Day7.Utils

let stepSize = 0
let steps ch = int ch - int 'A' + 1 + stepSize

let run exp fileName =
    Parser.parseInput fileName |> ignore

// |> Aoc.Utils.Run.checkResult exp
