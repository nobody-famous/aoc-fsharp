module Aoc.Year2018.Day19.Part1

module U = Aoc.Year2018.Day19.Utils

let run (input: string list) =
    let state = U.parse input

    U.mach 0
