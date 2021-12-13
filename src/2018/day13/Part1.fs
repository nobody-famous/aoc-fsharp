module Aoc.Year2018.Day13.Part1

module Utils = Aoc.Year2018.Day13.Utils

let run (input: string) =
    let mutable state = Utils.parse input

    while state.Crash.IsNone do
        state <- Utils.tick state

    $"{state.Crash.Value.X},{state.Crash.Value.Y}"
