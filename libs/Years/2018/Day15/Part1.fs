module Aoc.Year2018.Day15.Part1

module U = Aoc.Year2018.Day15.Utils

let run (input: string list) =
    let state = U.parse input
    let mutable roundNumber = 0

    while not (U.combatEnds state) do
        if U.round state then
            roundNumber <- roundNumber + 1

    U.computeAnswer roundNumber state
