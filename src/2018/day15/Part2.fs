module Aoc.Year2018.Day15.Part2

module U = Aoc.Year2018.Day15.Utils

let tryCombat (state: U.State) =
    let mutable roundNumber = 0
    let elfCount = Seq.length state.Elves

    while Seq.length state.Elves = elfCount
          && not (U.combatEnds state) do

        if U.round state then
            roundNumber <- roundNumber + 1

    if Seq.length state.Elves = elfCount then
        U.computeAnswer roundNumber state
    else
        -1

let run (input: string) =
    let state = U.parse input
    let mutable damage = 3
    let mutable result = -1

    while result = -1 do
        result <- tryCombat { U.copyState state with U.ElfDamage = damage }
        damage <- damage + 1

    result
