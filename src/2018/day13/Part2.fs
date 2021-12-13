module Aoc.Year2018.Day13.Part2

module Utils = Aoc.Year2018.Day13.Utils

let run (input: string) =
    let mutable state = Utils.parse input

    while state.Carts.Count > 1 do
        state <- Utils.tick state

    // Utils.printGrid state
    printfn $"CARTS {state.Carts.Count}"
    let mutable answer = ""
    for cart in state.Carts.Values do
        answer <- $"{cart.Loc.X},{cart.Loc.Y}"

    answer