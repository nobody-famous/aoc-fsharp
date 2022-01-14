module Aoc.Year2018.Day14.Part1

let run (input: string) =
    let target = input |> int
    let mutable state = Utils.newState target

    while state.Next < state.Scores.Length do
        let score = Utils.createNewRecipe &state
        Utils.addNewScore &state score
        Utils.updateElfs &state

    state.Scores[target..target + 9]
    |> Array.map (fun item -> string item)
    |> String.concat ""
