module Aoc.Year2018.Day14.Part2

let endsWith (state: Utils.State) target =
    let len = Array.length target
    target = state.Scores[ state.Next - len .. state.Next - 1 ]

let run (input: string) =
    let recipes = input |> int

    let target =
        input
        |> Seq.map (fun ch -> int ch - int '0')
        |> Seq.toArray

    let mutable state = Utils.newState recipes

    while not (endsWith state target) do
        let score = Utils.createNewRecipe &state
        Utils.addNewScore &state score
        Utils.updateElfs &state

    state.Next - Array.length target
