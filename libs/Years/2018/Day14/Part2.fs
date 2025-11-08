module Aoc.Year2018.Day14.Part2

let matches (first: Utils.IntList) startNdx (second: Utils.IntList) =
    let mutable equal =
        first.Count >= second.Count && startNdx >= 0

    let mutable sndNdx = 0
    let endNdx = startNdx + second.Count - 1

    for ndx in startNdx .. endNdx do
        if equal && first.Item(ndx) <> second.Item(sndNdx) then
            equal <- false

        sndNdx <- sndNdx + 1

    equal

let rec findRecipes (state: byref<Utils.State>) (target: Utils.IntList) =
    let ndx = state.Scores.Count - target.Count

    match state with
    | s when matches state.Scores ndx target -> ndx
    | s when matches state.Scores (ndx - 1) target -> (ndx - 1)
    | _ ->
        let score = Utils.createNewRecipe &state
        Utils.addNewScore &state score
        Utils.updateElfs &state

        findRecipes &state target

let buildTarget (input: string list) =
    let target = Utils.IntList()

    for item in input.[0] do
        target.Add(int item - int '0')

    target

let run (input: string list) =
    let target = buildTarget input
    let mutable state = Utils.newState ()

    findRecipes &state target
