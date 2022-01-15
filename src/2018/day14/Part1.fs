module Aoc.Year2018.Day14.Part1

let run (input: string) =
    let target = input |> int
    let mutable state = Utils.newState ()

    for _ in 1 .. target + 10 do
        let score = Utils.createNewRecipe &state
        Utils.addNewScore &state score
        Utils.updateElfs &state

    state.Scores.GetRange(target, 10)
    |> Seq.map (fun item -> string item)
    |> String.concat ""
