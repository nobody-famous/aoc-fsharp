module Aoc.Year2018.Day14.Utils

type IntList = System.Collections.Generic.List<int>

type State =
    { mutable Elf1: int
      mutable Elf2: int
      Scores: IntList }

let newState () =
    let scores = IntList()

    scores.Add(3)
    scores.Add(7)

    { Elf1 = 0; Elf2 = 1; Scores = scores }

let createNewRecipe (state: byref<State>) =
    state.Scores.[state.Elf1]
    + state.Scores.[state.Elf2]

let addNewScore (state: byref<State>) (score: int) =
    if score >= 10 then state.Scores.Add(1)

    state.Scores.Add(score % 10)

let pickNewCurrent (state: State) cur =
    let score = state.Scores.[cur]

    (cur + score + 1) % state.Scores.Count

let updateElf1 (state: byref<State>) =
    state.Elf1 <- pickNewCurrent state state.Elf1

let updateElf2 (state: byref<State>) =
    state.Elf2 <- pickNewCurrent state state.Elf2

let updateElfs (state: byref<State>) =
    updateElf1 &state
    updateElf2 &state

let printRecipes (state: State) =
    for ndx in 0 .. state.Scores.Count - 1 do
        let score = state.Scores.Item(ndx)

        match ndx with
        | n when n = state.Elf1 -> printf $"({score})"
        | n when n = state.Elf2 -> printf $"[{score}]"
        | _ -> printf $" {score}"

    printfn ""
