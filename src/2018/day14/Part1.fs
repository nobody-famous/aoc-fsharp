module Aoc.Year2018.Day14.Part1

type State =
    { mutable Elf1: int
      mutable Elf2: int
      mutable Next: int
      Scores: int array }

let newState recipes =
    { Elf1 = 0
      Elf2 = 1
      Next = 2
      Scores =
        Array.init (recipes + 10) (fun n ->
            match n with
            | 0 -> 3
            | 1 -> 7
            | _ -> 0) }

let printRecipes state =
    for ndx in 0 .. state.Next - 1 do
        let score = state.Scores.[ndx]

        match ndx with
        | n when n = state.Elf1 -> printf $"({score})"
        | n when n = state.Elf2 -> printf $"[{score}]"
        | _ -> printf $" {score} "

    printfn ""

let addNew (state: byref<State>) (score: int) =
    let mutable tmp = score

    if tmp >= 10 then
        if state.Next < state.Scores.Length then
            state.Scores.[state.Next] <- 1
            state.Next <- state.Next + 1
        tmp <- tmp % 10

    if state.Next < state.Scores.Length then
        state.Scores.[state.Next] <- tmp
        state.Next <- state.Next + 1

let createNew (state: byref<State>) =
    state.Scores.[state.Elf1]
    + state.Scores.[state.Elf2]

let pickNewCurrent state cur =
    let score = state.Scores.[cur]

    (cur + score + 1) % state.Next

let updateElf1 (state: byref<State>) =
    state.Elf1 <- pickNewCurrent state state.Elf1

let updateElf2 (state: byref<State>) =
    state.Elf2 <- pickNewCurrent state state.Elf2

let updateElfs (state: byref<State>) =
    updateElf1 &state
    updateElf2 &state

let run (input: string) =
    let target = input |> int
    let mutable state = newState target

    while state.Next < state.Scores.Length do
        let score = createNew &state
        addNew &state score
        updateElfs &state

    let mutable str = ""
    for ndx in target .. target + 9 do
        str <- $"{str}{state.Scores.[ndx]}"

    str
