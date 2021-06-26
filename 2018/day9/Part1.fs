module Aoc.Year2018.Day9.Part1

open Aoc.Year2018.Day9.Utils

let round (state: GameState) =
    let first = state.Board.[state.CurMarble].Right
    let second = state.Board.[first].Right

    state.Board.[state.NextMarble] <- { Left = first; Right = second }

    state.Board.[first] <-
        { Left = state.Board.[first].Left
          Right = state.NextMarble }

    state.Board.[second] <-
        { Left = state.NextMarble
          Right = state.Board.[second].Right }

    { state with
          CurMarble = state.NextMarble
          NextMarble = state.NextMarble + 1 }

let printBoard (board: Cell array) =
    let mutable marble = 0

    while board.[marble].Right <> 0 do
        printf $"{marble} "
        marble <- board.[marble].Right

    printfn ""

let playGame game =
    let mutable state = newGameState game

    while state.CurMarble <> state.Info.LastMarble do
        state <- round state

    printBoard state.Board
    ()

let run exp fileName =
    Parser.parseInput fileName |> playGame |> ignore
