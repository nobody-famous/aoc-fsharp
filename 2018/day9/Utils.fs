module Aoc.Year2018.Day9.Utils

type Game = { Players: int; LastMarble: int }

type Cell = { Left: int; Right: int }

type GameState =
    { Info: Game
      Player: int
      CurMarble: int
      NextMarble: int
      Scores: int64 array
      Board: Cell array }

let newGameState game =
    let board =
        Array.init
            (game.LastMarble + 1)
            (fun c ->
                match c with
                | 0 -> { Left = 0; Right = 0 }
                | _ -> { Left = -1; Right = -1 })

    let scores = Array.create game.Players 0L

    { Info = game
      Player = 0
      CurMarble = 0
      NextMarble = 1
      Scores = scores
      Board = board }

let endRound curMarble (state: GameState) =
    { state with
          Player = (state.Player + 1) % state.Info.Players
          CurMarble = curMarble
          NextMarble = state.NextMarble + 1 }

let specialRound (state: GameState) =
    let mutable toRemove = state.CurMarble

    for _ in 1 .. 7 do
        toRemove <- state.Board.[toRemove].Left

    let left = state.Board.[toRemove].Left
    let right = state.Board.[toRemove].Right

    state.Board.[left] <-
        { state.Board.[left] with
              Right = right }

    state.Board.[right] <- { state.Board.[right] with Left = left }

    state.Scores.[state.Player] <-
        state.Scores.[state.Player]
        + (int64) toRemove
        + (int64) state.NextMarble

    endRound right state

let normalRound (state: GameState) =
    let first = state.Board.[state.CurMarble].Right
    let second = state.Board.[first].Right

    state.Board.[state.NextMarble] <- { Left = first; Right = second }

    state.Board.[first] <-
        { state.Board.[first] with
              Right = state.NextMarble }

    state.Board.[second] <-
        { state.Board.[second] with
              Left = state.NextMarble }

    endRound state.NextMarble state

let round (state: GameState) =
    match state.NextMarble with
    | marble when marble % 23 = 0 -> specialRound state
    | _ -> normalRound state

let printBoard state =
    let board = state.Board
    let mutable marble = 0

    printf $"[{state.Player}] "

    while board.[marble].Right <> 0 do
        if marble = state.CurMarble then
            printf $"({marble}) "
        else
            printf $"{marble} "

        marble <- board.[marble].Right

    printf $"{marble}"
    printfn ""

let playGame game =
    let mutable state = newGameState game

    while state.NextMarble <= state.Info.LastMarble do
        state <- round state

    Array.max state.Scores
