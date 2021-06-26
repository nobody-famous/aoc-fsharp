module Aoc.Year2018.Day9.Utils

type Game = { Players: int; LastMarble: int }

type Cell = { Left: int; Right: int }

type GameState =
    { Info: Game
      Player: int
      CurMarble: int
      NextMarble: int
      Board: Cell array }

let newGameState game =
    let board =
        Array.init
            (game.LastMarble + 1)
            (fun c ->
                match c with
                | 0 -> { Left = 0; Right = 0 }
                | _ -> { Left = -1; Right = -1 })

    { Info = game
      Player = 1
      CurMarble = 0
      NextMarble = 1
      Board = board }
