module Aoc.Year2018.Day9.Utils

[<Struct>]
type Config = { NumPlayers: int; LastMarble: int }

[<Struct>]
type Node =
    { mutable Prev: int
      mutable Next: int }

[<Struct>]
type State =
    { Setup: Config
      Scores: int64 array
      Marbles: Node array
      mutable CurPlayer: int
      mutable CurMarble: int
      mutable NextMarble: int }

let newState cfg =
    { Setup = cfg
      Scores = Array.create cfg.NumPlayers 0L
      Marbles =
        Array.init (cfg.LastMarble + 1) (fun m ->
            match m with
            | 0 -> { Prev = 0; Next = 0 }
            | _ -> { Prev = -1; Next = -1 })
      CurPlayer = 0
      CurMarble = 0
      NextMarble = 1 }

let parse (input: string list) =
    let parseNumPlayers (str: string) =
        let parts = str.Split ' '
        parts.[0].Trim() |> int

    let parseLastMarble (str: string) =
        let parts = str.Split ' '
        parts.[5].Trim() |> int

    let parts = input.[0].Split ';'

    { NumPlayers = parseNumPlayers parts.[0]
      LastMarble = parseLastMarble parts.[1] }

let endRound marble (state: byref<State>) =
    state.CurMarble <- marble
    state.CurPlayer <- (state.CurPlayer + 1) % state.Setup.NumPlayers
    state.NextMarble <- state.NextMarble + 1

let insertMarble (state: byref<State>) =
    let firstNdx = state.Marbles.[state.CurMarble].Next
    let firstNode = state.Marbles.[firstNdx]
    let secondNdx = firstNode.Next

    state.Marbles.[firstNdx].Next <- state.NextMarble
    state.Marbles.[secondNdx].Prev <- state.NextMarble

    state.Marbles.[state.NextMarble].Next <- secondNdx
    state.Marbles.[state.NextMarble].Prev <- firstNdx

    endRound state.NextMarble &state

let findToRemove state =
    let rec loop count ndx =
        match count with
        | 0 -> ndx
        | _ -> loop (count - 1) (state.Marbles.[ndx].Prev)

    loop 7 state.CurMarble

let removeMarble state marble =
    let prev = state.Marbles.[marble].Prev
    let next = state.Marbles.[marble].Next

    state.Marbles.[prev].Next <- next
    state.Marbles.[next].Prev <- prev

let updateScore (state: byref<State>) =
    let toRemove = findToRemove state

    removeMarble state toRemove

    state.Scores.[state.CurPlayer] <-
        state.Scores.[state.CurPlayer]
        + (state.NextMarble |> int64)
        + (toRemove |> int64)

    endRound state.Marbles.[toRemove].Next &state

let play (state: byref<State>) =
    match state.NextMarble with
    | m when m % 23 = 0 -> updateScore &state
    | _ -> insertMarble &state

let printMarbles state =
    let rec printMarble ndx zeroDone =
        let node = state.Marbles.[ndx]

        if ndx = state.CurMarble then
            printf $"({ndx}) "
        else
            printf $"{ndx} "

        if not zeroDone || node.Next <> 0 then
            printMarble node.Next true

    printMarble 0 false

let playGame config =
    let mutable state = newState config

    for _ in 1 .. config.LastMarble do
        play &state

    Array.max state.Scores
