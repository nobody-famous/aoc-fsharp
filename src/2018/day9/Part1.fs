module y2018.day9.part1

type Config = { NumPlayers: int; LastMarble: int }

type Node =
    { mutable Prev: int
      mutable Next: int }

type State =
    { Scores: int []
      Marbles: Node []
      mutable CurMarble: int
      mutable NextMarble: int }

let newState players marbles =
    { Scores = [| for _ in 1 .. players -> 0 |]
      Marbles =
        [| for m in 1 .. marbles ->
               if m = 1 then
                   { Prev = 0; Next = 0 }
               else
                   { Prev = -1; Next = -1 } |]
      CurMarble = 0
      NextMarble = 1 }

let parse (input: string) =
    let parseNumPlayers (str: string) =
        let parts = str.Split ' '
        parts.[0].Trim() |> int

    let parseLastMarble (str: string) =
        let parts = str.Split ' '
        parts.[5].Trim() |> int

    let parts = input.Split ';'

    { NumPlayers = parseNumPlayers parts.[0]
      LastMarble = parseLastMarble parts.[1] }

let playMarble state =
    let firstNdx = state.Marbles.[state.CurMarble].Next
    let firstNode = state.Marbles.[firstNdx]
    let secondNdx = firstNode.Next

    state.Marbles.[firstNdx].Next <- state.NextMarble
    state.Marbles.[secondNdx].Prev <- state.NextMarble

    state.Marbles.[state.NextMarble].Next <- secondNdx
    state.Marbles.[state.NextMarble].Prev <- firstNdx

    state.CurMarble <- state.NextMarble
    state.NextMarble <- state.NextMarble + 1

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

let run (input: string) =
    let config = parse input

    let state =
        newState config.NumPlayers config.LastMarble

    for _ in 1 .. 22 do
        playMarble state

    printMarbles state

    0
