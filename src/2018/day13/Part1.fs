module Aoc.Year2018.Day13.Part1

module Utils = Aoc.Year2018.Day13.Utils


let buildState (lines: string list) =
    let mutable state = Utils.newState ()

    let updateTurns (turns: Utils.Point list) =
        for turn in turns do
            let horiz =
                if state.Grid.ContainsKey { turn with X = turn.X + 1 } then
                    Utils.East
                elif state.Grid.ContainsKey { turn with X = turn.X - 1 } then
                    Utils.West
                else
                    failwith $"No horizontal part for turn {turn}"

            let vert =
                if state.Grid.ContainsKey { turn with Y = turn.Y - 1 } then
                    Utils.North
                elif state.Grid.ContainsKey { turn with Y = turn.Y + 1 } then
                    Utils.South
                else
                    failwith $"No vertical part for turn {turn}"

            state.Grid.Add(turn, Utils.Turn(horiz, vert))

    let rec parseLines lines y =
        let mutable turns: Utils.Point list = []

        match lines with
        | [] -> ()
        | (next: string) :: rest ->
            for x in 0 .. next.Length - 1 do
                let loc = { Utils.X = x; Utils.Y = y }

                match next.[x] with
                | ch when Utils.isTurn ch -> turns <- loc :: turns
                | ch when Utils.isCart ch ->
                    let cart =
                        { Utils.Loc = loc
                          Utils.Dir = Utils.charToDir ch
                          Utils.NextTurn = Utils.Left }

                    state.Carts.Add(loc, cart)
                    state.Grid.Add(loc, Utils.charToTrack next.[x])
                | ch when Utils.isTrack ch -> state.Grid.Add({ X = x; Y = y }, Utils.charToTrack next.[x])
                | _ -> ()

            parseLines rest (y + 1)
            updateTurns turns

    parseLines lines 0
    state

let parse (input: string) =
    input.Split '\n'
    |> Utils.trimIndent
    |> Array.toList
    |> buildState

let run (input: string) =
    let state = parse input

    Utils.printGrid state
