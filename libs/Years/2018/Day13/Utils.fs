module Aoc.Year2018.Day13.Utils

module G = Aoc.Utils.Geometry
module S = Aoc.Utils.String
module T = Aoc.Utils.Types

open System.Collections.Generic

type Piece =
    | HorizLine
    | VertLine
    | Turn of T.Direction * T.Direction
    | Intersect
    | CartUp
    | CartDown
    | CartRight
    | CartLeft

type Turn =
    | Left
    | Right
    | Straight

[<Struct>]
type Cart =
    { Loc: G.Point
      Dir: T.Direction
      NextTurn: Turn
      IsCrashed: bool }

[<Struct>]
type State =
    { Grid: Dictionary<G.Point, Piece>
      Carts: Dictionary<G.Point, Cart>
      Crash: option<G.Point> }

let newState () =
    { Grid = Dictionary<G.Point, Piece>()
      Carts = Dictionary<G.Point, Cart>()
      Crash = None }

let isTrack ch =
    match ch with
    | '|'
    | '-'
    | '+' -> true
    | _ -> false

let isTurn ch =
    match ch with
    | '/'
    | '\\' -> true
    | _ -> false

let isCart ch =
    match ch with
    | '^'
    | 'v'
    | '<'
    | '>' -> true
    | _ -> false

let charToDir ch =
    match ch with
    | '^' -> T.North
    | 'v' -> T.South
    | '>' -> T.East
    | '<' -> T.West
    | _ -> failwith $"Invalid direction -{ch}-"

let charToTrack ch =
    match ch with
    | '^'
    | 'v'
    | '|' -> VertLine
    | '<'
    | '>'
    | '-' -> HorizLine
    | '+' -> Intersect
    | _ -> failwith $"Invalid char -{ch}-"

let printGrid state =
    let minPt, maxPt =
        state.Grid.Keys |> Seq.toList |> G.findBounds

    let pieceToChar piece =
        match piece with
        | HorizLine -> '-'
        | VertLine -> '|'
        | Turn (T.East, T.North) -> '\\'
        | Turn (T.West, T.North) -> '/'
        | Turn (T.East, T.South) -> '/'
        | Turn (T.West, T.South) -> '\\'
        | Intersect -> '+'
        | _ -> failwith $"Invalid piece {piece}"

    let cartToPiece cart =
        match cart.Dir with
        | T.North -> '^'
        | T.South -> 'v'
        | T.East -> '>'
        | T.West -> '<'

    for y in minPt.Y .. maxPt.Y do
        for x in minPt.X .. maxPt.X do
            let key: G.Point = { X = x; Y = y }

            if state.Crash.IsSome && state.Crash.Value = key then
                printf "X"
            elif state.Carts.ContainsKey key then
                printf $"{cartToPiece state.Carts.[key]}"
            elif state.Grid.ContainsKey key then
                printf $"{pieceToChar state.Grid.[key]}"
            else
                printf " "

        printfn ""

let isHorizLine state pt =
    state.Grid.ContainsKey pt
    && (state.Grid.[pt] = HorizLine
        || state.Grid.[pt] = Intersect)

let isVertLine state pt =
    state.Grid.ContainsKey pt
    && (state.Grid.[pt] = VertLine
        || state.Grid.[pt] = Intersect)

let buildState (lines: string list) =
    let mutable state = newState ()

    let rec updateTurns (turns: G.Point list) =
        match turns with
        | [] -> ()
        | turn :: rest ->
            let horiz =
                if isHorizLine state { turn with X = turn.X + 1 } then
                    T.East
                elif isHorizLine state { turn with X = turn.X - 1 } then
                    T.West
                else
                    failwith $"No horizontal part for turn {turn}"

            let vert =
                if isVertLine state { turn with Y = turn.Y - 1 } then
                    T.North
                elif isVertLine state { turn with Y = turn.Y + 1 } then
                    T.South
                else
                    failwith $"No vertical part for turn {turn}"

            state.Grid.Add(turn, Turn(horiz, vert))
            updateTurns rest

    let rec parseLines lines y =
        let mutable turns: G.Point list = []

        match lines with
        | [] -> ()
        | next: string :: rest ->
            for x in 0 .. next.Length - 1 do
                let loc: G.Point = { X = x; Y = y }

                match next.[x] with
                | ch when isTurn ch -> turns <- loc :: turns
                | ch when isCart ch ->
                    let cart =
                        { Loc = loc
                          Dir = charToDir ch
                          NextTurn = Left
                          IsCrashed = false }

                    state.Carts.Add(loc, cart)
                    state.Grid.Add(loc, charToTrack next.[x])
                | ch when isTrack ch -> state.Grid.Add({ X = x; Y = y }, charToTrack next.[x])
                | _ -> ()

            parseLines rest (y + 1)
            updateTurns turns

    parseLines lines 0
    state

let parse (input: string list) =
    buildState input

let move (state: State) (cart: Cart) =
    let mutable newLoc = cart.Loc
    let mutable newDir = cart.Dir
    let mutable newTurn = cart.NextTurn

    newLoc <-
        match cart.Dir with
        | T.North -> { cart.Loc with Y = cart.Loc.Y - 1 }
        | T.South -> { cart.Loc with Y = cart.Loc.Y + 1 }
        | T.East -> { cart.Loc with X = cart.Loc.X + 1 }
        | T.West -> { cart.Loc with X = cart.Loc.X - 1 }

    newDir <-
        match state.Grid.[newLoc] with
        | HorizLine
        | VertLine -> cart.Dir
        | Turn (T.East, T.North) ->
            match cart.Dir with
            | T.South -> T.East
            | T.West -> T.North
            | _ -> failwith $"Invalid dir {cart}"
        | Turn (T.West, T.North) ->
            match cart.Dir with
            | T.South -> T.West
            | T.East -> T.North
            | _ -> failwith $"Invalid dir {cart}"
        | Turn (T.East, T.South) ->
            match cart.Dir with
            | T.North -> T.East
            | T.West -> T.South
            | _ -> failwith $"Invalid dir {cart}"
        | Turn (T.West, T.South) ->
            match cart.Dir with
            | T.North -> T.West
            | T.East -> T.South
            | _ -> failwith $"Invalid dir {cart}"
        | Intersect ->
            match cart.NextTurn with
            | Left ->
                newTurn <- Straight

                match cart.Dir with
                | T.North -> T.West
                | T.South -> T.East
                | T.East -> T.North
                | T.West -> T.South
            | Right ->
                newTurn <- Left

                match cart.Dir with
                | T.North -> T.East
                | T.South -> T.West
                | T.East -> T.South
                | T.West -> T.North
            | Straight ->
                newTurn <- Right
                cart.Dir
        | _ -> failwith $"Unhandled grid location {newLoc}"

    { cart with
        Loc = newLoc
        Dir = newDir
        NextTurn = newTurn }

let tick (state: State) =
    let mutable crash: option<G.Point> = None

    let rec processCarts locs =
        match locs with
        | [] -> ()
        | loc :: rest ->
            let cart = state.Carts.[loc]

            if cart.IsCrashed then
                state.Carts.Remove loc |> ignore
                processCarts rest
            else
                let newCart = move state cart

                if state.Carts.ContainsKey newCart.Loc then
                    crash <- Some(newCart.Loc)
                    state.Carts.[newCart.Loc] <- { newCart with IsCrashed = true }
                else
                    state.Carts.[newCart.Loc] <- newCart

                state.Carts.Remove cart.Loc |> ignore
                processCarts rest

    state.Carts.Values
    |> Seq.toList
    |> List.sortBy (fun cart -> cart.Loc.Y * 1000 + cart.Loc.X)
    |> List.map (fun cart -> cart.Loc)
    |> processCarts

    for loc in state.Carts.Keys do
        if state.Carts.[loc].IsCrashed then
            state.Carts.Remove loc |> ignore

    { state with Crash = crash }
