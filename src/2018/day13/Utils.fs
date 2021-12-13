module Aoc.Year2018.Day13.Utils

open System.Collections.Generic

type Point = { X: int; Y: int }

type Direction =
    | North
    | South
    | East
    | West

type Piece =
    | HorizLine
    | VertLine
    | Turn of Direction * Direction
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
    { Loc: Point
      Dir: Direction
      NextTurn: Turn }

[<Struct>]
type State =
    { Grid: Dictionary<Point, Piece>
      Carts: Dictionary<Point, Cart>
      Crash: option<Point> }

let newState () =
    { Grid = Dictionary<Point, Piece>()
      Carts = Dictionary<Point, Cart>()
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
    | '^' -> North
    | 'v' -> South
    | '>' -> East
    | '<' -> West
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

let trimIndent (input: string array) : string array =
    let strIndent (str: string) =
        str.ToCharArray()
        |> Array.takeWhile (fun ch -> System.Char.IsWhiteSpace ch)

    let stripIndent (input: string array) : string array =
        let indent =
            Array.fold (fun acc str -> min acc (strIndent str).Length) System.Int32.MaxValue input

        Array.map (fun (str: string) -> str.Substring indent) input

    let findEnds (input: string array) : int * int =
        let mutable startNdx = 0
        let mutable endNdx = input.Length - 1

        while input.[startNdx].Trim().Length = 0 do
            startNdx <- startNdx + 1

        while input.[endNdx].Trim().Length = 0 do
            endNdx <- endNdx - 1

        (startNdx, endNdx)

    let cutEnds (input: string array) startNdx endNdx : string array =
        Array.sub input startNdx (endNdx - startNdx + 1)

    input |> findEnds ||> cutEnds input |> stripIndent

let findBounds (pts: Point list) : Point * Point =
    List.fold
        (fun (minPt, maxPt) (pt: Point) ->
            ({ X = min pt.X minPt.X
               Y = min pt.Y minPt.Y },
             { X = max pt.X maxPt.X
               Y = max pt.Y maxPt.Y }))
        ({ X = System.Int32.MaxValue
           Y = System.Int32.MaxValue },
         { X = System.Int32.MinValue
           Y = System.Int32.MinValue })
        pts

let printGrid state =
    let (minPt, maxPt) =
        state.Grid.Keys |> Seq.toList |> findBounds

    let pieceToChar piece =
        match piece with
        | HorizLine -> '-'
        | VertLine -> '|'
        | Turn (East, North) -> '\\'
        | Turn (West, North) -> '/'
        | Turn (East, South) -> '/'
        | Turn (West, South) -> '\\'
        | Intersect -> '+'
        | _ -> failwith $"Invalid piece {piece}"

    let cartToPiece cart =
        match cart.Dir with
        | North -> '^'
        | South -> 'v'
        | East -> '>'
        | West -> '<'

    for y in minPt.Y .. maxPt.Y do
        for x in minPt.X .. maxPt.X do
            let key = { X = x; Y = y }

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

    let updateTurns (turns: Point list) =
        for turn in turns do
            let horiz =
                if isHorizLine state { turn with X = turn.X + 1 } then
                    East
                elif isHorizLine state { turn with X = turn.X - 1 } then
                    West
                else
                    failwith $"No horizontal part for turn {turn}"

            let vert =
                if isVertLine state { turn with Y = turn.Y - 1 } then
                    North
                elif isVertLine state { turn with Y = turn.Y + 1 } then
                    South
                else
                    failwith $"No vertical part for turn {turn}"

            state.Grid.Add(turn, Turn(horiz, vert))

    let rec parseLines lines y =
        let mutable turns: Point list = []

        match lines with
        | [] -> ()
        | (next: string) :: rest ->
            for x in 0 .. next.Length - 1 do
                let loc = { X = x; Y = y }

                match next.[x] with
                | ch when isTurn ch -> turns <- loc :: turns
                | ch when isCart ch ->
                    let cart =
                        { Loc = loc
                          Dir = charToDir ch
                          NextTurn = Left }

                    state.Carts.Add(loc, cart)
                    state.Grid.Add(loc, charToTrack next.[x])
                | ch when isTrack ch -> state.Grid.Add({ X = x; Y = y }, charToTrack next.[x])
                | _ -> ()

            parseLines rest (y + 1)
            updateTurns turns

    parseLines lines 0
    state


let parse (input: string) =
    input.Split '\n'
    |> trimIndent
    |> Array.toList
    |> buildState

let move (state: State) (cart: Cart) =
    let mutable newLoc = cart.Loc
    let mutable newDir = cart.Dir
    let mutable newTurn = cart.NextTurn

    newLoc <-
        match cart.Dir with
        | North -> { cart.Loc with Point.Y = cart.Loc.Y - 1 }
        | South -> { cart.Loc with Point.Y = cart.Loc.Y + 1 }
        | East -> { cart.Loc with Point.X = cart.Loc.X + 1 }
        | West -> { cart.Loc with Point.X = cart.Loc.X - 1 }

    newDir <-
        match state.Grid.[newLoc] with
        | HorizLine
        | VertLine -> cart.Dir
        | Turn (East, North) ->
            match cart.Dir with
            | South -> East
            | West -> North
            | _ -> failwith $"Invalid dir {cart}"
        | Turn (West, North) ->
            match cart.Dir with
            | South -> West
            | East -> North
            | _ -> failwith $"Invalid dir {cart}"
        | Turn (East, South) ->
            match cart.Dir with
            | North -> East
            | West -> South
            | _ -> failwith $"Invalid dir {cart}"
        | Turn (West, South) ->
            match cart.Dir with
            | North -> West
            | East -> South
            | _ -> failwith $"Invalid dir {cart}"
        | Intersect ->
            match cart.NextTurn with
            | Left ->
                (newTurn <- Straight

                 match cart.Dir with
                 | North -> West
                 | South -> East
                 | East -> North
                 | West -> South)
            | Right ->
                (newTurn <- Left

                 match cart.Dir with
                 | North -> East
                 | South -> West
                 | East -> South
                 | West -> North)
            | Straight ->
                (newTurn <- Right
                 cart.Dir)
        | _ -> failwith $"Unhandled grid location {newLoc}"

    { cart with
        Loc = newLoc
        Dir = newDir
        NextTurn = newTurn }

let tick (state: State) =
    let mutable crash: option<Point> = None

    let carts =
        state.Carts.Values
        |> Seq.toList
        |> List.sortBy (fun cart -> cart.Loc.Y * 1000000 + cart.Loc.X)

    state.Carts.Clear()

    for cart in carts do
        let newCart = move state cart

        if state.Carts.ContainsKey newCart.Loc then
            (crash <- Some(newCart.Loc)
             state.Carts.Remove(newCart.Loc) |> ignore)
        else
            state.Carts.Add(newCart.Loc, newCart)

    { state with Crash = crash }
