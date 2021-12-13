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
      Carts: Dictionary<Point, Cart> }

let newState () =
    { Grid = Dictionary<Point, Piece>()
      Carts = Dictionary<Point, Cart>() }

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

        while input.[startNdx].Length = 0 do
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

            if state.Carts.ContainsKey key then
                printf $"{cartToPiece state.Carts.[key]}"
            elif state.Grid.ContainsKey key then
                printf $"{pieceToChar state.Grid.[key]}"
            else
                printf " "

        printfn ""
