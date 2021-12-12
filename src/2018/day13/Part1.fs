module y2018.day13.part1

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
    | Up
    | Down
    | Right
    | Left

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
      mutable Carts: Cart list }

let newState () =
    { Grid = Dictionary<Point, Piece>()
      Carts = [] }

let findBounds (pts: Point list) : Point * Point =
    List.fold
        (fun (minPt, maxPt) pt ->
            ({ X = min pt.X minPt.X
               Y = min pt.Y minPt.Y },
             { X = max pt.X maxPt.X
               Y = max pt.Y maxPt.Y }))
        ({ X = System.Int32.MaxValue
           Y = System.Int32.MaxValue },
         { X = System.Int32.MinValue
           Y = System.Int32.MinValue })
        pts

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

let isTrack ch =
    match ch with
    | '|'
    | '-'
    | '/'
    | '+'
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
    | '|' -> Piece.VertLine
    | '<'
    | '>'
    | '-' -> Piece.HorizLine
    | '/' -> Piece.Turn(North, North)
    | '\\' -> Piece.Turn(North, North)
    | '+' -> Piece.Intersect
    | _ -> failwith $"Invalid char -{ch}-"

let buildGrid (lines: string list) =
    let mutable state = newState ()

    let rec parseLines lines y =
        match lines with
        | [] -> state
        | (next: string) :: rest ->
            for x in 0 .. next.Length - 1 do
                match next.[x] with
                | ch when isCart ch ->
                    let loc = { X = x; Y = y }

                    let cart =
                        { Loc = loc
                          Dir = charToDir ch
                          NextTurn = Left }

                    state.Carts <- cart :: state.Carts
                    state.Grid.Add(loc, charToTrack next.[x])
                | ch when isTrack ch -> state.Grid.Add({ X = x; Y = y }, charToTrack next.[x])
                | _ -> ()

            parseLines rest (y + 1)

    parseLines lines 0

let parse (input: string) =
    input.Split '\n'
    |> trimIndent
    |> Array.toList
    |> buildGrid

let run (input: string) =
    let state = parse input

    let bounds =
        state.Grid.Keys |> Seq.toList |> findBounds

    printfn $"STATE {state}"
