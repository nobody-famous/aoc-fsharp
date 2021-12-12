module y2018.day13.part1

open System.Collections.Generic

type Point = { X: int; Y: int }

type Piece =
    | HorizLine
    | VertLine
    | CurveRight
    | CurveLeft
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
      Dir: Piece
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



let trimIndent (input: string list) : string list =
    let strIndent (str: string) =
        str.ToCharArray()
        |> Array.takeWhile (fun ch -> System.Char.IsWhiteSpace ch)

    let rec removeBlanks (lines: string list) =
        match lines with
        | [] -> []
        | line :: rest ->
            if line.Trim().Length = 0 then
                removeBlanks rest
            else
                line :: rest

    let indent =
        input
        |> removeBlanks
        |> List.fold (fun acc str -> min acc (strIndent str).Length) System.Int32.MaxValue

    input
    |> List.filter (fun str -> str.Length > 0)
    |> List.map (fun str -> str.Substring indent)

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
    | '^' -> Piece.Up
    | 'v' -> Piece.Down
    | '>' -> Piece.Right
    | '<' -> Piece.Left
    | _ -> failwith $"Invalid direction -{ch}-"

let charToTrack ch =
    match ch with
    | '^'
    | 'v'
    | '|' -> Piece.VertLine
    | '<'
    | '>'
    | '-' -> Piece.HorizLine
    | '/' -> Piece.CurveRight
    | '\\' -> Piece.CurveLeft
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
                          NextTurn = Turn.Left }

                    state.Carts <- cart :: state.Carts
                    state.Grid.Add(loc, charToTrack next.[x])
                | ch when isTrack ch -> state.Grid.Add({ X = x; Y = y }, charToTrack next.[x])
                | _ -> ()

            parseLines rest (y + 1)

    parseLines lines 0

let parse (input: string) =
    input.Split '\n'
    |> Array.toList
    |> trimIndent
    |> buildGrid

let run (input: string) =
    let state = parse input

    let bounds =
        state.Grid.Keys |> Seq.toList |> findBounds

    printfn $"STATE {state}"
