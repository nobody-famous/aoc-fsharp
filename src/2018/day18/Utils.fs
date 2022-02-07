module Aoc.Year2018.Day18.Utils

module S = Aoc.Utils.String
module G = Aoc.Utils.Geometry

type Piece =
    | Empty
    | Tree
    | Yard

type Grid = System.Collections.Generic.Dictionary<(int * int), Piece>
type Board = Piece [,]

let parseLine y (line: string) (grid: Grid) =
    line
    |> Seq.mapi (fun x ch -> (x, ch))
    |> Seq.iter (fun (x, ch) ->
        match ch with
        | '.' -> grid.[(x, y)] <- Empty
        | '|' -> grid.[(x, y)] <- Tree
        | '#' -> grid.[(x, y)] <- Yard
        | _ -> raise (System.Exception $"Invalid line {line}"))

let parse (input: string) =
    let grid = Grid()

    input.Split '\n'
    |> S.trimIndent
    |> Array.mapi (fun y line -> (y, line))
    |> Array.iter (fun (y, line) -> parseLine y line grid)

    let ((minX, minY), (maxX, maxY)) =
        G.findTupleBounds (grid.Keys |> Seq.toList)

    Array2D.init (maxX - minX + 1) (maxY - minY + 1) (fun x y -> grid.[(x, y)])

let printBoard (board: Board) =
    for y in 0 .. board.GetLength(1) - 1 do
        for x in 0 .. board.GetLength(0) - 1 do
            match board.[x, y] with
            | Tree -> printf "|"
            | Yard -> printf "#"
            | Empty -> printf "."

        printfn ""

let isOnBoard x y (board: Board) =
    x >= 0
    && x < board.GetLength(0)
    && y >= 0
    && y < board.GetLength(1)

let getNeighbors (x, y) (board: Board) =
    [ (x - 1, y - 1)
      (x - 1, y)
      (x - 1, y + 1)
      (x, y - 1)
      (x, y + 1)
      (x + 1, y - 1)
      (x + 1, y)
      (x + 1, y + 1) ]
    |> List.filter (fun (x, y) -> isOnBoard x y board)
    |> List.map (fun (x, y) -> board.[x, y])

let getCount (board: Board) target =
    let mutable count = 0

    for x in 0 .. board.GetLength(0) - 1 do
        for y in 0 .. board.GetLength(1) - 1 do
            count <-
                count
                + match board.[x, y] with
                  | v when v = target -> 1
                  | _ -> 0

    count

let getNewPiece (x, y) (board: Board) =
    let countPieces pieceList target =
        pieceList
        |> List.sumBy (fun piece ->
            match piece with
            | p when p = target -> 1
            | _ -> 0)

    let neighbors = getNeighbors (x, y) board
    let numTrees = countPieces neighbors Tree
    let numYards = countPieces neighbors Yard

    match board.[x, y] with
    | Empty -> if numTrees >= 3 then Tree else Empty
    | Tree -> if numYards >= 3 then Yard else Tree
    | Yard ->
        if numYards >= 1 && numTrees >= 1 then
            Yard
        else
            Empty

let runMinute (board: Board) =
    let newBoard =
        Array2D.init (board.GetLength(0)) (board.GetLength(1)) (fun _ _ -> Empty)

    for x in 0 .. board.GetLength(0) - 1 do
        for y in 0 .. board.GetLength(1) - 1 do
            newBoard.[x, y] <- getNewPiece (x, y) board

    newBoard

let getResourceValue (board: Board) =
    let trees = getCount board Tree
    let yards = getCount board Yard

    trees * yards
