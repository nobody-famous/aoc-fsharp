module Aoc.Year2018.Day18.Utils

module S = Aoc.Utils.String
module G = Aoc.Utils.Geometry

type Piece =
    | Empty
    | Tree
    | Yard

type Grid = System.Collections.Generic.Dictionary<(int * int), Piece>

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

    grid

let printGrid (grid: Grid) =
    let ((minX, minY), (maxX, maxY)) =
        grid.Keys |> Seq.toList |> G.findTupleBounds

    for y in minY .. maxY do
        for x in minX .. maxX do
            if grid.ContainsKey(x, y) then
                match grid.[(x, y)] with
                | Tree -> printf "|"
                | Yard -> printf "#"
                | Empty -> printf "."
            else
                printf "?"

        printfn ""

let getNeighbors (x, y) (grid: Grid) =
    [ for dx in -1 .. 1 do
          for dy in -1 .. 1 do
              if not (dx = 0 && dy = 0) then
                  yield (x + dx, y + dy) ]
    |> List.filter (fun pt -> grid.ContainsKey pt)
    |> List.map (fun pt -> grid.[pt])

let getCount (grid: Grid) target =
    grid
    |> Seq.sumBy (fun kv ->
        match kv.Value with
        | v when v = target -> 1
        | _ -> 0)

let getNewPiece (x, y) (grid: Grid) =
    let countPieces pieceList target =
        pieceList
        |> List.sumBy (fun piece ->
            match piece with
            | p when p = target -> 1
            | _ -> 0)

    let neigbors = getNeighbors (x, y) grid
    let numTrees = countPieces neigbors Tree
    let numYards = countPieces neigbors Yard

    match grid.[(x, y)] with
    | Empty -> if numTrees >= 3 then Tree else Empty
    | Tree -> if numYards >= 3 then Yard else Tree
    | Yard ->
        if numYards >= 1 && numTrees >= 1 then
            Yard
        else
            Empty

let runMinute (grid: Grid) =
    let newGrid = Grid()

    grid
    |> Seq.iter (fun kv -> newGrid.[kv.Key] <- getNewPiece kv.Key grid)

    newGrid

let getResourceValue (grid: Grid) =
    let trees = getCount grid Tree
    let yards = getCount grid Yard

    trees * yards
