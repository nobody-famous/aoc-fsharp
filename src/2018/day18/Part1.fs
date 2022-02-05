module Aoc.Year2018.Day18.Part1

module S = Aoc.Utils.String
module G = Aoc.Utils.Geometry

type Piece =
    | Empty
    | Tree
    | Yard

let parseLine y (line: string) (grid: Map<int * int, Piece>) =
    line
    |> Seq.mapi (fun x ch -> (x, ch))
    |> Seq.fold
        (fun oldGrid (x, ch) ->
            match ch with
            | '.' -> Map.add (x, y) Empty oldGrid
            | '|' -> Map.add (x, y) Tree oldGrid
            | '#' -> Map.add (x, y) Yard oldGrid
            | _ -> raise (System.Exception $"Invalid line {line}"))
        grid

let parse (input: string) =
    input.Split '\n'
    |> S.trimIndent
    |> Array.mapi (fun y line -> (y, line))
    |> Array.fold (fun grid (y, line) -> parseLine y line grid) Map.empty

let printGrid grid =
    let ((minX, minY), (maxX, maxY)) =
        G.findTupleBounds (Map.keys grid |> Seq.toList)

    for y in minY .. maxY do
        for x in minX .. maxX do
            if Map.containsKey (x, y) grid then
                match Map.find (x, y) grid with
                | Tree -> printf "|"
                | Yard -> printf "#"
                | Empty -> printf "."
            else
                printf "?"

        printfn ""

let getNeighbors (x, y) grid =
    [ for dx in -1 .. 1 do
          for dy in -1 .. 1 do
              if not (dx = 0 && dy = 0) then
                  yield (x + dx, y + dy) ]
    |> List.filter (fun pt -> Map.containsKey pt grid)
    |> List.map (fun pt -> Map.find pt grid)

let getCount (grid: Map<(int * int), Piece>) target =
    grid
    |> Seq.sumBy (fun kv ->
        match kv.Value with
        | v when v = target -> 1
        | _ -> 0)

let getNewPiece (x, y) grid =
    let countPieces pieceList target =
        pieceList
        |> List.sumBy (fun piece ->
            match piece with
            | p when p = target -> 1
            | _ -> 0)

    let updateEmpty neigbors =
        let numTrees = countPieces neigbors Tree

        if numTrees >= 3 then Tree else Empty

    let updateTree neigbors =
        let numYards = countPieces neigbors Yard

        if numYards >= 3 then Yard else Tree

    let updateYard neigbors =
        let numYards = countPieces neigbors Yard
        let numTrees = countPieces neigbors Tree

        if numYards >= 1 && numTrees >= 1 then
            Yard
        else
            Empty

    let neigbors = getNeighbors (x, y) grid

    match Map.find (x, y) grid with
    | Empty -> updateEmpty neigbors
    | Tree -> updateTree neigbors
    | Yard -> updateYard neigbors

let runMinute (grid: Map<(int * int), Piece>) =
    grid
    |> Seq.map (fun kv -> kv.Key)
    |> Seq.map (fun pt -> (pt, getNewPiece pt grid))
    |> Seq.fold (fun newGrid (pt, piece) -> Map.add pt piece newGrid) Map.empty

let getAnswer (grid: Map<(int * int), Piece>) =
    let trees = getCount grid Tree
    let yards = getCount grid Yard

    trees * yards

let run (input: string) =
    let grid = parse input

    let rec loop count curGrid =
        match count with
        | 10 -> curGrid
        | _ -> loop (count + 1) (runMinute curGrid)

    grid |> loop 0 |> getAnswer
