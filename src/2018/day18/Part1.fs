module Aoc.Year2018.Day18.Part1

module S = Aoc.Utils.String
module G = Aoc.Utils.Geometry

type Piece =
    | Tree
    | Yard

let parseLine y (line: string) (grid: Map<int * int, Piece>) =
    line
    |> Seq.mapi (fun x ch -> (x, ch))
    |> Seq.fold
        (fun oldGrid (x, ch) ->
            match ch with
            | '.' -> oldGrid
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
            else
                printf "."

        printfn ""

let run (input: string) =
    let grid = parse input

    printGrid grid

    0
