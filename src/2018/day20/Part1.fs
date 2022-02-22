module Aoc.Year2018.Day20.Part1

module G = Aoc.Utils.Geometry

type Direction =
    | N
    | S
    | E
    | W

type Piece =
    | Wall
    | HorizDoor
    | VertDoor
    | Room

type Grid = System.Collections.Generic.Dictionary<G.Point, Piece>

let buildGrid (input: string) =
    let grid = Grid()

    let updatePts pts dx dy door =
        List.map
            (fun (pt: G.Point) ->
                let newPt =
                    { G.X = pt.X + (dx * 2)
                      G.Y = pt.Y + (dy * 2) }

                grid.[{ G.X = pt.X + dx; G.Y = pt.Y + dy }] <- door
                grid.[newPt] <- Room

                newPt)
            pts

    let rec walk startPts curPts rem =
        match rem with
        | first :: rest ->
            match first with
            | '^' -> walk startPts curPts rest
            | 'N' ->
                let newPts = updatePts curPts 0 -1 HorizDoor
                walk startPts newPts rest
            | 'S' ->
                let newPts = updatePts curPts 0 1 HorizDoor
                walk startPts newPts rest
            | 'E' ->
                let newPts = updatePts curPts 1 0 VertDoor
                walk startPts newPts rest
            | 'W' ->
                let newPts = updatePts curPts -1 0 VertDoor
                walk startPts newPts rest
            | '(' ->
                let (newCurPts, newRem) = walk curPts curPts rest
                walk startPts newCurPts newRem
            | ')' -> (curPts, rest)
            | '|' -> walk startPts startPts rest
            | '$' -> (curPts, rest)
            | _ -> failwith $"Should not be here {first}"
        | [] -> (curPts, [])

    let startPt = { G.X = 0; G.Y = 0 }

    grid.[startPt] <- Room

    walk [ startPt ] [ startPt ] (input |> Seq.toList)
    |> ignore

    grid

let parse (input: string) =
    input.Split '\n'
    |> Array.toList
    |> List.head
    |> buildGrid

let printGrid (grid: Grid) =
    let (minPt, maxPt) = G.findBounds (grid.Keys |> Seq.toList)

    for y in minPt.Y - 1 .. maxPt.Y + 1 do
        for x in minPt.X - 1 .. maxPt.X + 1 do
            let pt = { G.X = x; G.Y = y }

            let value =
                if grid.ContainsKey(pt) then
                    grid.[pt]
                else
                    Wall

            match value with
            | Wall -> printf "#"
            | HorizDoor -> printf "-"
            | VertDoor -> printf "|"
            | Room -> printf " "

        printfn ""

let run (input: string) =
    let grid = parse input

    printGrid grid

    0
