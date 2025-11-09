module Aoc.Year2018.Day20.Utils

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
type DistMap = System.Collections.Generic.Dictionary<G.Point, int>

let buildGrid (input: string) =
    let grid = Grid()

    let updatePts pts dx dy door =
        List.map
            (fun (pt: G.Point) ->
                let newPt =
                    { G.X = pt.X + dx * 2
                      G.Y = pt.Y + dy * 2 }

                grid.[{ G.X = pt.X + dx; G.Y = pt.Y + dy }] <- door
                grid.[newPt] <- Room

                newPt)
            pts

    let rec walk startPts curPts (toRet: Set<G.Point>) rem =
        match rem with
        | first :: rest ->
            match first with
            | '^' -> walk startPts curPts toRet rest
            | 'N' ->
                let newPts = updatePts curPts 0 -1 HorizDoor
                walk startPts newPts toRet rest
            | 'S' ->
                let newPts = updatePts curPts 0 1 HorizDoor
                walk startPts newPts toRet rest
            | 'E' ->
                let newPts = updatePts curPts 1 0 VertDoor
                walk startPts newPts toRet rest
            | 'W' ->
                let newPts = updatePts curPts -1 0 VertDoor
                walk startPts newPts toRet rest
            | '(' ->
                let newCurPts, newRem = walk curPts curPts Set.empty rest
                walk startPts newCurPts toRet newRem
            | ')' ->
                let newToRet = Set.union toRet (Set.ofList curPts)
                Set.toList newToRet, rest
            | '|' ->
                let newToRet = Set.union toRet (Set.ofList curPts)
                walk startPts startPts newToRet rest
            | '$' -> Set.toList toRet, rest
            | _ -> failwith $"Should not be here {first}"
        | [] -> Set.toList toRet, []

    let startPt = { G.X = 0; G.Y = 0 }

    grid.[startPt] <- Room

    let _, _ =
        walk [ startPt ] [ startPt ] Set.empty (input |> Seq.toList)

    grid

let calcDists (grid: Grid) =
    let dists = DistMap()

    let addNeighbors ptSet (pt: G.Point) =
        let deltas = [ 0, 1; 0, -1; 1, 0; -1, 0 ]

        List.fold
            (fun acc (dx, dy) ->
                let newPt = { G.X = pt.X + dx; G.Y = pt.Y + dy }

                let toAdd =
                    { G.X = pt.X + dx * 2
                      G.Y = pt.Y + dy * 2 }

                if not (grid.ContainsKey newPt) then
                    acc
                else
                    match grid.[newPt] with
                    | VertDoor
                    | HorizDoor ->
                        if dists.ContainsKey toAdd then
                            acc
                        else
                            Set.add toAdd acc
                    | _ -> acc)
            ptSet
            deltas

    let rec loop curDist curPts =
        if not (Set.isEmpty curPts) then
            curPts
            |> Set.iter (fun pt -> dists.[pt] <- curDist)

            let newPts =
                curPts
                |> Set.fold (fun acc pt -> addNeighbors acc pt) Set.empty

            loop (curDist + 1) newPts

    loop 0 (Set.singleton { G.X = 0; G.Y = 0 })
    dists

let parse (input: string list) = input |> List.head |> buildGrid

let printGrid (grid: Grid) =
    let minPt, maxPt = G.findBounds (grid.Keys |> Seq.toList)

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
