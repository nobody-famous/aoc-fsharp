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

let charToDir ch =
    match ch with
    | 'N' -> N
    | 'S' -> S
    | 'E' -> E
    | 'W' -> W
    | _ -> failwith $"charToDir: {ch}"

let addToSegment dir seg = Seq.append seg (Seq.singleton dir)

let segToString seg =
    seg |> Seq.map string |> String.concat ""

let parseSegment input =
    let rec loop curSeg rem =
        match rem with
        | first :: rest ->
            match first with
            | 'N'
            | 'S'
            | 'E'
            | 'W' -> loop (addToSegment (charToDir first) curSeg) rest
            | _ -> (curSeg, rem)
        | [] -> (curSeg, [])

    loop Seq.empty input

let rec parseBranch paths input =
    let rec loop curPaths outPaths rem =
        match rem with
        | first :: rest ->
            match first with
            | '(' ->
                let (newPaths, newRem) = parseBranch curPaths rest

                loop newPaths outPaths newRem
            | ')' ->
                let newOutPaths =
                    if Seq.isEmpty curPaths then
                        Seq.append outPaths paths
                    else
                        Seq.append outPaths curPaths

                (newOutPaths, rest)
            | '|' ->
                let newOutPaths = Seq.append outPaths curPaths
                loop paths newOutPaths rest
            | _ ->
                let (seg, newRem) = parseSegment rem

                let newCurPaths =
                    Seq.map (fun s -> Seq.append s seg) curPaths

                loop newCurPaths outPaths newRem
        | _ -> (outPaths, rem)

    loop paths Seq.empty input

let parseRegex (input: string) =
    let rec loop paths rem =
        match rem with
        | first :: rest ->
            match first with
            | '^' -> loop Seq.empty rest
            | '$' -> paths
            | 'N'
            | 'S'
            | 'E'
            | 'W' ->
                let (seg, newRem) = parseSegment rem

                let newPaths =
                    if Seq.isEmpty paths then
                        Seq.append paths (Seq.singleton seg)
                    else
                        Seq.map (fun s -> Seq.append s seg) paths

                loop newPaths newRem
            | '(' ->
                let (newPaths, newRem) = parseBranch paths rest

                loop newPaths newRem
            | _ -> failwith $"Invalid input {first} {rest}"
        | [] -> paths

    loop Seq.empty (input |> Seq.toList)

let parse (input: string) =
    input.Split '\n'
    |> Array.toList
    |> List.head
    |> parseRegex

let walkPath path (grid: Grid) =
    let rec loop (curPt: G.Point) rem =
        grid.[curPt] <- Room

        match rem with
        | first :: rest ->
            match first with
            | N ->
                grid.[{ curPt with G.Y = curPt.Y - 1 }] <- HorizDoor
                loop { curPt with G.Y = curPt.Y - 2 } rest
            | S ->
                grid.[{ curPt with G.Y = curPt.Y + 1 }] <- HorizDoor
                loop { curPt with G.Y = curPt.Y + 2 } rest
            | E ->
                grid.[{ curPt with G.X = curPt.X + 1 }] <- VertDoor
                loop { curPt with G.X = curPt.X + 2 } rest
            | W ->
                grid.[{ curPt with G.X = curPt.X - 1 }] <- VertDoor
                loop { curPt with G.X = curPt.X - 2 } rest
        | [] -> ()

    loop { G.X = 0; G.Y = 0 } (path |> Seq.toList)

let printGrid (grid: Grid) =
    let (minPt, maxPt) = G.findBounds (grid.Keys |> Seq.toList)

    for y in minPt.Y - 1 .. maxPt.Y + 1 do
        for x in minPt.X - 1 .. maxPt.X + 1 do
            let pt = { G.X = x; G.Y = y }
            let mutable piece = Wall

            let value =
                if grid.TryGetValue(pt, &piece) then
                    piece
                else
                    Wall

            match value with
            | Wall -> printf "#"
            | HorizDoor -> printf "-"
            | VertDoor -> printf "|"
            | Room -> printf " "

        printfn ""

let run (input: string) =
    let paths = parse input

    let grid = Grid()

    for path in paths do
        walkPath path grid

    printGrid grid

    0
