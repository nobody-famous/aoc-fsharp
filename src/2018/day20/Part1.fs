module Aoc.Year2018.Day20.Part1

module G = Aoc.Utils.Geometry

type Direction =
    | N
    | S
    | E
    | W

type Node =
    | Leaf of seq<Direction>
    | Branch of seq<Node>

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

type Options =
    | Leaf of seq<Direction>
    | Branch of seq<Options>

let rec parseBranch depth input =
    let rec loop curBranch curOpts rem =
        match rem with
        | first :: rest ->
            match first with
            | '(' ->
                let (subBranch, newRem) = parseBranch (depth + 1) rest

                loop curBranch curOpts newRem
            | ')' ->
                let newOpts = Seq.append curOpts curBranch
                (newOpts, rest)
            | '|' ->
                printfn $"OPTION BREAK {depth}"
                let newOpts = Seq.append curOpts curBranch

                loop Seq.empty newOpts rest
            | _ ->
                let (seg, newRem) = parseSegment rem

                let newBranch = Seq.append curBranch (Seq.singleton seg)

                printfn $"BRANCH {depth} {segToString seg}"
                loop newBranch curOpts newRem
        | _ -> (curOpts, rem)

    loop Seq.empty Seq.empty input

let parseRegex (input: string) =
    let rec loop rem =
        match rem with
        | first :: rest ->
            match first with
            | '^' -> loop rest
            | '$' -> ()
            | 'N'
            | 'S'
            | 'E'
            | 'W' ->
                let (seg, newRem) = parseSegment rem

                printfn $"REGEX SEG {segToString seg}"
                loop newRem
            | '(' ->
                let (_, newRem) = parseBranch 1 rest

                loop newRem
            | _ -> failwith $"Invalid input {first} {rest}"
        | [] -> ()

    loop (input |> Seq.toList)

let buildGrid (input: string) =
    printfn $"BUILD GRID {input}"
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

    let rec loop pts rem =
        match rem with
        | first :: rest ->
            match first with
            | 'N' ->
                let newPts = updatePts (List.head pts) 0 -1 HorizDoor
                loop (newPts :: List.tail pts) rest
            | 'S' ->
                let newPts = updatePts (List.head pts) 0 1 HorizDoor
                loop (newPts :: List.tail pts) rest
            | 'E' ->
                let newPts = updatePts (List.head pts) 1 0 VertDoor
                loop (newPts :: List.tail pts) rest
            | 'W' ->
                let newPts = updatePts (List.head pts) -1 0 VertDoor
                loop (newPts :: List.tail pts) rest
            | '^' -> loop pts rest
            |'|' ->
                printfn $"{pts} {List.head (List.tail pts)}"
                grid
            | '(' -> loop ((List.head pts) :: pts) rest
            | ')' -> loop (List.tail pts) rest
            | _ ->
                printfn $"buildGrid NOT DONE {first}"
                grid
        | [] -> grid

    let startPt = { G.X = 0; G.Y = 0 }

    grid.[startPt] <- Room

    loop [ [ startPt ] ] (input |> Seq.toList)

let parse (input: string) =
    input.Split '\n'
    |> Array.toList
    |> List.head
    |> buildGrid

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
    let grid = parse input

    // let grid = Grid()

    // for path in paths do
    //     walkPath path grid

    printGrid grid

    0
