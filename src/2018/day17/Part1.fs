module Aoc.Year2018.Day17.Part1

module S = Aoc.Utils.String
module G = Aoc.Utils.Geometry

type Piece =
    | Spring
    | Water
    | Clay

type Grid =
    { Pieces: Map<G.Point, Piece>
      MinPt: G.Point
      MaxPt: G.Point }

let addToGrid pt piece grid =
    let minPt = G.smallerPt pt grid.MinPt
    let maxPt = G.biggerPt pt grid.MaxPt

    { Pieces = Map.add pt piece grid.Pieces
      MinPt = minPt
      MaxPt = maxPt }

let toRange (str: string) =
    let first = str.IndexOf '.'
    let last = str.LastIndexOf '.'

    if first < 0 then
        seq { int str }
    else
        let startValue = str.Substring(0, first) |> int
        let endValue = str.Substring(last + 1) |> int

        seq { startValue .. endValue }

let parseLine (line: string) =
    let parts =
        line.Split ','
        |> Array.map (fun s -> s.Trim())
        |> Array.map (fun s -> s.Split '=')
        |> Array.map (fun ss -> (ss.[0], ss.[1]))
        |> Array.map (fun (v, rangeStr) -> (v, toRange rangeStr))
        |> Array.sortBy (fun (v, _) -> v)

    [ for x in snd parts.[0] do
          for y in snd parts.[1] do
              yield { G.X = x; G.Y = y } ]

let toGrid (pts: G.Point list) =
    let board =
        pts
        |> List.fold (fun m pt -> Map.add pt Clay m) (Map<G.Point, Piece>([ { G.X = 500; G.Y = 0 }, Spring ]))

    let (minPt, maxPt) =
        G.findBounds (Map.keys board |> Seq.toList)

    { Pieces = board
      MinPt = minPt
      MaxPt = maxPt }

let printGrid (grid: Grid) =
    for y in grid.MinPt.Y .. grid.MaxPt.Y do
        for x in grid.MinPt.X .. grid.MaxPt.X do
            let pt = { G.X = x; G.Y = y }

            if Map.containsKey pt grid.Pieces then
                match Map.find pt grid.Pieces with
                | Clay -> printf "#"
                | Water -> printf "."
                | Spring -> printf "+"
            else
                printf " "

        printfn ""

let parse (input: string) =
    input.Split '\n'
    |> S.trimIndent
    |> Array.toList
    |> List.map parseLine
    |> List.concat
    |> toGrid

let vertical pt (grid: Grid) =
    let rec loop curPt curGrid =
        match curPt with
        | pt when Map.containsKey pt curGrid.Pieces ->
            match Map.find pt curGrid.Pieces with
            | Water -> (pt, curGrid)
            | _ -> ({ pt with G.Y = pt.Y - 1 }, curGrid)
        | pt when pt.Y > grid.MaxPt.Y -> (pt, curGrid)
        | pt -> loop { pt with G.Y = pt.Y + 1 } (addToGrid pt Water curGrid)

    loop { pt with G.Y = pt.Y + 1 } grid

let rec horizontal (pt: G.Point) (origGrid: Grid) =
    let rec loop pt dropSet (grid: Grid) =
        let rec fill dx (curPt: G.Point) curDropSet curGrid =
            match curPt with
            | pt when not (Map.containsKey { pt with G.Y = pt.Y + 1 } curGrid.Pieces) ->
                (Set.add pt curDropSet, addToGrid pt Water curGrid)
            | pt when Map.containsKey pt curGrid.Pieces ->
                match Map.find pt curGrid.Pieces with
                | Water -> fill dx { pt with G.X = pt.X + dx } curDropSet curGrid
                | _ -> (curDropSet, curGrid)
            | pt -> fill dx { pt with G.X = pt.X + dx } curDropSet (addToGrid pt Water curGrid)

        let (newDropSet, newGrid) =
            (dropSet, addToGrid pt Water grid)
            ||> fill 1 { pt with G.X = pt.X + 1 }
            ||> fill -1 { pt with G.X = pt.X - 1 }

        if Set.isEmpty newDropSet then
            loop { pt with G.Y = pt.Y - 1 } newDropSet newGrid
        else
            (newDropSet, newGrid)

    if pt.Y > origGrid.MaxPt.Y then
        (Set.empty, origGrid)
    else
        loop pt Set.empty origGrid

let doAllDrops (dropSet: Set<G.Point>) (grid: Grid) =
    dropSet
    |> Set.fold
        (fun (endPoints, oldGrid) pt ->
            let (endPt, newGrid) = vertical pt oldGrid

            ((Set.add endPt endPoints), newGrid))
        (Set.empty, grid)

let fillAll (pts: Set<G.Point>) (grid: Grid) =
    pts
    |> Set.fold
        (fun (oldDrops, oldGrid) pt ->
            let (newDrops, newGrid) = horizontal pt oldGrid
            (Set.union newDrops oldDrops, newGrid))
        (Set.empty, grid)

let fillWater (startPt: G.Point) (grid: Grid) =
    let rec loop toDrop curGrid count =
        if count > 5 || Set.isEmpty toDrop then
            curGrid
        else
            let (toFill, newGrid) = doAllDrops toDrop curGrid
            let (newToDrop, newGrid) = fillAll toFill newGrid

            loop newToDrop newGrid (count + 1)

    loop (Set.add startPt Set.empty) grid 0

let debugPrint grid =
    printGrid grid
    grid

let sumWater (grid: Grid) =
    grid.Pieces
    |> Map.values
    |> Seq.sumBy (fun v ->
        match v with
        | Water -> 1
        | _ -> 0)

let run (input: string) =
    let grid = parse input

    grid
    |> fillWater { G.X = 500; G.Y = 0 }
    |> debugPrint
    |> sumWater
