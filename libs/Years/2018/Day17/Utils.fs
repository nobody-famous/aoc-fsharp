module Aoc.Year2018.Day17.Utils

module S = Aoc.Utils.String
module G = Aoc.Utils.Geometry

type Piece =
    | Spring
    | DropWater
    | FillWater
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

let isClay pt (grid: Grid) =
    if not (Map.containsKey pt grid.Pieces) then
        false
    else
        match Map.find pt grid.Pieces with
        | Clay -> true
        | _ -> false

let isFillWater pt (grid: Grid) =
    if not (Map.containsKey pt grid.Pieces) then
        false
    else
        match Map.find pt grid.Pieces with
        | FillWater -> true
        | _ -> false

let isDropWater pt (grid: Grid) =
    if not (Map.containsKey pt grid.Pieces) then
        false
    else
        match Map.find pt grid.Pieces with
        | DropWater -> true
        | _ -> false

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

let parse (input: string list) =
    input
    |> List.toArray
    |> S.trimIndent
    |> Array.toList
    |> List.map parseLine
    |> List.concat
    |> toGrid

let printGrid (grid: Grid) =
    let (minPt, maxPt) =
        G.findBounds (Map.keys grid.Pieces |> Seq.toList)

    for y in minPt.Y .. maxPt.Y do
        for x in minPt.X .. maxPt.X do
            let pt = { G.X = x; G.Y = y }

            if Map.containsKey pt grid.Pieces then
                match Map.find pt grid.Pieces with
                | Clay -> printf "#"
                | DropWater -> printf "|"
                | FillWater -> printf "."
                | Spring -> printf "+"
            else
                printf " "

        printfn ""

let vertical pt (grid: Grid) =
    let rec loop curPt curGrid =
        match curPt with
        | pt when Map.containsKey pt curGrid.Pieces ->
            match Map.find pt curGrid.Pieces with
            | FillWater -> (pt, curGrid)
            | _ -> ({ pt with G.Y = pt.Y - 1 }, curGrid)
        | pt when pt.Y > grid.MaxPt.Y -> (pt, curGrid)
        | pt -> loop { pt with G.Y = pt.Y + 1 } (addToGrid pt DropWater curGrid)

    loop { pt with G.Y = pt.Y + 1 } grid

let rec horizontal (startPt: G.Point) (origGrid: Grid) =
    let rec loop (pt: G.Point) dropSet (grid: Grid) =
        let rec fill dx (curPt: G.Point) curDropSet curGrid =
            let nextPt = { curPt with G.X = curPt.X + dx }
            let belowPt = { curPt with G.Y = curPt.Y + 1 }

            match curPt with
            | pt when not (Map.containsKey belowPt curGrid.Pieces) ->
                (false, Set.add pt curDropSet, addToGrid pt FillWater curGrid)
            | _ when isDropWater belowPt curGrid -> (false, curDropSet, curGrid)
            | pt when Map.containsKey pt curGrid.Pieces ->
                match Map.find pt curGrid.Pieces with
                | FillWater -> fill dx nextPt curDropSet curGrid
                | DropWater -> fill dx nextPt curDropSet (addToGrid pt FillWater curGrid)
                | _ -> (true, curDropSet, curGrid)
            | pt -> fill dx nextPt curDropSet (addToGrid pt FillWater curGrid)

        let newGrid = addToGrid pt FillWater grid

        let (rightDone, newDropSet, newGrid) =
            fill 1 { pt with G.X = pt.X + 1 } dropSet newGrid

        let (leftDone, newDropSet, newGrid) =
            fill -1 { pt with G.X = pt.X - 1 } newDropSet newGrid

        if rightDone && leftDone then
            loop { pt with G.Y = pt.Y - 1 } newDropSet newGrid
        else
            (newDropSet, newGrid)

    if startPt.Y > origGrid.MaxPt.Y then
        (Set.empty, origGrid)
    else
        loop startPt Set.empty origGrid

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
        if Set.isEmpty toDrop then
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
    let minClay =
        grid.Pieces
        |> Seq.minBy (fun entry ->
            match entry.Value with
            | Clay -> entry.Key.Y
            | _ -> System.Int32.MaxValue)

    let minY = minClay.Key.Y

    let maxClay =
        grid.Pieces
        |> Seq.maxBy (fun entry ->
            match entry.Value with
            | Clay -> entry.Key.Y
            | _ -> System.Int32.MinValue)

    let maxY = maxClay.Key.Y

    grid.Pieces
    |> Seq.sumBy (fun v ->
        if v.Key.Y < minY || v.Key.Y > maxY then
            0
        else
            match v.Value with
            | FillWater
            | DropWater -> 1
            | _ -> 0)
