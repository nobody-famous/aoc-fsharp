module Aoc.Year2018.Day17.Part1

module S = Aoc.Utils.String
module G = Aoc.Utils.Geometry

type Piece =
    | Spring
    | Water
    | Clay

type Grid = Map<G.Point, Piece>

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
    pts
    |> List.fold (fun m pt -> Map.add pt Clay m) (Map<G.Point, Piece>([ { G.X = 500; G.Y = 0 }, Spring ]))

let printGrid (grid: Grid) =
    let (minPt, maxPt) =
        G.findBounds (Map.keys grid |> Seq.toList)

    for y in minPt.Y .. maxPt.Y do
        for x in minPt.X .. maxPt.X do
            let pt = { G.X = x; G.Y = y }

            if Map.containsKey pt grid then
                match Map.find pt grid with
                | Clay -> printf "#"
                | Water -> printf "~"
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

let fillWater (startPt: G.Point) (grid: Grid) =
    let (minPt, maxPt) =
        G.findBounds (Map.keys grid |> Seq.toList)

    let rec doDrop toDrop curGrid =
        let rec verticalDrop nextPt curGrid =
            match nextPt with
            | pt when Map.containsKey pt curGrid ->
                match Map.find pt curGrid with
                | Water -> (pt, curGrid)
                | _ -> ({ pt with G.Y = pt.Y - 1 }, curGrid)
            | pt when pt.Y > maxPt.Y || pt.Y < minPt.Y -> (pt, curGrid)
            | pt -> verticalDrop { pt with G.Y = pt.Y + 1 } (Map.add pt Water curGrid)

        let rec horizontalFill (startPt: G.Point) curGrid =
            let rec fill dx nextPt nextDrops curGrid =
                match nextPt with
                | pt when Map.containsKey pt curGrid -> (nextDrops, curGrid)
                | pt when not (Map.containsKey { pt with G.Y = pt.Y + 1 } curGrid) ->
                    (pt :: nextDrops, (Map.add pt Water curGrid))
                | pt -> fill dx { pt with G.X = pt.X + dx } nextDrops (Map.add pt Water curGrid)

            if startPt.Y > maxPt.Y || startPt.Y < minPt.Y then
                ([], curGrid)
            else if Map.containsKey { startPt with G.X = startPt.X + 1 } curGrid
                    && Map.containsKey { startPt with G.X = startPt.X - 1 } curGrid then
                ([], curGrid)
            else
                let (newToDrop, newGrid) =
                    curGrid
                    |> fill 1 { startPt with G.X = startPt.X + 1 } []
                    ||> fill -1 { startPt with G.X = startPt.X - 1 }

                if List.isEmpty newToDrop then
                    horizontalFill { startPt with G.Y = startPt.Y - 1 } newGrid
                else
                    (newToDrop, newGrid)

        match toDrop with
        | pt :: rest ->
            let (newToDrop, newGrid) =
                curGrid
                |> verticalDrop { pt with G.Y = pt.Y + 1 }
                ||> horizontalFill

            doDrop (List.append rest newToDrop) newGrid
        | [] -> curGrid

    doDrop [ startPt ] grid

let debugPrint grid =
    printGrid grid
    grid

let run (input: string) =
    let grid = parse input

    grid
    |> fillWater { G.X = 500; G.Y = 0 }
    |> debugPrint
    |> Map.values
    |> Seq.sumBy (fun v ->
        match v with
        | Water -> 1
        | _ -> 0)
