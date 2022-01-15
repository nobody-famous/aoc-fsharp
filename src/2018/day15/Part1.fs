module Aoc.Year2018.Day15.Part1

module G = Aoc.Utils.Geometry
module S = Aoc.Utils.String

type Piece =
    | Empty
    | Wall
    | Goblin of int
    | Elf of int

type Grid = System.Collections.Generic.Dictionary<G.Point, Piece>

let HitPoints = 200

let parseLines (lines: string array) =
    let grid = Grid()

    let parseRow y (line: string) =
        printfn $"LINE {y} {line}"

        Seq.iteri
            (fun x ch ->
                match ch with
                | '.' -> grid.Add({ G.X = x; G.Y = y }, Empty)
                | '#' -> grid.Add({ G.X = x; G.Y = y }, Wall)
                | 'G' -> grid.Add({ G.X = x; G.Y = y }, Goblin(HitPoints))
                | 'E' -> grid.Add({ G.X = x; G.Y = y }, Elf(HitPoints))
                | _ -> failwith $"Invalid character {int ch}")
            line

    Array.iteri parseRow lines
    grid

let parse (input: string) =
    input.Split '\n' |> S.trimIndent |> parseLines

let printGrid (grid: Grid) =
    let (minPt, maxPt) = grid.Keys |> Seq.toList |> G.findBounds

    for y in minPt.Y .. maxPt.Y do
        for x in minPt.X .. maxPt.X do
            match grid.TryGetValue { X = x; Y = y } with
            | true, Empty -> printf "."
            | true, Wall -> printf "#"
            | true, Goblin _ -> printf "G"
            | true, Elf _ -> printf "E"
            | status, piece -> failwith $"PRINT FAILED {status} {piece}"

        printfn ""

let run (input: string) =
    let grid = parse input

    printGrid grid

    0
