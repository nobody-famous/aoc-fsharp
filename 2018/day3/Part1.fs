module Aoc.Year2018.Day3.Part1

open Utils.Geometry

let addToGrid (grid: System.Int32 [,]) (rect: Parser.Rectangle) =
    for row in rect.Loc.Y .. rect.Loc.Y + rect.Dims.Height - 1 do
        for col in rect.Loc.X .. rect.Loc.X + rect.Dims.Width - 1 do
            grid.[col, row] <- grid.[col, row] + 1

let findRectBounds (rect: Parser.Rectangle) =
    (rect.Loc,
     { X = rect.Loc.X + rect.Dims.Width
       Y = rect.Loc.Y + rect.Dims.Height })

let updateBounds (oldMin: Point, oldMax: Point) (newMin: Point, newMax: Point) =
    let minX = System.Math.Min(oldMin.X, newMin.X)
    let minY = System.Math.Min(oldMin.Y, newMin.Y)
    let maxX = System.Math.Max(oldMax.X, newMax.X)
    let maxY = System.Math.Max(oldMax.Y, newMax.Y)

    ({ X = minX; Y = minY }, { X = maxX; Y = maxY })

let findBounds rects =
    let minRect =
        { X = System.Int32.MaxValue
          Y = System.Int32.MaxValue }

    let maxRect =
        { X = System.Int32.MinValue
          Y = System.Int32.MinValue }

    Array.fold (fun acc r -> findRectBounds r |> updateBounds acc) (minRect, maxRect) rects

let createGrid rects =
    let (_, maxPt) = findBounds rects

    let grid =
        Array2D.init maxPt.X maxPt.Y (fun x y -> 0)

    Array.iter (fun (r: Parser.Rectangle) -> addToGrid grid r) rects

    grid

let countMultiples (grid: System.Int32 [,]) =
    let count = ref 0

    for x in 0 .. Array2D.length1 grid - 1 do
        for y in 0 .. Array2D.length2 grid - 1 do
            if grid.[x, y] > 1 then
                count := !count + 1

    !count

let run exp fileName =
    Parser.parseInput fileName
    |> createGrid
    |> countMultiples
    |> Utils.Run.checkResult exp
