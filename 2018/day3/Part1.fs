module Aoc.Year2018.Day3.Part1

open utils.geometry

let addToGrid (grid: System.Int32 [,]) (rect: Parser.rectangle) =
    for row in rect.loc.y .. rect.loc.y + rect.dims.height - 1 do
        for col in rect.loc.x .. rect.loc.x + rect.dims.width - 1 do
            grid.[col, row] <- grid.[col, row] + 1

let findRectBounds (rect: Parser.rectangle) =
    (rect.loc,
     { x = rect.loc.x + rect.dims.width
       y = rect.loc.y + rect.dims.height })

let updateBounds (oldMin: Point, oldMax: Point) (newMin: Point, newMax: Point) =
    let minX = System.Math.Min(oldMin.x, newMin.x)
    let minY = System.Math.Min(oldMin.y, newMin.y)
    let maxX = System.Math.Max(oldMax.x, newMax.x)
    let maxY = System.Math.Max(oldMax.y, newMax.y)

    ({ x = minX; y = minY }, { x = maxX; y = maxY })

let findBounds rects =
    let minRect =
        { x = System.Int32.MaxValue
          y = System.Int32.MaxValue }

    let maxRect =
        { x = System.Int32.MinValue
          y = System.Int32.MinValue }

    Array.fold (fun acc r -> findRectBounds r |> updateBounds acc) (minRect, maxRect) rects

let createGrid rects =
    let (_, maxPt) = findBounds rects

    let grid =
        Array2D.init maxPt.x maxPt.y (fun x y -> 0)

    Array.iter (fun (r: Parser.rectangle) -> addToGrid grid r) rects

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
    |> utils.run.checkResult exp
