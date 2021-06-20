module Aoc.Year2018.Day3.Part1

let countMultiples (grid: System.Int32 [,]) =
    let count = ref 0

    for x in 0 .. Array2D.length1 grid - 1 do
        for y in 0 .. Array2D.length2 grid - 1 do
            if grid.[x, y] > 1 then
                count := !count + 1

    !count

let run exp fileName =
    Parser.parseInput fileName
    |> Utils.createGrid
    |> countMultiples
    |> Aoc.Utils.Run.checkResult exp
