module Aoc.Year2018.Day6.Part1

open Aoc.Utils.Geometry

let closest point points =
    let candidates =
        Array.fold
            (fun (dist, ptList) pt ->
                match manDist point pt with
                | d when d < dist -> (d, [ pt ])
                | d when d = dist -> (d, pt :: ptList)
                | _ -> (dist, ptList))
            (System.Int32.MaxValue, [])
            points
        |> snd

    match List.length candidates with
    | 1 -> Some candidates.[0]
    | _ -> None

let fillGrid points =
    let (minPt, maxPt) = findBounds points

    let grid =
        Array2D.init (maxPt.X + 1) (maxPt.Y + 1) (fun _ _ -> None)

    for y in minPt.Y .. maxPt.Y do
        for x in minPt.X .. maxPt.X do
            match closest { X = x; Y = y } points with
            | Some pt -> grid.[x, y] <- Some pt
            | None -> ()

    grid

let printGrid grid =
    for y in 0 .. Array2D.length2 grid - 1 do
        for x in 0 .. Array2D.length1 grid - 1 do
            match grid.[x, y] with
            | Some pt -> printf $" ({pt.X},{pt.Y})"
            | None -> printf "      "

        printfn ""

let run exp fileName =
    Parser.parseInput fileName
    |> fillGrid
    |> printGrid
