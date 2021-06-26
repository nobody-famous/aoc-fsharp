module Aoc.Year2018.Day6.Part1

open System.Collections.Generic
open Aoc.Utils.Geometry

type GridBounds = { MinPt: Point; MaxPt: Point }

type Grid =
    { Bounds: GridBounds
      Cells: Point option [,] }

let createBounds minPt maxPt = { MinPt = minPt; MaxPt = maxPt }

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

let fillGrid points bounds =
    let grid =
        Array2D.create (bounds.MaxPt.X + 1) (bounds.MaxPt.Y + 1) None

    for y in bounds.MinPt.Y .. bounds.MaxPt.Y do
        for x in bounds.MinPt.X .. bounds.MaxPt.X do
            match closest { X = x; Y = y } points with
            | Some pt -> grid.[x, y] <- Some pt
            | None -> ()

    grid

let createGrid points =
    let bounds = findBounds points ||> createBounds
    let cells = fillGrid points bounds

    { Bounds = bounds; Cells = cells }

let findInfinite grid =
    let infinite = Dictionary<Point, bool>()

    for x in grid.Bounds.MinPt.X .. grid.Bounds.MaxPt.X do
        match grid.Cells.[x, grid.Bounds.MinPt.Y] with
        | None -> ()
        | Some pt -> infinite.[pt] <- true

        match grid.Cells.[x, grid.Bounds.MaxPt.Y] with
        | None -> ()
        | Some pt -> infinite.[pt] <- true

    for y in grid.Bounds.MinPt.Y .. grid.Bounds.MaxPt.Y do
        match grid.Cells.[grid.Bounds.MinPt.X, y] with
        | None -> ()
        | Some pt -> infinite.[pt] <- true

        match grid.Cells.[grid.Bounds.MaxPt.X, y] with
        | None -> ()
        | Some pt -> infinite.[pt] <- true

    infinite

let incAreasEntry (areas: Dictionary<Point, int>) key =
    if areas.ContainsKey key then
        areas.[key] <- areas.[key] + 1
    else
        areas.[key] <- 1

let findAreas grid (infinite: Dictionary<Point, bool>) =
    let areas = Dictionary<Point, int>()

    for x in grid.Bounds.MinPt.X .. grid.Bounds.MaxPt.X do
        for y in grid.Bounds.MinPt.Y .. grid.Bounds.MaxPt.Y do
            match grid.Cells.[x, y] with
            | Some pt when not (infinite.ContainsKey pt) -> incAreasEntry areas pt
            | None
            | _ -> ()

    areas

let maxArea (areas: Dictionary<Point, int>) =
    Seq.toList areas
    |> Seq.fold (fun high (KeyValue (_, area)) -> if area > high then area else high) 0

let run exp fileName =
    let grid = Parser.parseInput fileName |> createGrid

    findInfinite grid
    |> findAreas grid
    |> maxArea
    |> Aoc.Utils.Run.checkResult exp
