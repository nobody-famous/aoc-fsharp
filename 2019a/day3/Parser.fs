module Aoc.Year2019.Day3.Parser

open Aoc.Utils.Geometry

let toDelta (str: string) =
    let dist = str.Substring 1 |> int

    match str.[0] with
    | 'R' -> { X = dist; Y = 0 }
    | 'L' -> { X = -dist; Y = 0 }
    | 'U' -> { X = 0; Y = dist }
    | 'D' -> { X = 0; Y = -dist }
    | ch -> failwith $"Invalid direction {ch}"

let deltaToPoint acc delta =
    match acc with
    | head :: rest ->
        { X = head.X + delta.X
          Y = head.Y + delta.Y }
        :: head :: rest
    | _ -> acc

let pointsToLines points =
    let rec loop lines pts =
        match pts with
        | []
        | [ _ ] -> List.rev lines
        | first :: second :: rest -> loop ({ Pt1 = first; Pt2 = second } :: lines) (second :: rest)

    loop [] points

let parseLine (line: string) =
    line.Split ','
    |> Array.map toDelta
    |> Array.fold deltaToPoint [ origin ]
    |> List.rev
    |> pointsToLines
    |> List.toArray

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map parseLine
