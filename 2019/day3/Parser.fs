module Aoc.Year2019.Day3.Parser

open Aoc.Utils.Geometry

let parseDir (str: string) =
    let turn = str.[0]
    let dist = str.Substring 1 |> int

    match turn with
    | 'R' -> { X = dist; Y = 0 }
    | 'L' -> { X = -dist; Y = 0 }
    | 'U' -> { X = 0; Y = dist }
    | 'D' -> { X = 0; Y = -dist }
    | _ -> raise <| System.Exception $"Unhandled input {str}"

let toLines diffs =
    let (_, lines) =
        Array.fold
            (fun (prevPt, lines) pt ->
                let next =
                    { X = prevPt.X + pt.X
                      Y = prevPt.Y + pt.Y }

                (next, { Pt1 = prevPt; Pt2 = next } :: lines))
            ({ X = 0; Y = 0 }, [])
            diffs

    List.rev lines

let parseWire (line: string) = line.Split ',' |> Array.map parseDir

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map (parseWire >> toLines)
