module Aoc.Year2019.Day3.Utils

open Aoc.Utils.Geometry

let inRange x y z = x <= z && z <= y || x >= z && z >= y

let isHoriz line = line.Pt1.Y = line.Pt2.Y
let isVert line = line.Pt1.X = line.Pt2.X

let linesCross line1 line2 =
    inRange line1.Pt1.Y line1.Pt2.Y line2.Pt1.Y
    && inRange line2.Pt1.X line2.Pt2.X line1.Pt1.X

let findLineCrosses line wire =
    List.fold
        (fun acc line2 ->
            if isHoriz line
               && isVert line2
               && linesCross line2 line then
                { X = line2.Pt1.X; Y = line.Pt1.Y } :: acc
            else if isHoriz line2
                    && isVert line
                    && linesCross line line2 then
                { X = line.Pt1.X; Y = line2.Pt1.Y } :: acc
            else
                acc)
        []

        wire

let findAllCrosses wire1 wire2 =
    List.fold (fun acc line -> findLineCrosses line wire2 @ acc) [] wire1
