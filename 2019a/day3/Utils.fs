module Aoc.Year2019.Day3.Utils

open Aoc.Utils.Geometry

let isVertical line = line.Pt1.X = line.Pt2.X

let isHorizontal line = line.Pt1.Y = line.Pt2.Y

let isBetween value first last =
    if first < last then
        value >= first && value <= last
    elif first > last then
        value >= last && value <= first
    else
        false

let findCross line1 line2 =
    if isVertical line1 && isHorizontal line2 then
        if isBetween line1.Pt1.X line2.Pt1.X line2.Pt2.X
           && isBetween line2.Pt1.Y line1.Pt1.Y line1.Pt2.Y then
            Some { X = line1.Pt1.X; Y = line2.Pt1.Y }
        else
            None
    elif isHorizontal line1 && isVertical line2 then
        if isBetween line1.Pt1.Y line2.Pt1.Y line2.Pt2.Y
           && isBetween line2.Pt1.X line1.Pt1.X line1.Pt2.X then
            Some { X = line2.Pt1.X; Y = line1.Pt1.Y }
        else
            None
    else
        None

let findCrosses (wires: Line [] []) =
    let wire1 = wires.[0]
    let wire2 = wires.[1]
    let mutable crosses = []

    for line1 in wire1 do
        for line2 in wire2 do
            match findCross line1 line2 with
            | None -> ()
            | Some pt ->
                if pt.X <> 0 && pt.Y <> 0 then
                    crosses <- pt :: crosses

    crosses
