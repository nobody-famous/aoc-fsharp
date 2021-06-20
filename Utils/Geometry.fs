module Aoc.Utils.Geometry

type Point = { X: int; Y: int }

type Line = { Pt1: Point; Pt2: Point }

let origin = { X = 0; Y = 0 }

let manDist pt1 pt2 =
    abs (pt1.X - pt2.X) + abs (pt1.Y - pt2.Y)
