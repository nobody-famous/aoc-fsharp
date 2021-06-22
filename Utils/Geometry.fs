module Aoc.Utils.Geometry

type Point = { X: int; Y: int }

type Line = { Pt1: Point; Pt2: Point }

let origin = { X = 0; Y = 0 }

let manDist pt1 pt2 =
    abs (pt1.X - pt2.X) + abs (pt1.Y - pt2.Y)

let makePoint x y = { X = x; Y = y }

let findBounds points =
    Array.fold
        (fun (minPt, maxPt) pt ->
            ({ X = (System.Math.Min(minPt.X, pt.X))
               Y = System.Math.Min(minPt.Y, pt.Y) },
             { X = System.Math.Max(maxPt.X, pt.X)
               Y = System.Math.Max(maxPt.Y, pt.Y) }))
        ({ X = System.Int32.MaxValue
           Y = System.Int32.MaxValue },
         { X = System.Int32.MinValue
           Y = System.Int32.MinValue })
        points
