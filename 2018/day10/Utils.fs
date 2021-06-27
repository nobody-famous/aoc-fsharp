module Aoc.Year2018.Day10.Utils

open Aoc.Utils.Geometry

[<Struct>]
type LightPoint = { Pos: Point; Vel: Point }

let findMinMax points =
    points
    |> Array.fold
        (fun (minPt, maxPt) pt ->
            ({ X = (System.Math.Min(minPt.X, pt.Pos.X))
               Y = System.Math.Min(minPt.Y, pt.Pos.Y) },
             { X = System.Math.Max(maxPt.X, pt.Pos.X)
               Y = System.Math.Max(maxPt.Y, pt.Pos.Y) }))
        ({ X = System.Int32.MaxValue
           Y = System.Int32.MaxValue },
         { X = System.Int32.MinValue
           Y = System.Int32.MinValue })

let updatePoint pt =
    { pt with
          Pos =
              { X = pt.Pos.X + pt.Vel.X
                Y = pt.Pos.Y + pt.Vel.Y } }

let findBox points =
    let (minPt, maxPt) = findMinMax points

    (maxPt.X - minPt.X + 1, maxPt.Y - minPt.Y + 1)

let findMinGrid points =
    let rec round count pts =
        let newPts = Array.map updatePoint pts
        let (_, oldHeight) = findBox pts
        let (_, newHeight) = findBox newPts

        if newHeight > oldHeight then
            count
        else
            round (count + 1) newPts

    round 0 points
