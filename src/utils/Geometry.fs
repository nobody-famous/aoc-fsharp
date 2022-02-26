module Aoc.Utils.Geometry

[<Struct>]
type Point = { X: int; Y: int }

[<Struct>]
type Point3d = { X3: int; Y3: int; Z3: int }

let origin = { X = 0; Y = 0 }

let origin3d = { X3 = 0; Y3 = 0; Z3 = 0 }

let smallerPt pt1 pt2 =
    { X = min pt1.X pt2.X
      Y = min pt1.Y pt2.Y }

let biggerPt pt1 pt2 =
    { X = max pt1.X pt2.X
      Y = max pt1.Y pt2.Y }

let findBounds (pts: Point list) : Point * Point =
    List.fold
        (fun (minPt, maxPt) (pt: Point) ->
            ({ X = min pt.X minPt.X
               Y = min pt.Y minPt.Y },
             { X = max pt.X maxPt.X
               Y = max pt.Y maxPt.Y }))
        ({ X = System.Int32.MaxValue
           Y = System.Int32.MaxValue },
         { X = System.Int32.MinValue
           Y = System.Int32.MinValue })
        pts

let findTupleBounds (pts: (int * int) list) =
    pts
    |> List.fold
        (fun ((minX, minY), (maxX, maxY)) (x, y) -> (min x minX, min y minY), (max x maxX, max y maxY))
        ((System.Int32.MaxValue, System.Int32.MaxValue), (System.Int32.MinValue, System.Int32.MinValue))

let manDist pt1 pt2 =
    abs (pt1.X - pt2.X) + abs (pt1.Y - pt2.Y)

let manDist3d pt1 pt2 =
    abs (pt1.X3 - pt2.X3)
    + abs (pt1.Y3 - pt2.Y3)
    + abs (pt1.Z3 - pt2.Z3)
