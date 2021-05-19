module utils.geometry

type Point = { x: int; y: int }

type Line = { pt1: Point; pt2: Point }

let origin = { x = 0; y = 0 }

let manDist pt1 pt2 =
    abs (pt1.x - pt2.x) + abs (pt1.y - pt2.y)
