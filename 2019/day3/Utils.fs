module aoc.year2019.day3.utils

open utils.geometry

let inRange x y z = x <= z && z <= y || x >= z && z >= y

let isHoriz line = line.pt1.y = line.pt2.y
let isVert line = line.pt1.x = line.pt2.x

let linesCross line1 line2 =
    inRange line1.pt1.y line1.pt2.y line2.pt1.y
    && inRange line2.pt1.x line2.pt2.x line1.pt1.x

let findLineCrosses line wire =
    List.fold
        (fun acc line2 ->
            if isHoriz line
               && isVert line2
               && linesCross line2 line then
                { x = line2.pt1.x; y = line.pt1.y } :: acc
            else if isHoriz line2
                    && isVert line
                    && linesCross line line2 then
                { x = line.pt1.x; y = line2.pt1.y } :: acc
            else
                acc)
        []

        wire

let findAllCrosses wire1 wire2 =
    List.fold (fun acc line -> findLineCrosses line wire2 @ acc) [] wire1
