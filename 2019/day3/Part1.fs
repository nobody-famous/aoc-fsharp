module aoc.year2019.day3.part1

open utils.geometry

let inRange x y z = x <= z && z <= y || x >= z && z >= y

let isHoriz line = line.head.y = line.tail.y
let isVert line = line.head.x = line.tail.x

let findLineCrosses line wire =
    List.fold
        (fun acc line2 ->
            if isHoriz line && isVert line2 then
                if inRange line2.head.y line2.tail.y line.head.y
                   && inRange line.head.x line.tail.x line2.head.x then
                    { x = line2.head.x; y = line.head.y } :: acc
                else
                    acc
            else if isVert line && isHoriz line2 then
                if inRange line2.head.x line2.tail.x line.head.x
                   && inRange line.head.y line.tail.y line2.head.y then
                    { x = line.head.x; y = line2.head.y } :: acc
                else
                    acc
            else
                acc)
        []

        wire

let findAllCrosses wire1 wire2 =
    List.fold (fun acc line -> findLineCrosses line wire2 @ acc) [] wire1

let run fileName =
    let wires = parser.parseInput fileName

    findAllCrosses wires.[0] wires.[1]
    |> List.map (fun cross -> manDist origin cross)
    |> List.fold (fun acc d -> if d <> 0 && d < acc then d else acc) System.Int32.MaxValue

