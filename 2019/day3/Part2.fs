module aoc.year2019.day3.part2

open System.Collections.Generic
open utils.geometry

let onLine line pt =
    (utils.isHoriz line
     && pt.y = line.head.y
     && utils.inRange line.head.x line.tail.x pt.x)
    || (utils.isVert line
        && pt.x = line.head.x
        && utils.inRange line.head.y line.tail.y pt.y)

let walkWire crosses wire =
    let steps = new Dictionary<Point, int>()

    let rec loop w dist =
        match w with
        | [] -> steps
        | line :: rest ->
            List.iter
                (fun pt ->
                    if onLine line pt then
                        steps.Add(pt, (dist + manDist line.head pt)))
                crosses

            loop rest (dist + manDist line.head line.tail)

    loop wire 0

let startWalkTasks wire1 wire2 crosses =
    [ async { return walkWire crosses wire1 }
      |> Async.StartAsTask

      async { return walkWire crosses wire2 }
      |> Async.StartAsTask ]

let run fileName =
    let wires = parser.parseInput fileName

    let tasks =
        utils.findAllCrosses wires.[0] wires.[1]
        |> startWalkTasks wires.[0] wires.[1]

    for entry in tasks.[0].Result do
        printfn "%d,%d %d" entry.Key.x entry.Key.y entry.Value

    0
