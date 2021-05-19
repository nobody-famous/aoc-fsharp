module aoc.year2019.day3.part2

open System.Collections.Generic
open System.Threading.Tasks
open utils.geometry

let onLine line pt =
    (utils.isHoriz line
     && pt.y = line.pt1.y
     && utils.inRange line.pt1.x line.pt2.x pt.x)
    || (utils.isVert line
        && pt.x = line.pt1.x
        && utils.inRange line.pt1.y line.pt2.y pt.y)

let walkWire crosses wire =
    let steps = new Dictionary<Point, int>()

    let rec loop w dist =
        match w with
        | [] -> steps
        | line :: rest ->
            List.iter
                (fun pt ->
                    if onLine line pt then
                        steps.Add(pt, (dist + manDist line.pt1 pt)))
                crosses

            loop rest (dist + manDist line.pt1 line.pt2)

    loop wire 0

let startWalkTasks wire1 wire2 crosses =
    [ async { return walkWire crosses wire1 }
      |> Async.StartAsTask

      async { return walkWire crosses wire2 }
      |> Async.StartAsTask ]

let findLowest ((dists1: Dictionary<Point, int>), (dists2: Dictionary<Point, int>)) =
    dists1
    |> Seq.fold
        (fun low (KeyValue (pt, dist)) ->
            let total = dist + dists2.[pt]

            if total <> 0 && total < low then
                total
            else
                low)
        System.Int32.MaxValue

let getTaskResults (tasks: list<Task<Dictionary<Point, int>>>) = (tasks.[0].Result, tasks.[1].Result)

let run fileName =
    let wires = parser.parseInput fileName

    utils.findAllCrosses wires.[0] wires.[1]
    |> startWalkTasks wires.[0] wires.[1]
    |> getTaskResults
    |> findLowest
