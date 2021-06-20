module Aoc.Year2019.Day3.Part2

open System.Collections.Generic
open System.Threading.Tasks
open Utils.Geometry

let onLine line pt =
    (Utils.isHoriz line
     && pt.Y = line.Pt1.Y
     && Utils.inRange line.Pt1.X line.Pt2.X pt.X)
    || (Utils.isVert line
        && pt.X = line.Pt1.X
        && Utils.inRange line.Pt1.Y line.Pt2.Y pt.Y)

let walkWire crosses wire =
    let steps = new Dictionary<Point, int>()

    let rec loop w dist =
        match w with
        | [] -> steps
        | line :: rest ->
            List.iter
                (fun pt ->
                    if onLine line pt then
                        steps.Add(pt, (dist + manDist line.Pt1 pt)))
                crosses

            loop rest (dist + manDist line.Pt1 line.Pt2)

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
    let wires = Parser.parseInput fileName

    Utils.findAllCrosses wires.[0] wires.[1]
    |> startWalkTasks wires.[0] wires.[1]
    |> getTaskResults
    |> findLowest
