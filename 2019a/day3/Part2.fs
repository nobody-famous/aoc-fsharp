module Aoc.Year2019.Day3.Part2

open Aoc.Utils.Geometry
open System.Collections.Generic

type State(wires) =
    member val Dists1 = Dictionary<Point, int>()
    member val Dists2 = Dictionary<Point, int>()

    member _.Wires = wires
    member _.Crosses = Utils.findCrosses wires

let onLine point line =
    (Utils.isHorizontal line
     && point.Y = line.Pt1.Y
     && Utils.isBetween point.X line.Pt1.X line.Pt2.X)
    || (Utils.isVertical line
        && point.X = line.Pt1.X
        && Utils.isBetween point.Y line.Pt1.Y line.Pt2.Y)

let calcDist wire crosses (dict: Dictionary<Point, int>) =
    let mutable total = 0

    for line in wire do
        for cross in crosses do
            if onLine cross line then
                dict.Add(cross, total + manDist cross line.Pt1)

        total <- total + manDist line.Pt1 line.Pt2

let calcDists (state: State) =
    calcDist state.Wires.[0] state.Crosses state.Dists1
    calcDist state.Wires.[1] state.Crosses state.Dists2

    state

let sumDists (state: State) =
    let sums = Dictionary<Point, int>()
    let dists1 = state.Dists1
    let dists2 = state.Dists2

    for cross in state.Crosses do
        sums.Add(cross, dists1.[cross] + dists2.[cross])

    sums

let minimum (sums: Dictionary<Point, int>) =
    Seq.fold (fun acc (KeyValue (k, v)) -> min acc v) System.Int32.MaxValue sums

let run exp fileName =
    Parser.parseInput fileName
    |> State
    |> calcDists
    |> sumDists
    |> minimum
    |> Aoc.Utils.Run.checkResult exp
