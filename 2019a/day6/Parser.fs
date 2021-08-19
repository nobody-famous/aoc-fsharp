module Aoc.Year2019.Day6.Parser

open System.Collections.Generic

type Orbit = { Parent: string; Child: string }

let parseLine (line: string) =
    let parts = line.Split ')'

    { Parent = parts.[0]
      Child = parts.[1] }

let buildGraph orbits =
    let graph = Dictionary<string, Utils.Node list>()

    for { Parent = parent; Child = child } in orbits do
        if not (graph.ContainsKey parent) then
            graph.[parent] <- []

        graph.[parent] <- new Utils.Node(child) :: graph.[parent]

    graph

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map parseLine
    |> buildGraph
