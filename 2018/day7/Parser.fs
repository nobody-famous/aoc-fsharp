module Aoc.Year2018.Day7.Parser

open System.Collections.Generic
open Aoc.Year2018.Day7.Utils
open Aoc.Utils.Regex

let regex =
    @"Step ([A-Z]) must be finished before step ([A-Z]) can begin."

let parseLine line =
    match line with
    | MatchPattern regex [ parent; child ] ->
        { Parent = parent.[0]
          Child = child.[0] }
    | _ -> failwith $"Invalid line {line}"

let buildGraph entries =
    let graph =
        { Tree = Dictionary<char, char list>()
          Parents = Dictionary<char, int>() }

    let addToGraph node =
        if not (graph.Parents.ContainsKey node.Parent) then
            graph.Parents.[node.Parent] <- 0

        if not (graph.Parents.ContainsKey node.Child) then
            graph.Parents.[node.Child] <- 1
        else
            graph.Parents.[node.Child] <- graph.Parents.[node.Child] + 1

        if not (graph.Tree.ContainsKey node.Parent) then
            graph.Tree.[node.Parent] <- []

        graph.Tree.[node.Parent] <- node.Child :: graph.Tree.[node.Parent]

    Array.iter addToGraph entries

    graph

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map parseLine
    |> buildGraph
