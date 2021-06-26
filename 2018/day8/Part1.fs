module Aoc.Year2018.Day8.Part1

open Aoc.Year2018.Day8.Utils

let rec sumAllMetas node =
    List.fold (fun total n -> total + sumAllMetas n) (List.sum node.Metadata) node.Kids

let run exp fileName =
    Parser.parseInput fileName
    |> Array.toList
    |> parseTree
    |> sumAllMetas
    |> Aoc.Utils.Run.checkResult exp
