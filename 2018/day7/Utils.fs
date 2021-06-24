module Aoc.Year2018.Day7.Utils

open System.Collections.Generic

type Node = { Parent: char; Child: char }

type Graph =
    { Tree: Dictionary<char, char list>
      Parents: Dictionary<char, int>
      ParentNodes: Dictionary<char, char list> }

let getNextNode graph =
    Seq.toList graph.Parents
    |> List.fold
        (fun next (KeyValue (cur, count)) ->
            match count with
            | 0 -> System.Math.Min(int cur, int next) |> char
            | _ -> next)
        'Z'
