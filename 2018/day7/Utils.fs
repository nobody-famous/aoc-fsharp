module Aoc.Year2018.Day7.Utils

open System.Collections.Generic

[<Struct>]
type Node = { Parent: char; Child: char }

[<Struct>]
type Graph =
    { Tree: Dictionary<char, char list>
      Parents: Dictionary<char, int>
      ParentNodes: Dictionary<char, char list> }
