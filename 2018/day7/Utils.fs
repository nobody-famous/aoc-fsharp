module Aoc.Year2018.Day7.Utils

open System.Collections.Generic

type Node = { Parent: char; Child: char }

type Graph =
    { Tree: Dictionary<char, char list>
      Parents: Dictionary<char, int>
      ParentNodes: Dictionary<char, char list> }
