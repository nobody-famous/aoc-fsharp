module Aoc.Year2019.Day6.Utils

type Node(label) =
    let mutable count = 0

    member _.Label = label
    member _.Count = count
    member _.IncCount() = count <- count + 1
