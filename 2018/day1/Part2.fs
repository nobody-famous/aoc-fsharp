module Aoc.Year2018.Day1.Part2

open System.Collections.Generic

let findDupes (input: int array) =
    let seen = new Dictionary<int, bool>()

    let rec loop acc ndx =
        if seen.ContainsKey acc then
            acc
        else
            seen.Add(acc, true)
            loop (acc + input.[ndx]) ((ndx + 1) % Array.length input)

    loop 0 0

let run fileName = Parser.parseInput fileName |> findDupes
