module Aoc.Year2019.Day6.Part1

open System.Collections.Generic

let countOrbits (orbits: Dictionary<string, string list>) =
    let rec countKids curCount object =
        if not (orbits.ContainsKey object) then
            0
        else
            let kids = orbits.[object]
            let nextCount = curCount + 1
            List.fold (fun acc kid -> acc + countKids nextCount kid) (nextCount * kids.Length) kids

    countKids 0 "COM"

let run exp fileName =
    Parser.parseInput fileName
    |> countOrbits
    |> Aoc.Utils.Run.checkResult exp
