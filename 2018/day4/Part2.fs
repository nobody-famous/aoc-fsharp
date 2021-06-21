module Aoc.Year2018.Day4.Part2

open System.Collections.Generic

let getHighMinutes (entries: Dictionary<int, int array>) =
    let highMinutes = Dictionary<int, int * int>()

    let highIndex minutes =
        let mutable highest = (0, 0)

        for ndx in 0 .. Array.length minutes - 1 do
            if minutes.[ndx] > snd highest then
                highest <- (ndx, minutes.[ndx])

        highest

    for (KeyValue (id, minutes)) in entries do
        highMinutes.[id] <- highIndex minutes

    highMinutes

let answer id (minute, _) = id * minute

let run exp fileName =
    Parser.parseInput fileName
    |> getHighMinutes
    |> Seq.toList
    |> Seq.fold
        (fun (highId, (highNdx, highValue)) (KeyValue (id, (ndx, minute))) ->
            if minute > highValue then
                (id, (ndx, minute))
            else
                (highId, (highNdx, highValue)))

        (0, (0, 0))
    ||> answer
    |> Aoc.Utils.Run.checkResult exp
