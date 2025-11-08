module Aoc.Year2018.Day16.Part1

module U = Aoc.Year2018.Day16.Utils

let countMatches (samples: U.Sample list) =
    samples
    |> List.sumBy (fun sample ->
        let count =
            U.fns
            |> List.sumBy (fun fn ->
                if fn sample.Before sample.Instr = sample.After then
                    1
                else
                    0)

        if count >= 3 then 1 else 0)

let run (input: string list) =
    let config = U.parse input

    countMatches config.Samples
