module y2018.day1.part1

module utils = y2018.day1.utils

let run (input: string) = utils.parse input |> List.sum
