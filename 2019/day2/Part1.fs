module aoc.year2019.day2.part1

open aoc.year2019.intcode

let run fileName =
    parseInput fileName
    |> newMachine
    |> setAddr 1 12
    |> setAddr 2 2
    |> utils.runMachine
    |> getAddr 0
