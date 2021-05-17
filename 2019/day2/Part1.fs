module aoc.year2019.day2.part1

open aoc.year2019.intcode

let runMachine mach =
    let rec loop m = if m.halt then m else loop <| step m

    loop mach

let run fileName =
    parseInput fileName
    |> newMachine
    |> setAddr 1 12
    |> setAddr 2 2
    |> runMachine
    |> getAddr 0
