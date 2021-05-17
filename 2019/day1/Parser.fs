module aoc.year2019.day1.parser

let parseInput fileName =
    utils.parser.readLines fileName
    |> Array.map (fun line -> int line)
