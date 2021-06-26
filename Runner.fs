// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module Runner

open Aoc.Utils.Run

let problems2018 =
    [ { Data = (2018, 1, 1, Aoc.Year2018.Day1.Part1.run 437, "puzzle.txt") }
      { Data = (2018, 1, 2, Aoc.Year2018.Day1.Part2.run 655, "puzzle.txt") }
      { Data = (2018, 2, 1, Aoc.Year2018.Day2.Part1.run 6225, "puzzle.txt") }
      { Data = (2018, 2, 2, Aoc.Year2018.Day2.Part2.run "revtaubfniyhsgxdoajwkqilp", "puzzle.txt") }
      { Data = (2018, 3, 1, Aoc.Year2018.Day3.Part1.run 118858, "puzzle.txt") }
      { Data = (2018, 3, 2, Aoc.Year2018.Day3.Part2.run 1100, "puzzle.txt") }
      { Data = (2018, 4, 1, Aoc.Year2018.Day4.Part1.run 39698, "puzzle.txt") }
      { Data = (2018, 4, 2, Aoc.Year2018.Day4.Part2.run 14920, "puzzle.txt") }
      { Data = (2018, 5, 1, Aoc.Year2018.Day5.Part1.run 11590, "puzzle.txt") }
      { Data = (2018, 5, 2, Aoc.Year2018.Day5.Part2.run 4504, "puzzle.txt") }
      { Data = (2018, 6, 1, Aoc.Year2018.Day6.Part1.run 3569, "puzzle.txt") }
      { Data = (2018, 6, 2, Aoc.Year2018.Day6.Part2.run 48978, "puzzle.txt") }
      { Data = (2018, 7, 1, Aoc.Year2018.Day7.Part1.run "GJKLDFNPTMQXIYHUVREOZSAWCB", "puzzle.txt") }
      { Data = (2018, 7, 2, Aoc.Year2018.Day7.Part2.run 967, "puzzle.txt") }
      { Data = (2018, 8, 1, Aoc.Year2018.Day8.Part1.run 40701, "puzzle.txt") }
      { Data = (2018, 8, 2, Aoc.Year2018.Day8.Part2.run 21399, "puzzle.txt") }
      { Data = (2018, 9, 1, Aoc.Year2018.Day9.Part1.run 382055L, "puzzle.txt") }
      { Data = (2018, 9, 2, Aoc.Year2018.Day9.Part2.run 3133277384L, "puzzle.txt") } ]

// let problems2019 =
//     [ { data = (2019, 1, 1, 3279287, aoc.year2019.day1.part1.run, "puzzle.txt") }
//       { data = (2019, 1, 2, 4916076, aoc.year2019.day1.part2.run, "puzzle.txt") }
//       { data = (2019, 2, 1, 3101844, aoc.year2019.day2.part1.run, "puzzle.txt") }
//       { data = (2019, 2, 2, 8478, aoc.year2019.day2.part2.run, "puzzle.txt") }
//       { data = (2019, 3, 1, 529, aoc.year2019.day3.part1.run, "puzzle.txt") }
//       { data = (2019, 3, 2, 20386, aoc.year2019.day3.part2.run, "puzzle.txt") }
//       { data = (2019, 4, 1, 511, aoc.year2019.day4.part1.run, "puzzle.txt") }
//       { data = (2019, 4, 2, 316, aoc.year2019.day4.part2.run, "puzzle.txt") }
//       { data = (2019, 5, 1, 13294380, aoc.year2019.day5.part1.run, "puzzle.txt") } ]

let runAll probs =
    let total =
        List.fold (fun acc p -> acc + run p) 0 probs

    printfn $"total {total} ms"

[<EntryPoint>]
let main argv =
    let problems2018 =
        [ { Data = (2018, 10, 1, Aoc.Year2018.Day10.Part1.run 3133277384L, "puzzle.txt") } ]

    runAll problems2018

    0 // return an integer exit code
