// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module Runner

open Aoc.Utils.Run

let problems2018 =
    [ { Data = (2018, 1, 1, Aoc.Year2018.Day1.Part1.run 437, "puzzle.txt") }
      { Data = (2018, 1, 2, Aoc.Year2018.Day1.Part2.run 655, "puzzle.txt") }
      { Data = (2018, 2, 1, Aoc.Year2018.Day2.Part1.run 6225, "puzzle.txt") }
      { Data = (2018, 2, 1, Aoc.Year2018.Day2.Part2.run "revtaubfniyhsgxdoajwkqilp", "puzzle.txt") }
      { Data = (2018, 3, 1, Aoc.Year2018.Day3.Part1.run 118858, "puzzle.txt") }
      { Data = (2018, 3, 2, Aoc.Year2018.Day3.Part2.run 1100, "puzzle.txt") } ]

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
    // let problems2018 =
    //     [ { Data = (2018, 3, 1, Aoc.Year2018.Day3.Part2.run 1100, "puzzle.txt") } ]

    runAll problems2018

    0 // return an integer exit code
