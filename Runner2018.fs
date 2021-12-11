// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module Runner

open Aoc.Utils.Run

let problems2018 =
    [ { Data = (2018, 2, 1, Aoc.Year2018.Day2.Part1.run 6225, "puzzle.txt") }
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
      { Data = (2018, 9, 2, Aoc.Year2018.Day9.Part2.run 3133277384L, "puzzle.txt") }
      { Data = (2018, 10, 1, Aoc.Year2018.Day10.Part1.run "EJZEAAPE", "puzzle.txt") }
      { Data = (2018, 10, 2, Aoc.Year2018.Day10.Part2.run 10054, "puzzle.txt") } ]

let runAll probs =
    let total =
        List.fold (fun acc p -> acc + run p) 0 probs

    printfn $"total {total} ms"

[<EntryPoint>]
let main argv =
    // let problems2018 =
    //     [ { Data = (2018, 10, 2, Aoc.Year2018.Day10.Part2.run 10054, "puzzle.txt") } ]

    runAll problems2018

    0 // return an integer exit code
