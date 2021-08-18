// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module Runner2019

open Aoc.Utils.Run
open Aoc.Year2019

let problems2019 =
    [ { Data = (2019, 1, 1, Day1.Part1.run 3279287, "puzzle.txt") }
      { Data = (2019, 1, 2, Day1.Part2.run 4916076, "puzzle.txt") }
      { Data = (2019, 2, 1, Day2.Part1.run 3101844, "puzzle.txt") }
      { Data = (2019, 2, 2, Day2.Part2.run 8478, "puzzle.txt") }
      { Data = (2019, 3, 1, Day3.Part1.run 529, "puzzle.txt") }
      { Data = (2019, 3, 2, Day3.Part2.run 20386, "puzzle.txt") }
      { Data = (2019, 4, 1, Day4.Part1.run 511, "puzzle.txt") }
      { Data = (2019, 4, 2, Day4.Part2.run 316, "puzzle.txt") } ]

let runAll probs =
    let total =
        List.fold (fun acc p -> acc + run p) 0 probs

    printfn $"total {total} ms"

[<EntryPoint>]
let main _ =
    // let problems2019 =
    //     [ { Data = (2019, 4, 2, Day4.Part2.run 316, "puzzle.txt") } ]

    runAll problems2019

    0 // return an integer exit code
