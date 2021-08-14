// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module Runner2019

open Aoc.Utils.Run

let problems2019 =
    [ { Data = (2019, 1, 1, Aoc.Year2019.Day1.Part1.run 3279287, "puzzle.txt") }
      { Data = (2019, 1, 2, Aoc.Year2019.Day1.Part2.run 4916076, "puzzle.txt") } ]

let runAll probs =
    let total =
        List.fold (fun acc p -> acc + run p) 0 probs

    printfn $"total {total} ms"

[<EntryPoint>]
let main argv =
    let problems2019 =
        [ { Data = (2019, 2, 1, Aoc.Year2019.Day2.Part1.run 3101844, "puzzle.txt") } ]

    runAll problems2019

    0 // return an integer exit code
