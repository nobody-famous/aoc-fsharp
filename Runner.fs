// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module runner

open utils.run

let problems2019 =
    [ { data = (2019, 1, 1, 3279287, aoc.year2019.day1.part1.run, "puzzle.txt") }
      { data = (2019, 1, 2, 4916076, aoc.year2019.day1.part2.run, "puzzle.txt") }
      { data = (2019, 2, 1, 3101844, aoc.year2019.day2.part1.run, "puzzle.txt") }
      { data = (2019, 2, 2, 8478, aoc.year2019.day2.part2.run, "puzzle.txt") }
      { data = (2019, 3, 1, 529, aoc.year2019.day3.part1.run, "puzzle.txt") }
      { data = (2019, 3, 2, 20386, aoc.year2019.day3.part2.run, "puzzle.txt") }
      { data = (2019, 4, 1, 511, aoc.year2019.day4.part1.run, "puzzle.txt") }
      { data = (2019, 4, 2, 316, aoc.year2019.day4.part2.run, "puzzle.txt") }
      { data = (2019, 5, 1, 13294380, aoc.year2019.day5.part1.run, "puzzle.txt") } ]

let runAll probs =
    let total =
        List.fold (fun acc p -> acc + run p) 0 probs

    printfn $"total {total} ms"

[<EntryPoint>]
let main argv =
    // let problems2019 =
    //     [ { data = (2019, 5, 1, 13294380, aoc.year2019.day5.part1.run, "puzzle.txt") } ]

    runAll problems2019

    0 // return an integer exit code
