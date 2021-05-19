// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open utils.run

let problems =
    [ { label = "Day 1, Part 1"
        fn = (fun () -> aoc.year2019.day1.part1.run "input/2019/day1/puzzle.txt")
        exp = 3279287 }
      { label = "Day 1, Part 2"
        fn = (fun () -> aoc.year2019.day1.part2.run "input/2019/day1/puzzle.txt")
        exp = 4916076 }
      { label = "Day 2, Part 1"
        fn = (fun () -> aoc.year2019.day2.part1.run "input/2019/day2/puzzle.txt")
        exp = 3101844 }
      { label = "Day 2, Part 2"
        fn = (fun () -> aoc.year2019.day2.part2.run "input/2019/day2/puzzle.txt")
        exp = 8478 }
      { label = "Day 3, Part 1"
        fn = (fun () -> aoc.year2019.day3.part1.run "input/2019/day3/puzzle.txt")
        exp = 529 }
      { label = "Day 3, Part 2"
        fn = (fun () -> aoc.year2019.day3.part2.run "input/2019/day3/puzzle.txt")
        exp = 20386 } ]

let runAll probs =
    let total =
        List.fold (fun acc p -> acc + run p) 0 probs

    printfn $"total {total} ms"

[<EntryPoint>]
let main argv =
    // let problems =
    //     [ { label = "Day 3, Part 1"
    //         fn = (fun () -> aoc.year2019.day3.part2.run "input/2019/day3/puzzle.txt")
    //         exp = 20386 } ]

    runAll problems

    0 // return an integer exit code
