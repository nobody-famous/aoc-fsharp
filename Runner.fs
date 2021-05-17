// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open utils.run

let problems =
    [ { label = "Day 1, Part 1"
        fn = (fun () -> aoc.year2019.day1.part1.run "input/2019/day1/puzzle.txt")
        exp = 3279287 } ]

let runProb prob =
    let answer = prob.fn ()

    if answer <> prob.exp then
        printfn $"{prob.label} FAILED: {answer} != {prob.exp}"

let runAll probs =
    let total =
        List.fold (fun acc p -> acc + run p) 0 probs

    printfn $"total {total} ms"

[<EntryPoint>]
let main argv =
    runAll problems

    0 // return an integer exit code
