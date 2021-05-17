module aoc.year2019.day1

let fuel mass = (mass / 3) - 2

let parseInput fileName =
    let lines = utils.parser.readLines fileName
    Array.map (fun line -> int line) lines

let run fileName exp =
    let answer =
        parseInput fileName
        |> Array.map (fun n -> async { return fuel n })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.fold (+) 0

    printfn $"answer {answer}"
