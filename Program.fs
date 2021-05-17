// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let problems =
    [ (fun () -> aoc.year2019.day1.run "input/2019/day1/puzzle.txt" 3279287) ]

let runAll probs =
    let total =
        List.fold
            (fun acc p ->
                let start = DateTime.Now.Millisecond

                p ()

                let diff = DateTime.Now.Millisecond - start
                diff + acc)
            0
            probs

    printfn $"total {total} ms"

[<EntryPoint>]
let main argv =
    runAll problems

    0 // return an integer exit code
