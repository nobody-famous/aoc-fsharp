open Aoc.Utils.Runner

module S = Aoc.Utils.String

let problems: Problem list =
    [
      //   IntProblem("2018/day1/part1", Aoc.Year2018.Day1.Part1.run, 437)
      //   IntProblem("2018/day1/part2", Aoc.Year2018.Day1.Part2.run, 655)
      StringProblem("2018/day13/part1", Aoc.Year2018.Day13.Part1.run, "80,100")
      StringProblem("2018/day13/part2", Aoc.Year2018.Day13.Part2.run, "16,99") ]

let args = System.Environment.GetCommandLineArgs()

let path =
    if Array.length args > 1 then
        args.[1]
    else
        "UNKNOWN_PATH"

let private mutex = obj ()

let printString s = lock mutex (fun () -> printfn $"{s}")


let total =
    problems
    |> List.map (fun p ->
        System.Threading.Tasks.Task.Run (fun () ->
            let msg, ms = runProblem path p
            printString msg
            ms))
    |> System.Threading.Tasks.Task.WhenAll
    |> (fun task -> task.Result)
    |> Array.sum

printfn $"Total {total} ms"
