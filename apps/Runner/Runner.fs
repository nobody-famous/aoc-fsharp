open Aoc.Utils.Runner

module S = Aoc.Utils.String

let problems: Problem list =
    [
      //   IntProblem("2018/day1/part1", Aoc.Year2018.Day1.Part1.run, 437)
      //   IntProblem("2018/day1/part2", Aoc.Year2018.Day1.Part2.run, 655)
      StringProblem("2018/day13/part1", Aoc.Year2018.Day13.Part1.run, "80,100")
      StringProblem("2018/day13/part2", Aoc.Year2018.Day13.Part2.run, "16,99") ]

let getInputFile path p =
    let label =
        match p with
        | IntProblem (label, _, _) -> label
        | LongProblem (label, _, _) -> label
        | StringProblem (label, _, _) -> label

    let ndx = label.LastIndexOf('/')
    $"{path}/input/{label.[0..ndx]}puzzle.txt"

let runProblem path p =
    p
    |> getInputFile path
    |> System.IO.File.ReadAllText
    |> fun s -> s.Split '\n'
    |> S.trimIndent
    |> Array.toList
    |> run p


let args = System.Environment.GetCommandLineArgs()

let path =
    if Array.length args > 1 then
        args.[1]
    else
        "UNKNOWN_PATH"

let total =
    List.fold (fun total p -> total + runProblem path p) 0L problems

printfn $"Total {total} ms"
