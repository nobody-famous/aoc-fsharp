type Problem =
    val label: string
    new(str) = { label = str }


type IntProblem =
    inherit Problem

    val fn: string -> int
    val exp: int

    new(l, f, e) = { inherit Problem(l); fn = f; exp = e }

type LongProblem =
    inherit Problem

    val fn: string -> int64
    val exp: int64

    new(l, f, e) = { inherit Problem(l); fn = f; exp = e }

type StringProblem =
    inherit Problem

    val fn: string -> string
    val exp: string

    new(l, f, e) = { inherit Problem(l); fn = f; exp = e }

let fileName (label: string) =
    let ndx = label.LastIndexOf('/')
    $"input/{label.[0..ndx]}puzzle.txt"

let runIntProblem (prob: IntProblem) (input: string) =
    let answer = prob.fn input

    if answer = prob.exp then
        "OK"
    else
        $"Failed: expected {prob.exp}, found {answer}"

let runLongProblem (prob: LongProblem) (input: string) =
    let answer = prob.fn input

    if answer = prob.exp then
        "OK"
    else
        $"Failed: expected {prob.exp}, found {answer}"

let runStringProblem (prob: StringProblem) (input: string) =
    let answer = prob.fn input

    if answer = prob.exp then
        "OK"
    else
        $"Failed: expected {prob.exp}, found {answer}"

let runProblem (prob: Problem) =
    let file = fileName prob.label
    let input = System.IO.File.ReadAllText file
    let watch = System.Diagnostics.Stopwatch()

    watch.Start()

    let result =
        match prob with
        | :? IntProblem as p -> runIntProblem p input
        | :? LongProblem as p -> runLongProblem p input
        | :? StringProblem as p -> runStringProblem p input
        | p -> failwith $"Unknown problem type {p}"

    watch.Stop()

    printfn $"{prob.label} [{result}] {watch.ElapsedMilliseconds} ms"

    watch.ElapsedMilliseconds |> int

let year2018: Problem list =
    [ IntProblem("2018/day1/part1", Aoc.Year2018.Day1.Part1.run, 437)
      IntProblem("2018/day1/part2", Aoc.Year2018.Day1.Part2.run, 655)
      LongProblem("2018/day9/part1", Aoc.Year2018.Day9.Part1.run, 382055)
      LongProblem("2018/day9/part2", Aoc.Year2018.Day9.Part2.run, 3133277384L)
      StringProblem("2018/day13/part1", Aoc.Year2018.Day13.Part1.run, "80,100")
      StringProblem("2018/day13/part2", Aoc.Year2018.Day13.Part2.run, "16,99")
      StringProblem("2018/day14/part1", Aoc.Year2018.Day14.Part1.run, "9276422810")
      IntProblem("2018/day14/part2", Aoc.Year2018.Day14.Part2.run, 20319117)
      IntProblem("2018/day15/part1",Aoc.Year2018.Day15.Part1.run, 269430)
      IntProblem("2018/day15/part1",Aoc.Year2018.Day15.Part2.run, 55160)
      IntProblem("2018/day16/part1",Aoc.Year2018.Day16.Part1.run, 605) ]

[<EntryPoint>]
let main _ =
    // let allProblems: Problem list = year2018
    let allProblems: Problem list = [IntProblem("2018/day16/part1",Aoc.Year2018.Day16.Part1.run, 605)]

    let total =
        List.fold (fun total p -> total + runProblem p) 0 allProblems

    printfn $"Total {total} ms"
    0
