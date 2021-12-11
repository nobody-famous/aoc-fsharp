type Problem =
    val label: string
    new(str) = { label = str }


type IntProblem =
    inherit Problem

    val fn: string -> int
    val exp: int

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
        | :? StringProblem as p -> runStringProblem p input
        | p -> failwith $"Unknown problem type {p}"

    watch.Stop()

    printfn $"{prob.label} [{result}] {watch.ElapsedMilliseconds} ms"

    watch.ElapsedMilliseconds |> int

let year2018: Problem list =
    [ IntProblem("2018/day1/part1", y2018.day1.part1.run, 437)
      IntProblem("2018/day1/part2", y2018.day1.part2.run, 655) ]

[<EntryPoint>]
let main _ =
    let allProblems: Problem list = year2018

    let total =
        List.fold (fun total p -> total + runProblem p) 0 allProblems

    printfn $"Total {total} ms"
    0
