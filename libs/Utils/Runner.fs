module Aoc.Utils.Runner

module S = Aoc.Utils.String

type Problem =
    | IntProblem of label: string * fn: (string list -> int) * exp: int
    | LongProblem of label: string * fn: (string list -> int64) * exp: int64
    | StringProblem of label: string * fn: (string list -> string) * exp: string

let getInputFile path p =
    let label =
        match p with
        | IntProblem (label, _, _) -> label
        | LongProblem (label, _, _) -> label
        | StringProblem (label, _, _) -> label

    let ndx = label.LastIndexOf('/')
    $"{path}/input/{label.[0..ndx]}puzzle.txt"

let checkAnswer exp act ms =
    let status =
        if act = exp then
            "OK"
        else
            $"Failed: expected {exp}, found {act}"

    status, ms

let timeFn fn input =
    let watch = System.Diagnostics.Stopwatch()
    watch.Start()

    let result = fn input
    watch.Stop()

    result, watch.ElapsedMilliseconds

let reportResult label status ms =
    $"{label} [{status}] {ms} ms", ms

let run problem input =
    match problem with
    | IntProblem (label, fn, exp) ->
        input
        |> timeFn fn
        ||> checkAnswer exp
        ||> reportResult label
    | LongProblem (label, fn, exp) ->
        input
        |> timeFn fn
        ||> checkAnswer exp
        ||> reportResult label
    | StringProblem (label, fn, exp) ->
        input
        |> timeFn fn
        ||> checkAnswer exp
        ||> reportResult label

let runProblem path p =
    p
    |> getInputFile path
    |> System.IO.File.ReadAllText
    |> fun s -> s.Split '\n'
    |> S.trimIndent
    |> Array.toList
    |> run p
