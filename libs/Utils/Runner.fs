module Aoc.Utils.Runner

type Problem =
    | IntProblem of label: string * fn: (string list -> int) * exp: int
    | LongProblem of label: string * fn: (string list -> int64) * exp: int64
    | StringProblem of label: string * fn: (string list -> string) * exp: string

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
    printfn $"{label} [{status}] {ms} ms"
    ms

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
