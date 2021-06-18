module aoc.year2019.day5.part1

open aoc.year2019.intcode

type machineState = { next: int; last: int option }

let newState () = { next = 1; last = None }

let runMachine m =
    let rec loop m = if m.halt then m else loop <| step m

    loop m

let inputFn m =
    let s = getState m
    (s.next, m)

let outputFn value m =
    setState { getState m with last = Some value } m

let getLastOutput state = state.last

let run fileName =
    let out =
        parseInput fileName
        |> newMachineState (newState ())
        |> setInputFn inputFn
        |> setOutputFn outputFn
        |> runMachine
        |> getState
        |> getLastOutput

    match out with
    | None -> 0
    | Some v -> v
