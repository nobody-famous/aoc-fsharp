module Aoc.Year2019.Day5.Part1

open Aoc.Year2019.Intcode

type MachineState = { Next: int; Last: int option }

let newState () = { Next = 1; Last = None }

let runMachine m =
    let rec loop m = if m.Halt then m else loop <| step m

    loop m

let inputFn m =
    let s = getState m
    (s.Next, m)

let outputFn value m =
    setState { getState m with Last = Some value } m

let getLastOutput state = state.Last

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
