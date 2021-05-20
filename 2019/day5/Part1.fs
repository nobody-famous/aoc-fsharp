module aoc.year2019.day5.part1

open aoc.year2019.intcode

type InputState = { next: int }

type OutputState = { last: int option }

type machineState =
    { input: InputState
      output: OutputState }

let newState () =
    { input = { next = 1 }
      output = { last = None } }

let runMachine m =
    let rec loop m = if m.halt then m else loop <| step m

    loop m

let inputFn m =
    let s = getState m
    (s.input.next, m)

let outputFn (state, value) = { state with last = value }

let run fileName =
    let _ =
        parseInput fileName
        |> newMachineState (newState ())
        |> setInputFn inputFn
        |> setDebug true
        |> runMachine

    0
