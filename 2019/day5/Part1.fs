module aoc.year2019.day5.part1

open aoc.year2019.intcode

type InputState = { next: int }

type OutputState = { last: int }

type machineIO =
    { input: InputState -> (InputState * int)
      output: (OutputState * int) -> OutputState }

let newMachineIO inpFn outFn = { input = inpFn; output = outFn }

let runMachine m =
    let rec loop m = if m.halt then m else loop <| step m

    loop m

let inputFn state = (state, state.next)

let outputFn (state, value) = { state with last = value }

let run fileName =
    let io = newMachineIO inputFn outputFn

    let _ =
        parseInput fileName |> newMachine |> runMachine

    0
