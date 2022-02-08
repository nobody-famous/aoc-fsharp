module Aoc.Year2018.Day19.Utils

type Device =
    { Registers: int array
      mutable IpReg: int
      mutable Halt: bool }

type Instr =
    { Op: Device -> (int * int * int) -> unit
      A: int
      B: int
      C: int }

type State = { Mach: Device; Prog: Instr array }

let newState prog =
    { Prog = prog
      Mach =
        { Registers = [| for _ in 0 .. 5 -> 0 |]
          IpReg = 0
          Halt = false } }

let printState (state: State) =
    printf "{"
    printf $"IP:{state.Mach.IpReg}"
    printf $" {if state.Mach.Halt then 'T' else 'F'}"
    printf " ["

    for ndx in 0 .. state.Mach.Registers.Length - 1 do
        printf $" {state.Mach.Registers.[ndx]}"

    printf " ]"
    printfn "}"

let rr (state: Device) (a, b, c) op =
    state.Registers.[c] <- op state.Registers.[a] state.Registers.[b]

let ri (state: Device) (a, b, c) op =
    state.Registers.[c] <- op state.Registers.[a] b

let ir (state: Device) (a, b, c) op =
    state.Registers.[c] <- op a state.Registers.[b]

let addr state instr = rr state instr (+)
let addi state instr = ri state instr (+)

let mulr state instr = rr state instr (*)

let muli state instr = ri state instr (*)

let banr state instr = rr state instr (&&&)
let bani state instr = ri state instr (&&&)

let borr state instr = rr state instr (|||)
let bori state instr = ri state instr (|||)

let setr (state: Device) (a, _, c) =
    state.Registers.[c] <- state.Registers.[a]

let seti (state: Device) (a, _, c) = state.Registers.[c] <- a

let gtir (state: Device) (a, b, c) =
    state.Registers.[c] <- if a > state.Registers.[b] then 1 else 0

let gtri (state: Device) (a, b, c) =
    state.Registers.[c] <- if state.Registers.[a] > b then 1 else 0

let gtrr (state: Device) (a, b, c) =
    state.Registers.[c] <-
        if state.Registers.[a] > state.Registers.[b] then
            1
        else
            0

let eqir (state: Device) (a, b, c) =
    state.Registers.[c] <- if a = state.Registers.[b] then 1 else 0

let eqri (state: Device) (a, b, c) =
    state.Registers.[c] <- if state.Registers.[a] = b then 1 else 0

let eqrr (state: Device) (a, b, c) =
    state.Registers.[c] <-
        if state.Registers.[a] = state.Registers.[b] then
            1
        else
            0
