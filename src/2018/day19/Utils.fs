module Aoc.Year2018.Day19.Utils

module S = Aoc.Utils.String

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

let nameToOp (name: string) =
    match name with
    | "#ip" -> addr
    | "addr" -> addr
    | "addi" -> addi
    | "mulr" -> mulr
    | "muli" -> muli
    | "banr" -> banr
    | "bani" -> bani
    | "borr" -> borr
    | "bori" -> bori
    | "setr" -> setr
    | "seti" -> seti
    | "gtir" -> gtir
    | "gtri" -> gtri
    | "gtrr" -> gtrr
    | "eqir" -> eqir
    | "eqri" -> eqri
    | "eqrr" -> eqrr
    | _ -> failwith $"Unknown Op {name}"

let parseLine (line: string) =
    let pieces =
        line.Split ' ' |> Array.map (fun s -> s.Trim())

    { Op = nameToOp pieces.[0]
      A = int pieces.[1]
      B =
        if Array.length pieces > 2 then
            int pieces.[2]
        else
            0
      C =
        if Array.length pieces > 3 then
            int pieces.[3]
        else
            0 }

let parse (input: string) =
    let instrs =
        input.Split '\n'
        |> S.trimIndent
        |> Array.map parseLine

    let state = newState (instrs.[1..])

    state.Mach.IpReg <- instrs.[0].A
    state

let exec (state: State) =
    let ip = state.Mach.Registers.[state.Mach.IpReg]

    if ip >= state.Prog.Length then
        state.Mach.Halt <- true
    else
        let instr = state.Prog.[ip]

        instr.Op state.Mach (instr.A, instr.B, instr.C)

        state.Mach.Registers.[state.Mach.IpReg] <- state.Mach.Registers.[state.Mach.IpReg] + 1

let mach r0 =
    let mutable a = r0
    let mutable b = 0
    let mutable c = 0
    let mutable d = 0
    let mutable e = 0

    let rec loop initB initE =
        b <- initB
        e <- initE

        c <- b * e
        if c = d then a <- a + b

        e <- e + 1

        if e > d then
            b <- b + 1
            if b <= d then loop b 1 else a
        else
            loop b e

    d <- (2 * 2 * 209) + (5 * 22 + 1)

    if a = 0 then
        loop 1 1
    else
        c <- 10550400
        d <- d + c
        a <- 0
        loop 1 1
