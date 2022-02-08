module Aoc.Year2018.Day19.Part1

module U = Aoc.Year2018.Day19.Utils
module S = Aoc.Utils.String

let nameToOp (name: string) =
    match name with
    | "#ip" -> U.addr
    | "addr" -> U.addr
    | "addi" -> U.addi
    | "mulr" -> U.mulr
    | "muli" -> U.muli
    | "banr" -> U.banr
    | "bani" -> U.bani
    | "borr" -> U.borr
    | "bori" -> U.bori
    | "setr" -> U.setr
    | "seti" -> U.seti
    | "gtir" -> U.gtir
    | "gtri" -> U.gtri
    | "gtrr" -> U.gtrr
    | "eqir" -> U.eqir
    | "eqri" -> U.eqri
    | "eqrr" -> U.eqrr
    | _ -> failwith $"Unknown Op {name}"

let parseLine (line: string) =
    let pieces =
        line.Split ' ' |> Array.map (fun s -> s.Trim())

    { U.Op = nameToOp pieces.[0]
      U.A = int pieces.[1]
      U.B =
        if Array.length pieces > 2 then
            int pieces.[2]
        else
            0
      U.C =
        if Array.length pieces > 3 then
            int pieces.[3]
        else
            0 }

let parse (input: string) =
    let instrs =
        input.Split '\n'
        |> S.trimIndent
        |> Array.map parseLine

    let state = U.newState (instrs.[1..])

    state.Mach.IpReg <- instrs.[0].A
    state

let exec (state: U.State) =
    let ip = state.Mach.Registers.[state.Mach.IpReg]

    if ip >= state.Prog.Length then
        state.Mach.Halt <- true
    else
        let instr = state.Prog.[ip]

        instr.Op state.Mach (instr.A, instr.B, instr.C)

        state.Mach.Registers.[state.Mach.IpReg] <- state.Mach.Registers.[state.Mach.IpReg] + 1

let run (input: string) =
    let state = parse input

    while not state.Mach.Halt do
        exec state

    state.Mach.Registers.[0]
