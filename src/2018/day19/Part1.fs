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
            if b <= d
                then loop b 1
                else
                    printfn $"LOOP {a} {b} {c} {d} {e}"
                    a
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

let run (input: string) =
    let state = parse input

    mach 1

// while not state.Mach.Halt do
//     exec state

// state.Mach.Registers.[0]

// INIT
// GOTO MAIN

// INNER_LOOP
// b = 1

// INNER_LOOP_INIT_E
// e = 1

// INNER_LOOP_NO_INIT
// c = b * e
// if c == d
//   a += b
// e += 1
// if e > d
//   b += 1
//   if b > d
//     exit
//   else GOTO_INNER_LOOP_INIT_E
// else GOTO INNER_LOOP_NO_INIT

// MAIN
// d = (d + 2) * (d + 2) * 209
// c = (c + 5) * 22 + 1
// d = d + c
// if a == 0
//   GOTO INNER_LOOP
// c = 10550400
// d += c
// a = 0
// GOTO INNER_LOOP
