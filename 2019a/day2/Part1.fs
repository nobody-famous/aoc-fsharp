module Aoc.Year2019.Day2.Part1

type Instruction =
    | Add of int * int * int
    | Mul of int * int * int
    | Hlt

type MachineState =
    { mutable Ip: int
      mutable CurInstr: Instruction option
      Prog: int array
      mutable Debug: bool
      mutable Halted: bool }

let Machine prog =
    { Ip = 0
      CurInstr = None
      Prog = prog
      Debug = false
      Halted = false }

let parseInstr mach =
    let instr = mach.Prog.[mach.Ip]

    mach.CurInstr <-
        (match instr with
         | 1 -> Some(Add(mach.Prog.[mach.Ip + 1], mach.Prog.[mach.Ip + 2], mach.Prog.[mach.Ip + 3]))
         | 2 -> Some(Mul(mach.Prog.[mach.Ip + 1], mach.Prog.[mach.Ip + 2], mach.Prog.[mach.Ip + 3]))
         | 99 -> Some(Hlt)
         | i -> failwith $"Unhandled op {i}")

    mach

let execInstr mach =
    match mach.CurInstr with
    | Some (Add (arg1, arg2, addr)) ->
        mach.Prog.[addr] <- mach.Prog.[arg1] + mach.Prog.[arg2]

        if mach.Debug then
            printfn $"{mach.Ip}: [{addr}] = {mach.Prog.[addr]} ({mach.Prog.[arg1]} + {mach.Prog.[arg2]})"

        mach.Ip <- mach.Ip + 4
    | Some (Mul (arg1, arg2, addr)) ->
        mach.Prog.[addr] <- mach.Prog.[arg1] * mach.Prog.[arg2]

        if mach.Debug then
            printfn $"{mach.Ip}: [{addr}] = {mach.Prog.[addr]} ({mach.Prog.[arg1]} * {mach.Prog.[arg2]})"

        mach.Ip <- mach.Ip + 4
    | Some (Hlt) ->
        mach.Halted <- true

        if mach.Debug then
            printfn $"{mach.Ip}: HLT"

        mach.Ip <- mach.Ip + 1
    | None -> failwith "No current instruction"

    mach

let setDebug debug mach =
    mach.Debug <- debug
    mach

let exec mach = mach |> parseInstr |> execInstr

let rec execAll mach =
    if mach.Halted then
        mach
    else
        exec mach |> execAll

let setPosition pos value mach =
    mach.Prog.[pos] <- value
    mach

let getPosition pos mach = mach.Prog.[pos]

let printState mach = printfn $"{mach}"

let run exp fileName =
    Parser.parseInput fileName
    |> Machine
    |> setDebug false
    |> setPosition 1 12
    |> setPosition 2 2
    |> execAll
    |> getPosition 0
    |> Aoc.Utils.Run.checkResult exp
