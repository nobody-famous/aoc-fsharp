module Aoc.Year2019.Utils.Intcode

type ArgMode =
    | Position
    | Immediate

type Arg = { Mode: ArgMode; Value: int }

type Instruction =
    | Add of Arg * Arg * int
    | Mul of Arg * Arg * int
    | Input of int
    | Output of Arg
    | Hlt

type MachineIo =
    { Input: unit -> int
      Output: int -> unit }

type MachineState =
    { Prog: int array
      Io: MachineIo option
      mutable Ip: int
      mutable CurInstr: Instruction option
      mutable Debug: bool
      mutable Halted: bool }

let initState io prog =
    { Ip = 0
      CurInstr = None
      Prog = Array.copy prog
      Io = io
      Debug = false
      Halted = false }

let machineIo io prog = initState(Some io) prog
let machine prog = initState None prog

let parseMode num =
    match num with
    | 0 -> Position
    | 1 -> Immediate
    | m -> failwith $"Unhandled mode {m}"

let parseArg instr shift offset mach =
    { Mode = parseMode ((instr / shift) % 10)
      Value = mach.Prog.[mach.Ip + offset] }

let parseInstr mach =
    let instr = mach.Prog.[mach.Ip]
    let op = instr % 100

    mach.CurInstr <-
        (match op with
         | 1 -> Some(Add(parseArg instr 100 1 mach, parseArg instr 1000 2 mach, mach.Prog.[mach.Ip + 3]))
         | 2 -> Some(Mul(parseArg instr 100 1 mach, parseArg instr 1000 2 mach, mach.Prog.[mach.Ip + 3]))
         | 3 -> Some(Input(mach.Prog.[mach.Ip + 1]))
         | 4 -> Some(Output(parseArg instr 100 1 mach))
         | 99 -> Some(Hlt)
         | i -> failwith $"Unhandled op {i}")

    mach

let argValue arg mach =
    match arg.Mode with
    | Position -> mach.Prog.[arg.Value]
    | Immediate -> arg.Value

let doAdd arg1 arg2 addr mach =
    let value1 = argValue arg1 mach
    let value2 = argValue arg2 mach

    mach.Prog.[addr] <- value1 + value2

    if mach.Debug then
        printfn $"{mach.Ip}: [{addr}] = {mach.Prog.[addr]} ({value1} + {value2})"

    mach.Ip <- mach.Ip + 4

let doMul arg1 arg2 addr mach =
    let value1 = argValue arg1 mach
    let value2 = argValue arg2 mach

    mach.Prog.[addr] <- value1 * value2

    if mach.Debug then
        printfn $"{mach.Ip}: [{addr}] = {mach.Prog.[addr]} ({value1} * {value2})"

    mach.Ip <- mach.Ip + 4

let doHalt mach =
    mach.Halted <- true

    if mach.Debug then
        printfn $"{mach.Ip}: HLT"

    mach.Ip <- mach.Ip + 1

let doInput addr mach =
    match mach.Io with
    | Some io -> mach.Prog.[addr] <- io.Input()
    | None -> failwith $"IO not set"

    if mach.Debug then
        printfn $"{mach.Ip}: [{addr}] = {mach.Prog.[addr]}"

    mach.Ip <- mach.Ip + 2

let doOutput arg mach =
    let value = argValue arg mach

    match mach.Io with
    | Some io -> io.Output value
    | None -> failwith $"IO not set"

    if mach.Debug then
        printfn $"{mach.Ip}: OUTPUT {value}"

    mach.Ip <- mach.Ip + 2

let execInstr mach =
    match mach.CurInstr with
    | Some (Add (arg1, arg2, addr)) -> doAdd arg1 arg2 addr mach
    | Some (Mul (arg1, arg2, addr)) -> doMul arg1 arg2 addr mach
    | Some (Input (addr)) -> doInput addr mach
    | Some (Output (arg)) -> doOutput arg mach
    | Some (Hlt) -> doHalt mach
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
        mach |> exec |> execAll

let setPosition pos value mach =
    mach.Prog.[pos] <- value
    mach

let getPosition pos mach = mach.Prog.[pos]

let printState mach = printfn $"{mach}"

let parseInput fileName =
    let prog =
        Aoc.Utils.Parser.readLines fileName
        |> Array.map (fun (line: string) -> line.Split ',' |> Array.map int)

    prog.[0]
