module Aoc.Year2019.Intcode

let parseInput fileName =
    let lines =
        Aoc.Utils.Parser.readLines fileName
        |> Array.map (fun (line: string) -> line.Split ',' |> Array.map (int))

    lines.[0]

type 'a Machine =
    { Ip: int
      State: 'a
      Debug: bool
      Halt: bool
      Prog: int array
      InputFn: ('a Machine -> (int * 'a Machine)) option
      OutputFn: (int -> 'a Machine -> 'a Machine) option }

type Instruction =
    | Add of int * int * int
    | Mul of int * int * int
    | Inp of int
    | Out of int
    | Hlt

type Mode =
    | Position
    | Immediate

let instrLength =
    function
    | Add (_, _, _)
    | Mul (_, _, _) -> 4
    | Inp (_)
    | Out (_) -> 2
    | Hlt -> 1

let newMachine prog =
    { Ip = 0
      State = ()
      Halt = false
      Debug = false
      Prog = Array.copy prog
      InputFn = None
      OutputFn = None }

let newMachineState s prog =
    { Ip = 0
      State = s
      Halt = false
      Debug = false
      Prog = Array.copy prog
      InputFn = None
      OutputFn = None }

let setInputFn fn m = { m with InputFn = Some fn }

let setOutputFn fn m = { m with OutputFn = Some fn }

let setDebug dbg mach = { mach with Debug = dbg }

let setAddr addr value mach =
    mach.Prog.[addr] <- value
    mach

let getState mach = mach.State

let setState s mach = { mach with State = s }

let getAddr addr mach = mach.Prog.[addr]

let getMode instr mask =
    let value = (instr / mask) % 10

    match value with
    | 0 -> Position
    | 1 -> Immediate
    | _ ->
        raise
        <| System.Exception $"Unhandled mode {value}"

let getArg instr offset mask m =
    let argMode = getMode instr mask

    match argMode with
    | Position -> m.Prog.[m.Prog.[m.Ip + offset]]
    | Immediate -> m.Prog.[m.Ip + offset]

let parseInstr m =
    let instr = m.Prog.[m.Ip]
    let op = instr % 100

    match op with
    | 1 -> m, Add(getArg instr 1 100 m, getArg instr 2 1000 m, m.Prog.[m.Ip + 3])
    | 2 -> m, Mul(getArg instr 1 100 m, getArg instr 2 1000 m, m.Prog.[m.Ip + 3])
    | 3 -> m, Inp(m.Prog.[m.Ip + 1])
    | 4 -> m, Out(getArg instr 1 100 m)
    | 99 -> m, Hlt
    | _ -> raise <| System.Exception $"Unhandled op {op}"

let setInputAddr addr (inp, m) = setAddr addr inp m

let getInput addr m =
    match m.InputFn with
    | None -> raise <| System.Exception "No input function"
    | Some fn -> m |> fn |> setInputAddr addr

let sendOutput value m =
    match m.OutputFn with
    | None -> raise <| System.Exception "No output function"
    | Some fn -> fn value m

let execInstr (mach, instr) =
    match instr with
    | Add (arg1, arg2, addr) -> setAddr addr (arg1 + arg2) mach
    | Mul (arg1, arg2, addr) -> setAddr addr (arg1 * arg2) mach
    | Inp (addr) -> getInput addr mach
    | Out (arg1) -> sendOutput arg1 mach
    | Hlt -> { mach with Halt = true }

let incIP instr mach =
    { mach with
          Ip = mach.Ip + instrLength instr }

let execute (mach, instr) =
    (mach, instr) |> execInstr |> incIP instr

let debug (mach, instr) =
    if mach.Debug then
        match instr with
        | Add (arg1, arg2, addr) -> printfn $"{mach.Ip} ADD {arg1} {arg2} => {addr}"
        | Mul (arg1, arg2, addr) -> printfn $"{mach.Ip} MUL {arg1} {arg2} => {addr}"
        | Inp (addr) -> printfn $"{mach.Ip} INP {addr}"
        | Out (addr) -> printfn $"{mach.Ip} OUT {addr}"
        | Hlt -> printfn $"{mach.Ip} HLT"

    (mach, instr)

let step mach = mach |> parseInstr |> debug |> execute
