module aoc.year2019.intcode

let parseInput fileName =
    let lines =
        utils.parser.readLines fileName
        |> Array.map (fun (line: string) -> line.Split ',' |> Array.map (int))

    lines.[0]

type machine =
    { ip: int
      debug: bool
      halt: bool
      prog: int array }

type instruction =
    | Add of int * int * int
    | Mul of int * int * int
    | Hlt

let instrLength =
    function
    | Add (_, _, _)
    | Mul (_, _, _) -> 4
    | Hlt -> 1

let newMachine prog =
    { ip = 0
      halt = false
      debug = false
      prog = Array.copy prog }

let setAddr addr value mach =
    mach.prog.[addr] <- value
    mach

let getAddr addr mach = mach.prog.[addr]

let parseInstr m =
    let instr = m.prog.[m.ip]
    let op = instr % 100

    match op with
    | 1 -> m, Add(m.prog.[m.ip + 1], m.prog.[m.ip + 2], m.prog.[m.ip + 3])
    | 2 -> m, Mul(m.prog.[m.ip + 1], m.prog.[m.ip + 2], m.prog.[m.ip + 3])
    | 99 -> m, Hlt
    | _ -> raise <| System.Exception $"Unhandled op {op}"

let execInstr (mach, instr) =
    match instr with
    | Add (arg1, arg2, addr) -> setAddr addr (mach.prog.[arg1] + mach.prog.[arg2]) mach
    | Mul (arg1, arg2, addr) -> setAddr addr (mach.prog.[arg1] * mach.prog.[arg2]) mach
    | Hlt -> { mach with halt = true }

let incIP instr mach =
    let newIP =
        match instr with
        | Add (_, _, _)
        | Mul (_, _, _) -> mach.ip + 4
        | Hlt -> mach.ip + 1

    { mach with ip = newIP }

let execute (mach, instr) =
    (mach, instr) |> execInstr |> incIP instr

let debug (mach, instr) =
    if mach.debug then
        match instr with
        | Add (arg1, arg2, addr) -> printfn $"{mach.ip} ADD {mach.prog.[arg1]} {mach.prog.[arg2]} => {addr}"
        | Mul (arg1, arg2, addr) -> printfn $"{mach.ip} MUL {mach.prog.[arg1]} {mach.prog.[arg2]} => {addr}"
        | Hlt -> printfn $"{mach.ip} HLT"

    (mach, instr)

let step mach = mach |> parseInstr |> debug |> execute
