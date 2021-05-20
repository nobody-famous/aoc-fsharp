module aoc.year2019.intcode

let parseInput fileName =
    let lines =
        utils.parser.readLines fileName
        |> Array.map (fun (line: string) -> line.Split ',' |> Array.map (int))

    lines.[0]

type 'a machine =
    { ip: int
      state: 'a
      debug: bool
      halt: bool
      prog: int array
      inputFn: ('a machine -> (int * 'a machine)) option }

type instruction =
    | Add of int * int * int
    | Mul of int * int * int
    | Inp of int
    | Hlt

let instrLength =
    function
    | Add (_, _, _)
    | Mul (_, _, _) -> 4
    | Inp (_) -> 2
    | Hlt -> 1

let newMachine prog =
    { ip = 0
      state = ()
      halt = false
      debug = false
      prog = Array.copy prog
      inputFn = None }

let newMachineState s prog =
    { ip = 0
      state = s
      halt = false
      debug = false
      prog = Array.copy prog
      inputFn = None }

let setInputFn fn m = { m with inputFn = Some fn }

let setDebug dbg mach = { mach with debug = dbg }

let setAddr addr value mach =
    mach.prog.[addr] <- value
    mach

let getState mach = mach.state

let getAddr addr mach = mach.prog.[addr]

let parseInstr m =
    let instr = m.prog.[m.ip]
    let op = instr % 100

    match op with
    | 1 -> m, Add(m.prog.[m.ip + 1], m.prog.[m.ip + 2], m.prog.[m.ip + 3])
    | 2 -> m, Mul(m.prog.[m.ip + 1], m.prog.[m.ip + 2], m.prog.[m.ip + 3])
    | 3 -> m, Inp(m.prog.[m.ip + 1])
    | 99 -> m, Hlt
    | _ -> raise <| System.Exception $"Unhandled op {op}"

let setInputAddr addr (inp, m) = setAddr addr inp m

let getInput addr m =
    match m.inputFn with
    | None -> raise <| System.Exception "No input function"
    | Some fn -> m |> fn |> setInputAddr addr

let execInstr (mach, instr) =
    match instr with
    | Add (arg1, arg2, addr) -> setAddr addr (mach.prog.[arg1] + mach.prog.[arg2]) mach
    | Mul (arg1, arg2, addr) -> setAddr addr (mach.prog.[arg1] * mach.prog.[arg2]) mach
    | Inp (addr) -> getInput addr mach
    | Hlt -> { mach with halt = true }

let incIP instr mach =
    { mach with
          ip = mach.ip + instrLength instr }

let execute (mach, instr) =
    (mach, instr) |> execInstr |> incIP instr

let debug (mach, instr) =
    if mach.debug then
        match instr with
        | Add (arg1, arg2, addr) -> printfn $"{mach.ip} ADD {mach.prog.[arg1]} {mach.prog.[arg2]} => {addr}"
        | Mul (arg1, arg2, addr) -> printfn $"{mach.ip} MUL {mach.prog.[arg1]} {mach.prog.[arg2]} => {addr}"
        | Inp (addr) -> printfn $"{mach.ip} INP {addr}"
        | Hlt -> printfn $"{mach.ip} HLT"

    (mach, instr)

let step mach = mach |> parseInstr |> debug |> execute
