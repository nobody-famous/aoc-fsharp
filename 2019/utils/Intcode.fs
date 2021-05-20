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
      inputFn: ('a machine -> (int * 'a machine)) option
      outputFn: (int -> 'a machine -> 'a machine) option }

type instruction =
    | Add of int * int * int
    | Mul of int * int * int
    | Inp of int
    | Out of int
    | Hlt

type mode =
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
    { ip = 0
      state = ()
      halt = false
      debug = false
      prog = Array.copy prog
      inputFn = None
      outputFn = None }

let newMachineState s prog =
    { ip = 0
      state = s
      halt = false
      debug = false
      prog = Array.copy prog
      inputFn = None
      outputFn = None }

let setInputFn fn m = { m with inputFn = Some fn }

let setOutputFn fn m = { m with outputFn = Some fn }

let setDebug dbg mach = { mach with debug = dbg }

let setAddr addr value mach =
    mach.prog.[addr] <- value
    mach

let getState mach = mach.state

let setState s mach = { mach with state = s }

let getAddr addr mach = mach.prog.[addr]

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
    | Position -> m.prog.[m.prog.[m.ip + offset]]
    | Immediate -> m.prog.[m.ip + offset]

let parseInstr m =
    let instr = m.prog.[m.ip]
    let op = instr % 100

    match op with
    | 1 -> m, Add(getArg instr 1 100 m, getArg instr 2 1000 m, m.prog.[m.ip + 3])
    | 2 -> m, Mul(getArg instr 1 100 m, getArg instr 2 1000 m, m.prog.[m.ip + 3])
    | 3 -> m, Inp(m.prog.[m.ip + 1])
    | 4 -> m, Out(getArg instr 1 100 m)
    | 99 -> m, Hlt
    | _ -> raise <| System.Exception $"Unhandled op {op}"

let setInputAddr addr (inp, m) = setAddr addr inp m

let getInput addr m =
    match m.inputFn with
    | None -> raise <| System.Exception "No input function"
    | Some fn -> m |> fn |> setInputAddr addr

let sendOutput value m =
    match m.outputFn with
    | None -> raise <| System.Exception "No output function"
    | Some fn -> fn value m

let execInstr (mach, instr) =
    match instr with
    | Add (arg1, arg2, addr) -> setAddr addr (arg1 + arg2) mach
    | Mul (arg1, arg2, addr) -> setAddr addr (arg1 * arg2) mach
    | Inp (addr) -> getInput addr mach
    | Out (arg1) -> sendOutput arg1 mach
    | Hlt -> { mach with halt = true }

let incIP instr mach =
    { mach with
          ip = mach.ip + instrLength instr }

let execute (mach, instr) =
    (mach, instr) |> execInstr |> incIP instr

let debug (mach, instr) =
    if mach.debug then
        match instr with
        | Add (arg1, arg2, addr) -> printfn $"{mach.ip} ADD {arg1} {arg2} => {addr}"
        | Mul (arg1, arg2, addr) -> printfn $"{mach.ip} MUL {arg1} {arg2} => {addr}"
        | Inp (addr) -> printfn $"{mach.ip} INP {addr}"
        | Out (addr) -> printfn $"{mach.ip} OUT {addr}"
        | Hlt -> printfn $"{mach.ip} HLT"

    (mach, instr)

let step mach = mach |> parseInstr |> debug |> execute
