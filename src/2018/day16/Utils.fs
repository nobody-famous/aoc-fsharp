module Aoc.Year2018.Day16.Utils

module S = Aoc.Utils.String

type RawInstr = int * int * int * int

type State = Map<int, int>

type Sample =
    { Before: State
      After: State
      Instr: RawInstr }

type Config =
    { Samples: Sample list
      Prog: RawInstr list }

let newState values = Map values

let parseInstr (line: string) =
    try
        let parts =
            line.Split ' '
            |> Array.map (fun p -> p.Trim() |> int)

        RawInstr(parts.[0], parts.[1], parts.[2], parts.[3])
    with
    | ex ->
        printfn $"FAILED {line}"
        raise ex

let parseSamples (lines: string array) =
    let parseIntList (line: string) =
        let intList = line.Substring(line.IndexOf '[')

        intList.[1..intList.Length - 2].Split ','
        |> Array.map (fun p -> p.Trim() |> int)
        |> Array.mapi (fun ndx v -> (ndx, v))
        |> newState

    let rec parseSample ndx samples =
        if ndx >= lines.Length || lines.[ndx].Length = 0 then
            (ndx + 1, samples)
        else
            parseSample
                (ndx + 4)
                ({ Before = parseIntList lines.[ndx]
                   Instr = parseInstr lines.[ndx + 1]
                   After = parseIntList lines.[ndx + 2] }
                 :: samples)

    parseSample 0 []

let skipBlanks ndx (lines: string array) =
    let rec loop n =
        match n with
        | n when lines.[n].Length = 0 -> loop (n + 1)
        | _ -> n

    loop ndx

let parseProg (lines: string array) ndx =
    let afterBlanks = skipBlanks ndx lines

    if afterBlanks < lines.Length then
        lines
        |> Array.skip afterBlanks
        |> Array.map parseInstr
        |> Array.toList
    else
        List.empty

let parse (input: string) =
    let lines = input.Split '\n' |> S.trimIndent
    let (ndx, samples) = parseSamples lines
    let prog = parseProg lines ndx

    { Samples = samples; Prog = prog }

let rr (state: State) (_, a, b, c) op = state.Add(c, op state.[a] state.[b])

let ri (state: State) (_, a, b, c) op = state.Add(c, op state.[a] b)

let ir (state: State) (_, a, b, c) op = state.Add(c, op a state.[b])

let addr state instr = rr state instr (+)
let addi state instr = ri state instr (+)

let mulr state instr = rr state instr (*)
let muli state instr = ri state instr (*)

let banr state instr = rr state instr (&&&)
let bani state instr = ri state instr (&&&)

let borr state instr = rr state instr (|||)
let bori state instr = ri state instr (|||)

let setr (state: State) (_, a, _, c) = state.Add(c, state.[a])
let seti (state: State) (_, a, _, c) = state.Add(c, a)

let gtir (state: State) (_, a, b, c) =
    state.Add(c, (if a > state.[b] then 1 else 0))

let gtri (state: State) (_, a, b, c) =
    state.Add(c, (if state.[a] > b then 1 else 0))

let gtrr (state: State) (_, a, b, c) =
    state.Add(c, (if state.[a] > state.[b] then 1 else 0))

let eqir (state: State) (_, a, b, c) =
    state.Add(c, (if a = state.[b] then 1 else 0))

let eqri (state: State) (_, a, b, c) =
    state.Add(c, (if state.[a] = b then 1 else 0))

let eqrr (state: State) (_, a, b, c) =
    state.Add(c, (if state.[a] = state.[b] then 1 else 0))

let fns =
    [ addr
      addi
      mulr
      muli
      banr
      bani
      borr
      bori
      setr
      seti
      gtir
      gtri
      gtrr
      eqir
      eqri
      eqrr ]
