module Aoc.Year2018.Day16.Part1

module S = Aoc.Utils.String

type RawInstr = int * int * int * int

type State = { Registers: Map<int, int> }

type Sample =
    { Before: State
      After: State
      Instr: RawInstr }

type Config =
    { Samples: Sample list
      Prog: RawInstr list }

let newState values = { Registers = Map values }

let parseInstr (line: string) =
    let parts = line.Split ' ' |> Array.map int

    RawInstr(parts.[0], parts.[1], parts.[2], parts.[3])

let parseSamples (lines: string array) =
    let parseIntList (line: string) =
        let intList = line.Substring(line.IndexOf '[')

        intList.[1..intList.Length - 2].Split ','
        |> Array.map int
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

let parseProg (lines: string array) ndx =
    if ndx < lines.Length then
        lines
        |> Array.skip ndx
        |> Array.map parseInstr
        |> Array.toList
    else
        List.empty

let parse (input: string) =
    let lines = input.Split '\n' |> S.trimIndent
    let (ndx, samples) = parseSamples lines
    let prog = parseProg lines ndx

    { Samples = samples; Prog = prog }

let addr state instr =
    let (_, a, b, c) = instr

    { state with Registers = state.Registers.Add(c, (state.Registers.[a] + state.Registers.[b])) }

let addi state instr =
    let (_, a, b, c) = instr

    { state with Registers = state.Registers.Add(c, (state.Registers.[a] + b)) }

let mulr state instr =
    let (_, a, b, c) = instr

    { state with Registers = state.Registers.Add(c, (state.Registers.[a] * state.Registers.[b])) }

let muli state instr =
    let (_, a, b, c) = instr

    { state with Registers = state.Registers.Add(c, (state.Registers.[a] * b)) }

let banr state instr =
    let (_, a, b, c) = instr

    { state with Registers = state.Registers.Add(c, (state.Registers.[a] &&& state.Registers.[b])) }

let bani state instr =
    let (_, a, b, c) = instr

    { state with Registers = state.Registers.Add(c, (state.Registers.[a] &&& b)) }

let borr state instr = state
let bori state instr = state
let setr state instr = state
let seti state instr = state
let gtir state instr = state
let gtri state instr = state
let gtrr state instr = state
let eqir state instr = state
let eqri state instr = state
let eqrr state instr = state

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

let countMatches (samples: Sample list) =
    let mutable count = 0

    for sample in samples do
        for fn in fns do
            if (fn (sample.Before) (sample.Instr)) = (sample.After) then
                count <- count + 1

    count

let run (input: string) =
    let config = parse input

    printfn $"COUNT {countMatches config.Samples}"
    0
