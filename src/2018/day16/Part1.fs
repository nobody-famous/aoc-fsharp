module Aoc.Year2018.Day16.Part1

module S = Aoc.Utils.String

type RawInstr = int * int * int * int

type Sample =
    { Before: int list
      After: int List
      Instr: RawInstr }

let parseInstr (line: string) =
    let parts = line.Split ' ' |> Array.map int

    RawInstr(parts.[0], parts.[1], parts.[2], parts.[3])

let parseSamples (lines: string array) =
    let parseIntList (line: string) =
        let intList = line.Substring(line.IndexOf '[')

        intList.[1..intList.Length - 2].Split ','
        |> Array.map int
        |> Array.toList

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
        lines |> Array.skip ndx |> Array.map parseInstr
    else
        Array.empty

let parse (input: string) =
    let lines = input.Split '\n' |> S.trimIndent
    let (ndx, samples) = parseSamples lines
    let prog = parseProg lines ndx

    for sample in samples do
        printfn $"{sample}"

    printfn $"PROG {prog}"
    0

let run (input: string) =
    let data = parse input
    0
