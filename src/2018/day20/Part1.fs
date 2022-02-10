module Aoc.Year2018.Day20.Part1

let parseSegment (regex: string) (startNdx: int) = ""

let parseRegex (regex: string) =
    let re = regex.Substring(1, regex.Length - 2)

    $"NOT DONE {re}"

let parse (input: string) =
    input.Split '\n'
    |> Array.toList
    |> List.head
    |> parseRegex

let run (input: string) =
    let regex = parse input

    printfn $"REGEX {regex}"
