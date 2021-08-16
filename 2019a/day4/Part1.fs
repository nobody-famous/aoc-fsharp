module Aoc.Year2019.Day4.Part1

let printInput (input: char [] []) = printfn $"{input.[0].[0]}..{input.[1].[0]}"

let run exp fileName =
    Parser.parseInput fileName |> printInput |> ignore
