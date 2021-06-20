module Aoc.Year2018.Day4.Part1

let run exp fileName =
    Parser.parseInput fileName
    |> Array.iter (fun entry -> printfn $"{entry}")

    failwith "Not Done Yet"
