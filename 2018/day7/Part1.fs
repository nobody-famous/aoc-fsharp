module Aoc.Year2018.Day7.Part1

let run exp fileName =
    let graph = Parser.parseInput fileName

    Seq.toList graph.Tree
    |> Seq.iter (fun (KeyValue (p, kids)) -> printfn $"{p} Kids {List.length kids}")
