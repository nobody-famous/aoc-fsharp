module Aoc.Year2018.Day5.Part1

let runReaction (polymer: string) =
    Seq.fold
        (fun stack ch ->
            match stack with
            | top :: rest when Utils.willTrigger top ch -> rest
            | s -> ch :: s)
        []
        polymer

let run exp fileName =
    Parser.parseInput fileName
    |> runReaction
    |> List.length
    |> Aoc.Utils.Run.checkResult exp
