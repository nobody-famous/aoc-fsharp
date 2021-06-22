module Aoc.Year2018.Day5.Part2

let runReaction ignore (polymer: char list) =
    Seq.fold
        (fun stack ch ->
            match stack with
            | top :: rest when Utils.willTrigger top ch -> rest
            | s when ch = ignore || Utils.isUpperMatch ignore ch -> s
            | s -> ch :: s)
        []
        polymer

let runAll polymer =
    [ 'a' .. 'z' ]
    |> List.map (fun ch -> async { return runReaction ch polymer })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.map List.length
    |> Array.min

let run exp fileName =
    Parser.parseInput fileName
    |> Seq.toList
    |> runReaction '0' // Run initial rection so that rest are shorter
    |> runAll
    |> Aoc.Utils.Run.checkResult exp
