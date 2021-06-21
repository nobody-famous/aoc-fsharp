module Aoc.Year2018.Day5.Part1

type ReactRange = { Start: int; End: int }

let isUpperMatch lc ch = System.Char.ToUpper lc = ch
let isLowerMatch uc ch = System.Char.ToLower uc = ch

let willTrigger unit1 unit2 =
    match unit1 with
    | lc when unit1 >= 'a' && unit1 <= 'z' -> isUpperMatch lc unit2
    | uc when unit1 >= 'A' && unit1 <= 'Z' -> isLowerMatch uc unit2
    | _ -> false

let runReaction (polymer: string) =
    Seq.fold
        (fun stack ch ->
            match stack with
            | top :: rest when willTrigger top ch -> rest
            | s -> ch :: s)
        []
        polymer

let run exp fileName =
    Parser.parseInput fileName
    |> runReaction
    |> List.length
    |> Aoc.Utils.Run.checkResult exp
