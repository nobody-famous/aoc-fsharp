module Aoc.Year2018.Day5.Part1

let isUpperMatch lc ch = System.Char.ToUpper lc = ch
let isLowerMatch uc ch = System.Char.ToLower uc = ch

let willTrigger unit1 unit2 =
    match unit1 with
    | lc when unit1 >= 'a' && unit1 <= 'z' -> isUpperMatch lc unit2
    | uc when unit1 >= 'A' && unit1 <= 'Z' -> isLowerMatch uc unit2
    | _ -> false

let findReactNdx (polymer: string) =
    let rec loop ndx =
        if ndx >= Seq.length polymer - 1 then
            None
        else if willTrigger polymer.[ndx] polymer.[ndx + 1] then
            Some ndx
        else
            loop (ndx + 1)

    loop 0

let expandReaction (polymer: string) ndx =
    let rec loop startNdx endNdx =
        if startNdx >= 0
           && endNdx < Seq.length polymer
           && willTrigger polymer.[startNdx] polymer.[endNdx] then
            loop (startNdx - 1) (endNdx + 1)
        else
            ((startNdx + 1), (endNdx - 1))

    loop ndx (ndx + 1)

let runReaction (polymer: string) =
    let rec loop (str: string) (ndx: int option) =
        match ndx with
        | None -> str
        | Some n ->
            let startNdx, endNdx = expandReaction str n

            printfn $"Reaction {startNdx} {endNdx} {endNdx - startNdx + 1}"
            let newStr =
                str.Remove(startNdx, endNdx - startNdx + 1)

            loop newStr <| findReactNdx newStr

    loop polymer <| findReactNdx polymer

let run exp fileName =
    let line =
        Parser.parseInput fileName |> runReaction

    printfn $"{Seq.length line}"
    failwith "Not Done Yet"
