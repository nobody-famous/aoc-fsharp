module Aoc.Year2019.Day6.Part2

open System.Collections.Generic

type State(orbits) =
    member _.Orbits = orbits
    member val Paths = Dictionary<string, string list>()

let buildPath start target (state: State) =
    let rec loop curPath object =
        let orbits: Dictionary<string, string list> = state.Orbits

        if object = target then
            Some(List.rev curPath)
        elif orbits.ContainsKey object then
            List.fold
                (fun acc kid ->
                    match acc with
                    | Some _ -> acc
                    | None -> loop (kid :: curPath) kid)
                None
                orbits.[object]
        else
            None

    let path = loop [ start ] start

    if path.IsSome then
        state.Paths.[target] <- path.Value

    state

let findCommonLength path1 path2 =
    let rec loop p1 p2 length =
        match p1, p2 with
        | first1 :: rest1, first2 :: rest2 ->
            if first1 = first2 then
                loop rest1 rest2 (length + 1)
            else
                length
        | _ -> length

    loop path1 path2 0

let countTransfers key1 key2 (state: State) =
    let path1 = state.Paths.[key1]
    let path2 = state.Paths.[key2]
    let common = findCommonLength path1 path2

    let len1 = path1.Length - 1
    let len2 = path2.Length - 1

    len1 - common + (len2 - common)

let run exp fileName =
    Parser.parseInput fileName
    |> State
    |> buildPath "COM" "YOU"
    |> buildPath "COM" "SAN"
    |> countTransfers "YOU" "SAN"
    |> Aoc.Utils.Run.checkResult exp
