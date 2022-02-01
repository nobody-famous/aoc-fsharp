module Aoc.Year2018.Day16.Part2

module U = Aoc.Year2018.Day16.Utils

let getMatches (sample: U.Sample) (fns: seq<(U.State -> U.RawInstr -> U.State)>) : int list =
    fns
    |> Seq.indexed
    |> Seq.fold
        (fun acc (ndx, fn) ->
            if fn sample.Before sample.Instr = sample.After then
                ndx :: acc
            else
                acc)
        []


let findOpCodes (samples: U.Sample list) =
    let mutable ss = Set.ofList samples
    let mutable fns = Seq.ofList U.fns
    let mutable fnMap = Map.empty

    for _ in 1 .. 4 do
        let mutable samplesToRemove = []

        for sample in ss do
            let (opCode, _, _, _) = sample.Instr

            let fnsToRemove =
                System.Collections.Generic.HashSet<int>()

            for fn in fns do
                let matches = getMatches sample fns

                if List.length matches = 1 then
                    fnMap <- Map.add opCode fn fnMap
                    fnsToRemove.Add(List.head matches) |> ignore
                    samplesToRemove <- sample :: samplesToRemove

            if fnsToRemove.Count > 0 then
                for toRemove in fnsToRemove do
                    fns <- Seq.removeAt toRemove fns

        if not (List.isEmpty samplesToRemove) then
            for toRemove in samplesToRemove do
                ss <- Set.remove toRemove ss

    printfn $"DONE {Seq.length fns} {Map.count fnMap}"

let run (input: string) =
    let config = U.parse input

    findOpCodes config.Samples |> ignore
    0
