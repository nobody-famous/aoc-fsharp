module Aoc.Year2018.Day16.Part2

module U = Aoc.Year2018.Day16.Utils

let getMatches (sample: U.Sample) (fns: seq<(U.State -> U.RawInstr -> U.State)>) : int list =
    fns
    |> Seq.indexed
    |> Seq.fold
        (fun acc (ndx, fn) ->
            if fn sample.Before sample.Instr = sample.After then
                // if ndx = 2 then
                //     printfn $"MATCHED {ndx} {sample.Instr} {U.stateToString sample.Before} -> {U.stateToString sample.After}"
                ndx :: acc
            else
                acc)
        []


let findOpCodes (samples: U.Sample list) =
    let mutable ss = Set.ofList samples
    let mutable fns = Seq.ofList U.fns
    let mutable fnMap = Map.empty

    while not (Seq.isEmpty fns) do
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

    fnMap

let runProg (prog: list<U.RawInstr>) (opCodeMap: Map<int, (U.State -> U.RawInstr -> U.State)>) =
    let rec exec prog (state: U.State) =
        printfn $"EXEC {state}"

        match prog with
        | [] -> state.[0]
        | instr :: rest ->
            let (op, _, _, _) = instr
            let fn = opCodeMap.[op]
            exec rest (fn state instr)

    exec prog (Map [| (0, 0); (1, 0); (2, 0); (3, 0) |])

let run (input: string) =
    let config = U.parse input
    let opCodeMap = findOpCodes config.Samples

    let fn = opCodeMap.[1]

    let state =
        U.newState [| (0, 1)
                      (1, 0)
                      (2, 0)
                      (3, 3) |]

    let after = U.mulr state (U.RawInstr(1, 0, 2, 2))

    printfn $"AFTER {U.stateToString after}"
    // printfn $"RESULT {runProg config.Prog opCodeMap}"
    0
