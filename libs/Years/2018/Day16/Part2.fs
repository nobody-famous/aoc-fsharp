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

    while not (Seq.isEmpty fns) do
        let matches =
            ss
            |> Seq.map (fun s ->
                let op, _, _, _ = s.Instr
                op, getMatches s fns)
            |> Seq.filter (fun (_, fnList) -> List.length fnList = 1)
            |> Seq.groupBy (fun (op, _) -> op)
            |> Seq.map (fun (op, fnList) -> op, fnList |> Seq.map (fun (_, f) -> f))
            |> Seq.map (fun (op, fnList) -> op, Set.ofList (List.concat fnList))

        matches
        |> Seq.iter (fun (op, ndxSet) ->
            if Set.count ndxSet <> 1 then
                raise (System.Exception "Too many indices")

            ndxSet
            |> Seq.iter (fun ndx -> fnMap <- fnMap |> Map.add op (Seq.item ndx fns))

            ss <-
                ss
                |> Set.filter (fun s ->
                    let op', _, _, _ = s.Instr
                    op <> op'))

        matches
        |> Seq.map (fun (_, fnList) -> Set.toList fnList)
        |> Seq.toList
        |> List.concat
        |> List.sortDescending
        |> List.iter (fun ndx -> fns <- fns |> Seq.removeAt ndx)

    fnMap

let runProg (prog: list<U.RawInstr>) (opCodeMap: Map<int, (U.State -> U.RawInstr -> U.State)>) =
    let rec exec prog (state: U.State) =
        match prog with
        | [] -> state.[0]
        | instr :: rest ->
            let op, _, _, _ = instr
            let fn = opCodeMap.[op]
            exec rest (fn state instr)

    exec prog (Map [| 0, 0; 1, 0; 2, 0; 3, 0 |])

let run (input: string list) =
    let config = U.parse input
    let opCodeMap = findOpCodes config.Samples

    runProg config.Prog opCodeMap
