module Aoc.Year2018.Day7.Part2

open Aoc.Year2018.Day7.Utils

let stepSize = 0
let steps ch = int ch - int 'A' + 1 + stepSize

type WorkItem = { Step: char; Count: int }

let workers : WorkItem array =
    Array.init 5 (fun _ -> { Step = '0'; Count = 0 })

let getNextNodes graph =
    Seq.toList graph.Parents
    |> List.fold
        (fun nodes (KeyValue (ch, count)) ->
            match count with
            | 0 -> ch :: nodes
            | _ -> nodes)
        []

let addWorkItem workers graph item =
    let nextAvailableNdx () =
        let mutable ndx = 0

        for n in 0 .. Array.length workers - 1 do
            if workers.[n].Count < workers.[ndx].Count then
                ndx <- n

        ndx

    match graph.ParentNodes.[item.Step] with
    | [] ->
        let ndx = nextAvailableNdx ()

        workers.[ndx] <-
            { Step = item.Step
              Count = item.Count + workers.[ndx].Count }
    | _ -> printfn "More complicated"

let assignWorkItem graph = addWorkItem workers graph

let assignWork graph nodes =
    if List.length nodes > 5 then
        failwith $"Too many nodes ${List.length nodes}"

    List.sortDescending nodes
    |> List.map (fun n -> { Step = n; Count = steps n })
    |> List.iter (assignWorkItem graph)

    Array.iter (fun item -> printfn $"{item.Step} {item.Count}") workers

let run exp fileName =
    let graph = Parser.parseInput fileName
    let nodes = getNextNodes graph

    assignWork graph nodes

// |> Aoc.Utils.Run.checkResult exp
