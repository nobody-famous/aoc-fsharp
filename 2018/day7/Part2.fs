module Aoc.Year2018.Day7.Part2

open System.Collections.Generic
open Aoc.Year2018.Day7.Utils

let stepSize = 60
let numWorkers = 5
let steps ch = int ch - int 'A' + 1 + stepSize

type WorkItem = { Step: char; Count: int }

type State =
    { Workers: WorkItem array
      Layout: Graph
      Placed: Dictionary<char, int> }

let newState graph =
    { Workers = Array.create numWorkers { Step = '0'; Count = 0 }
      Layout = graph
      Placed = Dictionary<char, int>() }

let getNextNodes graph =
    Seq.toList graph.Parents
    |> List.fold
        (fun nodes (KeyValue (ch, count)) ->
            match count with
            | 0 -> ch :: nodes
            | _ -> nodes)
        []

let addWorkItem state item =
    let workers = state.Workers

    let nextAvailableNdx () =
        let mutable ndx = 0

        for n in 0 .. Array.length workers - 1 do
            if workers.[n].Count < workers.[ndx].Count then
                ndx <- n

        ndx

    let lastTime nodes =
        let node =
            List.maxBy (fun n -> state.Placed.[n]) nodes

        state.Placed.[node]

    let workerForTime time =
        let mutable ndx = None

        for n in 0 .. Array.length workers - 1 do
            if workers.[n].Count <= time then
                ndx <- Some n

        match ndx with
        | None -> nextAvailableNdx ()
        | Some n -> n

    let updateWorker item time ndx =
        let endTime =
            item.Count
            + System.Math.Max(state.Workers.[ndx].Count, time)

        state.Workers.[ndx] <- { Step = item.Step; Count = endTime }
        state.Placed.[item.Step] <- endTime

    match state.Layout.ParentNodes.[item.Step] with
    | [] -> updateWorker item 0 (nextAvailableNdx ())
    | nodes ->
        let time = lastTime nodes
        let ndx = workerForTime time
        updateWorker item time ndx

let removeNodes state nodes =
    List.iter
        (fun node ->
            let graph = state.Layout
            graph.Parents.Remove node |> ignore

            if graph.Tree.ContainsKey node then
                List.iter (fun ch -> graph.Parents.[ch] <- graph.Parents.[ch] - 1) graph.Tree.[node])
        nodes

let assignWork state nodes =
    if List.length nodes > numWorkers then
        failwith $"Too many nodes {List.length nodes}"

    List.sort nodes
    |> List.map (fun n -> { Step = n; Count = steps n })
    |> List.iter (addWorkItem state)

    removeNodes state nodes

let doAllWork state =
    while state.Layout.Parents.Count > 0 do
        getNextNodes state.Layout |> assignWork state

    state

let getWorkers state = state.Workers

let run exp fileName =
    Parser.parseInput fileName
    |> newState
    |> doAllWork
    |> getWorkers
    |> Array.fold (fun high worker -> System.Math.Max(worker.Count, high)) 0
    |> Aoc.Utils.Run.checkResult exp
