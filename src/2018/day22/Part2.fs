module Aoc.Year2018.Day22.Part2

module U = Aoc.Year2018.Day22.Utils
module G = Aoc.Utils.Geometry

type Tool =
    | Torch
    | Climbing
    | Neither

type State = { Pos: G.Point; Tool: Tool }

type Move = { Target: State; Weight: int }

type Queue = System.Collections.Generic.PriorityQueue<State, int>

let startState =
    { Pos = { G.X = 0; G.Y = 0 }
      Tool = Torch }

let canMove (grid: U.Grid) (cur: State) (target: G.Point) =
    let targetRegion = grid.[target]

    match cur.Tool with
    | Torch ->
        match targetRegion.Type with
        | U.Wet -> false
        | _ -> true
    | Climbing ->
        match targetRegion.Type with
        | U.Narrow -> false
        | _ -> true
    | Neither ->
        match targetRegion.Type with
        | U.Rocky -> false
        | _ -> true

let canSwitch (grid: U.Grid) (cur: State) (target: Tool) =
    let region = grid.[cur.Pos]

    match region.Type with
    | U.Rocky ->
        match target with
        | Neither -> false
        | _ -> true
    | U.Wet ->
        match target with
        | Torch -> false
        | _ -> true
    | U.Narrow ->
        match target with
        | Climbing -> false
        | _ -> true

let getMoves (grid: U.Grid) (state: State) =
    let opts =
        [ { Target = { state with Pos = { state.Pos with G.Y = state.Pos.Y - 1 } }
            Weight = 1 }

          { Target = { state with Pos = { state.Pos with G.Y = state.Pos.Y + 1 } }
            Weight = 1 }

          { Target = { state with Pos = { state.Pos with G.X = state.Pos.X + 1 } }
            Weight = 1 }

          { Target = { state with Pos = { state.Pos with G.X = state.Pos.X - 1 } }
            Weight = 1 } ]
        |> List.filter (fun m -> m.Target.Pos.X >= 0 && m.Target.Pos.Y >= 0)
        |> List.filter (fun m -> canMove grid state m.Target.Pos)

    let toolOpts =
        match state.Tool with
        | Torch ->
            [ { Target = { state with Tool = Climbing }
                Weight = 7 }
              { Target = { state with Tool = Neither }
                Weight = 7 } ]
        | Climbing ->
            [ { Target = { state with Tool = Torch }
                Weight = 7 }
              { Target = { state with Tool = Neither }
                Weight = 7 } ]
        | Neither ->
            [ { Target = { state with Tool = Torch }
                Weight = 7 }
              { Target = { state with Tool = Climbing }
                Weight = 7 } ]

    List.concat [ opts
                  (toolOpts
                   |> List.filter (fun m -> canSwitch grid state m.Target.Tool)) ]

let findPath (cfg: U.Config) (grid: U.Grid) =
    let pq = Queue()

    let seen =
        System.Collections.Generic.HashSet<State>()

    let endState = { Pos = cfg.Target; Tool = Torch }

    let dequeue (q: Queue) =
        let mutable state = startState
        let mutable weight = 0

        q.TryDequeue(&state, &weight) |> ignore
        (state, weight)

    pq.Enqueue(startState, 0)

    let rec loop () =
        let (state, dist) = dequeue pq

        if state = endState then
            dist
        else if seen.Contains(state) then
            loop ()
        else
            let moves =
                getMoves grid state
                |> List.filter (fun m -> not (seen.Contains(m.Target)))

            if not (seen.Add(state)) then
                failwith "Failed to add state to seen"

            moves
            |> List.iter (fun m -> pq.Enqueue(m.Target, m.Weight + dist))

            loop ()

    loop ()

let run (input: string) =
    let cfg = U.parse input

    let grid = U.buildGrid cfg

    findPath cfg grid
