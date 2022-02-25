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

    List.append
        opts
        (toolOpts
         |> List.filter (fun m -> canSwitch grid state m.Target.Tool))

let findPath (cfg: U.Config) (grid: U.Grid) =
    let pq = Queue()

    let seen =
        System.Collections.Generic.HashSet<State>()

    let dequeue (q: Queue) =
        let mutable state = startState
        let mutable weight = 0

        q.TryDequeue(&state, &weight) |> ignore
        (state, weight)

    pq.Enqueue(startState, 0)

    while pq.Count > 0 do
        let (state, dist) = dequeue pq
        let moves = getMoves grid state

        seen.Add(state) |> ignore

        moves
        |> List.iter (fun m ->
            if not (seen.Contains(m.Target)) then
                pq.Enqueue(m.Target, m.Weight + dist))

    0

let run (input: string) =
    let cfg = U.parse input
    let grid = U.buildGrid cfg

    U.printGrid grid

    findPath cfg grid

// let ops = moves grid startState

// for op in ops do
//     printfn $"{op.Weight} -> {op.Target.Pos.X},{op.Target.Pos.Y} ({op.Target.Tool})"
