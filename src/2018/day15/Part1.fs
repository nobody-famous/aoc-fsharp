module Aoc.Year2018.Day15.Part1

module G = Aoc.Utils.Geometry
module S = Aoc.Utils.String

type Piece =
    | Empty
    | Wall
    | Goblin of int
    | Elf of int

type Grid = System.Collections.Generic.Dictionary<G.Point, Piece>
type KVP = System.Collections.Generic.KeyValuePair<G.Point, Piece>
type PointSet = System.Collections.Generic.HashSet<G.Point>
type PieceMap = System.Collections.Generic.Dictionary<G.Point, Piece>

type State =
    { Board: PointSet
      Goblins: PieceMap
      Elfs: PieceMap }

let HitPoints = 200

let parseLines (lines: string array) =
    let state =
        { Board = PointSet()
          Goblins = PieceMap()
          Elfs = PieceMap() }

    let parseRow y (line: string) =
        Seq.iteri
            (fun x ch ->
                let pt = { G.X = x; G.Y = y }

                match ch with
                | '.' -> state.Board.Add(pt) |> ignore
                | '#' -> ()
                | 'G' ->
                    state.Board.Add(pt) |> ignore
                    state.Goblins.Add(pt, Goblin(HitPoints))

                | 'E' ->
                    state.Board.Add(pt) |> ignore
                    state.Elfs.Add(pt, Elf(HitPoints))
                | _ -> failwith $"Invalid character {int ch}")
            line

    Array.iteri parseRow lines
    state

let parse (input: string) =
    input.Split '\n' |> S.trimIndent |> parseLines

let printGrid (state: State) =
    let (minPt, maxPt) =
        state.Board |> Seq.toList |> G.findBounds

    for y in minPt.Y .. maxPt.Y do
        for x in minPt.X .. maxPt.X do
            let pt = { G.X = x; G.Y = y }

            if state.Goblins.ContainsKey pt then
                printf "G"
            else if state.Elfs.ContainsKey pt then
                printf "E"
            else if state.Board.Contains pt then
                printf "."
            else
                printf "#"

        printfn ""

let findUnits (grid: Grid) =
    grid
    |> Seq.filter (fun kv ->
        match kv.Value with
        | Goblin _
        | Elf _ -> true
        | _ -> false)

let getUnitGroup state pt =
    if state.Goblins.ContainsKey pt then
        state.Goblins
    else
        state.Elfs

let getOpponentGroup state pt =
    if state.Goblins.ContainsKey pt then
        state.Elfs
    else
        state.Goblins

let readOrder pts =
    Seq.sortBy (fun (a: G.Point) -> (a.Y * 1000) + a.X) pts

// let getToAct grid =
//     grid
//     |> findUnits
//     |> Seq.map (fun item -> item.Key)
//     |> readOrder
//     |> Seq.map (fun pt ->
//         match grid.TryGetValue pt with
//         | true, v -> (pt, v)
//         | _ -> failwith "SHOULD NOT HAPPEN")

let getToAct (state: State) =
    [ Seq.map (fun (kv: KVP) -> kv.Key) state.Elfs
      Seq.map (fun (kv: KVP) -> kv.Key) state.Goblins ]
    |> Seq.concat
    |> readOrder

let neighborPoints (pt: G.Point) =
    [ { G.X = pt.X - 1; G.Y = pt.Y }
      { G.X = pt.X + 1; G.Y = pt.Y }
      { G.X = pt.X; G.Y = pt.Y - 1 }
      { G.X = pt.X; G.Y = pt.Y + 1 } ]

let getToAttack (state: State) (pt: G.Point) =
    let group = getOpponentGroup state pt

    neighborPoints pt
    |> List.filter (fun pt -> group.ContainsKey pt)

let doAttack (state: State) (targets: G.Point seq) =
    let group =
        getOpponentGroup state (Seq.head targets)

    let least =
        targets
        |> Seq.map (fun t ->
            match group.TryGetValue t with
            | true, Goblin hp
            | true, Elf hp -> hp
            | _ -> failwith "doAttack SHOULD NOT BE HERE")
        |> Seq.min

    let opponent =
        targets
        |> Seq.filter (fun t ->
            match group.TryGetValue t with
            | true, Goblin hp
            | true, Elf hp -> hp = least
            | _ -> failwith "doAttack SHOULD NOT BE HERE")
        |> readOrder
        |> Seq.head

    match group.TryGetValue opponent with
    | true, Goblin hp ->
        if hp <= 3 then
            group.Remove opponent |> ignore
        else
            group.[opponent] <- Goblin(hp - 3)
    | true, Elf hp ->
        if hp <= 3 then
            group.Remove opponent |> ignore
        else
            group.[opponent] <- Elf(hp - 3)
    | _ -> failwith "doAttack SHOULD NOT BE HERE"

let isSpaceEmpty (state: State) (pt: G.Point) =
    state.Board.Contains pt
    && not (state.Goblins.ContainsKey pt)
    && not (state.Elfs.ContainsKey pt)

let getMoveTargets (state: State) (pt: G.Point) =
    getOpponentGroup state pt
    |> Seq.map (fun (kv: KVP) -> neighborPoints kv.Key)
    |> Seq.concat
    |> Seq.filter (fun pt -> isSpaceEmpty state pt)

type DfsState =
    { seen: Set<G.Point>
      path: G.Point list }

let newDfsState () = { seen = Set.empty; path = [] }

let getPaths (state: State) (startPt: G.Point) (endPt: G.Point) =
    let mutable shortest = System.Int32.MaxValue

    let rec walk (pt: G.Point) (dfsState: DfsState) =
        if pt = endPt then
            if List.length dfsState.path < shortest then
                shortest <- List.length dfsState.path

            [ List.rev dfsState.path ]
        else if List.length dfsState.path > shortest then
            []
        else
            neighborPoints pt
            |> List.filter (fun item -> not (dfsState.seen.Contains item))
            |> List.filter (fun item -> isSpaceEmpty state item)
            |> List.map (fun k ->
                walk
                    k
                    { dfsState with
                        seen = dfsState.seen.Add pt
                        path = k :: dfsState.path })
            |> List.concat

    newDfsState ()
    |> walk startPt
    |> List.filter (fun path -> List.length path > 0)

let doMove (state: State) (pt: G.Point) =
    let keepShortest paths =
        if Seq.isEmpty paths then
            paths
        else
            let shortest =
                Seq.map (fun p -> List.length p) paths |> Seq.min

            Seq.filter (fun p -> List.length p = shortest) paths

    let newPts =
        getMoveTargets state pt
        |> Seq.map (fun t -> getPaths state pt t)
        |> Seq.concat
        |> keepShortest
        |> Seq.map (fun p -> List.head p)
        |> readOrder

    if not (Seq.isEmpty newPts) then
        let newPt = Seq.head newPts
        let group = getUnitGroup state pt

        printfn $"DO MOVE {pt.X},{pt.Y} -> {newPt.X},{newPt.Y}"

        let piece = group.[pt]

        group.Remove pt |> ignore
        group.[newPt] <- piece

        match getToAttack state newPt with
        | targets when Seq.length targets > 0 -> doAttack state targets
        | _ -> ()

let doAction state (pt: G.Point) =
    match getToAttack state pt with
    | targets when Seq.length targets > 0 -> doAttack state targets
    | _ -> doMove state pt

let combatEnds (state: State) =
    Seq.length state.Elfs = 0
    || Seq.length state.Goblins = 0

let round state =
    let toAct = getToAct state
    let mutable completed = true

    for pt in toAct do
        if combatEnds state then
            completed <- false

        doAction state pt

    completed

let run (input: string) =
    let state = parse input
    let mutable roundNumber = 0

    printGrid state
    round state |> ignore
    printGrid state

    // while not (combatEnds state) do
    //     printfn "BEFORE ROUND"
    //     if round state then
    //         roundNumber <- roundNumber + 1

    //     printGrid state

    // printfn $"ROUND {roundNumber}"
    // printGrid grid

    // grid
    // |> Seq.iter (fun (kv: KVP) ->
    //     match kv.Value with
    //     | Goblin _ -> printfn $"({kv.Key.X},{kv.Key.Y}) {kv.Value}"
    //     | Elf _ -> printfn $"({kv.Key.X},{kv.Key.Y}) {kv.Value}"
    //     | _ -> ())

    let group =
        if Seq.length state.Goblins = 0 then
            state.Elfs
        else
            state.Goblins

    roundNumber
    * Seq.sumBy
        (fun (kv: KVP) ->
            match kv.Value with
            | Goblin hp -> hp
            | Elf hp -> hp
            | _ -> 0)
        group
