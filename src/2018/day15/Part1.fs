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

[<Struct>]
type State =
    { Board: PointSet
      Goblins: PieceMap
      Elves: PieceMap }

let HitPoints = 200

let parseLines (lines: string array) =
    let state =
        { Board = PointSet()
          Goblins = PieceMap()
          Elves = PieceMap() }

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
                    state.Elves.Add(pt, Elf(HitPoints))
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
            else if state.Elves.ContainsKey pt then
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
        state.Elves

let getOpponentGroup state pt =
    if state.Goblins.ContainsKey pt then
        state.Elves
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
    [ Seq.map (fun (kv: KVP) -> kv.Key) state.Elves
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
    let group = getUnitGroup state (Seq.head targets)

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
    && not (state.Elves.ContainsKey pt)

let getMoveTargets (state: State) (pt: G.Point) =
    getOpponentGroup state pt
    |> Seq.map (fun (kv: KVP) -> neighborPoints kv.Key)
    |> Seq.concat
    |> Seq.filter (fun pt -> isSpaceEmpty state pt)

let keepShortest (paths: seq<list<G.Point>>) =
    if Seq.isEmpty paths then
        paths
    else
        let shortest =
            Seq.map (fun p -> List.length p) paths |> Seq.min

        Seq.filter (fun p -> List.length p = shortest) paths

type PathMap = System.Collections.Generic.Dictionary<G.Point, G.Point list list>

[<Struct>]
type DfsState = { seen: Set<G.Point> }

let newDfsState () = { seen = Set.empty }

let getPaths (state: State) (startPt: G.Point) (endPt: G.Point) =
    let pathToEnd = PathMap()

    let candidates dfsState pt =
        neighborPoints pt
        |> List.filter (fun item -> not (dfsState.seen.Contains item))
        |> List.filter (fun item -> isSpaceEmpty state item)

    let rec walk (pt: G.Point) (dfsState: DfsState) =
        if pathToEnd.ContainsKey pt then
            pathToEnd.[pt]
        else if pt = endPt then
            pathToEnd.[endPt] <- [ [ endPt ] ]
            [ [ endPt ] ]
        else
            let paths =
                candidates dfsState pt
                |> List.map (fun c -> walk c { dfsState with seen = dfsState.seen.Add pt })
                |> List.concat
                |> List.toSeq
                |> keepShortest
                |> Seq.map (fun path -> pt :: path)

            if Seq.length paths > 0 then
                pathToEnd.[pt] <- Seq.toList paths

            Seq.toList paths

    newDfsState ()
    |> walk startPt
    |> List.map (fun p -> List.tail p)

let doMove (state: State) (pt: G.Point) =
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
    Seq.length state.Elves = 0
    || Seq.length state.Goblins = 0

let round state =
    let toAct = getToAct state
    let mutable completed = true

    for pt in toAct do
        if combatEnds state then
            completed <- false

        if not (isSpaceEmpty state pt) then
            doAction state pt

    completed

let run (input: string) =
    let state = parse input
    let mutable roundNumber = 0

    // printGrid state
    // round state |> ignore
    // printGrid state

    while not (combatEnds state) do
        // for _ in 1 .. 24 do
        if round state then
            roundNumber <- roundNumber + 1

    printfn $"ROUND {roundNumber}"
    printGrid state

    // printfn "GOBLINS"
    // for kv in state.Goblins do
    //     printfn $" {kv.Key.X},{kv.Key.Y} {kv.Value}"

    // printfn "ELVES"
    // for kv in state.Elves do
    //     printfn $" {kv.Key.X},{kv.Key.Y} {kv.Value}"

    let group =
        if Seq.length state.Goblins = 0 then
            state.Elves
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
