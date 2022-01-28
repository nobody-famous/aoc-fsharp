module Aoc.Year2018.Day15.Utils

module G = Aoc.Utils.Geometry
module S = Aoc.Utils.String

type NPC =
    | Goblin of int
    | Elf of int

type Piece =
    | Empty
    | Wall
    | NPC

type KVP = System.Collections.Generic.KeyValuePair<G.Point, NPC>
type PointSet = System.Collections.Generic.HashSet<G.Point>
type NpcMap = System.Collections.Generic.Dictionary<G.Point, NPC>
type EndpointSet = System.Collections.Generic.HashSet<G.Point * G.Point>

let HitPoints = 200

[<Struct>]
type State =
    { Board: PointSet
      Goblins: NpcMap
      Elves: NpcMap
      ElfDamage: int }

let computeAnswer roundNumber (state: State) =
    let group =
        if Seq.isEmpty state.Goblins then
            state.Elves
        else
            state.Goblins

    roundNumber
    * Seq.sumBy
        (fun (kv: KVP) ->
            match kv.Value with
            | Goblin hp -> hp
            | Elf hp -> hp)
        group

let parseLines (lines: string array) =
    let state =
        { Board = PointSet()
          Goblins = NpcMap()
          Elves = NpcMap()
          ElfDamage = 3 }

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

let copyState (state: State) =
    { Board = PointSet(state.Board)
      Goblins = NpcMap(state.Goblins)
      Elves = NpcMap(state.Elves)
      ElfDamage = state.ElfDamage }

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

let hashPoint (pt: G.Point) = (pt.Y * 1000) + pt.X

let getToAct (state: State) =
    Seq.append state.Elves state.Goblins
    |> Seq.map (fun kv -> kv.Key)
    |> Seq.sortBy hashPoint

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

    let opponent =
        targets
        |> Seq.map (fun p ->
            (p,
             match group.[p] with
             | Goblin hp
             | Elf hp -> hp))
        |> Seq.groupBy (fun (_, hp) -> hp)
        |> Seq.minBy (fun (hp, _) -> hp)
        |> snd
        |> Seq.map (fun (p, _) -> p)
        |> Seq.minBy hashPoint

    match group.[opponent] with
    | Goblin hp -> group.[opponent] <- Goblin(hp - state.ElfDamage)
    | Elf hp -> group.[opponent] <- Elf(hp - 3)

    match group.[opponent] with
    | Goblin hp
    | Elf hp ->
        if hp <= 0 then
            group.Remove opponent |> ignore

let isSpaceEmpty (state: State) (pt: G.Point) =
    state.Board.Contains pt
    && not (state.Goblins.ContainsKey pt)
    && not (state.Elves.ContainsKey pt)

let getMoveTargets (state: State) (pt: G.Point) =
    getOpponentGroup state pt
    |> Seq.map (fun (kv: KVP) ->
        neighborPoints kv.Key
        |> Seq.filter (isSpaceEmpty state))
    |> Seq.concat

type Queue = System.Collections.Generic.PriorityQueue<G.Point, int>

let getDistance (state: State) (startPt: G.Point) (endPt: G.Point) =
    let dequeue (pq: Queue) =
        let mutable pt: G.Point = { X = 0; Y = 0 }
        let mutable dist = 0

        match pq.TryDequeue(&pt, &dist) with
        | true -> (pt, dist)
        | false -> failwith "Failed to dequeue next point"

    let djikstra (pt: G.Point) =
        let pq = Queue()
        let seen = PointSet()
        let mutable finalDist = System.Int32.MaxValue

        pq.Enqueue(pt, 0)

        while pq.Count > 0 do
            let (pt, dist) = dequeue pq

            if not (seen.Contains pt) then
                if not (seen.Add pt) then
                    failwith "Failed to add to seen"

                let opts =
                    neighborPoints pt
                    |> List.filter (fun p -> not (seen.Contains p) && isSpaceEmpty state p)

                if pt = endPt then
                    pq.Clear()
                    finalDist <- 0
                else if List.contains endPt opts then
                    pq.Clear()
                    finalDist <- dist + 1
                else
                    List.iter (fun p -> pq.Enqueue(p, dist + 1)) opts

        finalDist

    djikstra startPt

let getTargetDistances (state: State) (startPt: G.Point) (targets: G.Point seq) =
    let distances =
        System.Collections.Generic.Dictionary<G.Point, int>()

    let seen = PointSet()

    let rec walk (toVisit: PointSet) (rem: PointSet) dist =
        toVisit
        |> Seq.iter (fun p ->
            if rem.Contains p then
                distances.[p] <- dist
                rem.Remove(p) |> ignore

            seen.Add p |> ignore)

        if not (Seq.isEmpty toVisit) && rem.Count > 0 then
            let next = PointSet()

            toVisit
            |> Seq.iter (fun p ->
                neighborPoints p
                |> List.filter (fun p -> not (seen.Contains p) && isSpaceEmpty state p)
                |> List.iter (fun p -> next.Add p |> ignore))

            walk next rem (dist + 1)

    let pts = PointSet()
    pts.Add(startPt) |> ignore

    walk pts (PointSet(targets)) 0
    distances |> Seq.map (fun kv -> kv.Key, kv.Value)

let getMovePoint (state: State) (pt: G.Point) =
    let targets =
        getMoveTargets state pt
        |> getTargetDistances state pt

    if Seq.isEmpty targets then
        pt
    else
        let target =
            targets
            |> Seq.groupBy (fun (_, d) -> d)
            |> Seq.minBy (fun (d, _) -> d)
            |> snd
            |> Seq.map (fun (p, _) -> p)
            |> Seq.minBy hashPoint

        neighborPoints pt
        |> List.filter (fun p -> isSpaceEmpty state p)
        |> List.map (fun p -> (p, getDistance state p target))
        |> List.groupBy (fun (_, d) -> d)
        |> List.minBy (fun (d, _) -> d)
        |> snd
        |> List.map (fun (p, _) -> p)
        |> List.minBy hashPoint

let doMove (state: State) (pt: G.Point) =
    let movePt = getMovePoint state pt

    if movePt <> pt then
        let group = getUnitGroup state pt
        let piece = group.[pt]

        group.Remove pt |> ignore
        group.[movePt] <- piece

        let ts = getToAttack state movePt

        if Seq.length ts > 0 then
            doAttack state ts

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
