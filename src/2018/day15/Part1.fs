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

let HitPoints = 200

let parseLines (lines: string array) =
    let grid = Grid()

    let parseRow y (line: string) =
        Seq.iteri
            (fun x ch ->
                match ch with
                | '.' -> grid.Add({ G.X = x; G.Y = y }, Empty)
                | '#' -> grid.Add({ G.X = x; G.Y = y }, Wall)
                | 'G' -> grid.Add({ G.X = x; G.Y = y }, Goblin(HitPoints))
                | 'E' -> grid.Add({ G.X = x; G.Y = y }, Elf(HitPoints))
                | _ -> failwith $"Invalid character {int ch}")
            line

    Array.iteri parseRow lines
    grid

let parse (input: string) =
    input.Split '\n' |> S.trimIndent |> parseLines

let printGrid (grid: Grid) =
    let (minPt, maxPt) = grid.Keys |> Seq.toList |> G.findBounds

    for y in minPt.Y .. maxPt.Y do
        for x in minPt.X .. maxPt.X do
            match grid.TryGetValue { X = x; Y = y } with
            | true, Empty -> printf "."
            | true, Wall -> printf "#"
            | true, Goblin _ -> printf "G"
            | true, Elf _ -> printf "E"
            | status, piece -> failwith $"PRINT FAILED {status} {piece}"

        printfn ""

let findUnits (grid: Grid) =
    grid
    |> Seq.filter (fun kv ->
        match kv.Value with
        | Goblin _
        | Elf _ -> true
        | _ -> false)

let readOrder pts =
    Seq.sortBy (fun (a: G.Point) -> (a.Y * 1000) + a.X) pts

let getToAct grid =
    grid
    |> findUnits
    |> Seq.map (fun item -> item.Key)
    |> readOrder
    |> Seq.map (fun pt ->
        match grid.TryGetValue pt with
        | true, v -> (pt, v)
        | _ -> failwith "SHOULD NOT HAPPEN")

let neighborPoints (pt: G.Point) =
    [ { G.X = pt.X - 1; G.Y = pt.Y }
      { G.X = pt.X + 1; G.Y = pt.Y }
      { G.X = pt.X; G.Y = pt.Y - 1 }
      { G.X = pt.X; G.Y = pt.Y + 1 } ]

let getToAttack (grid: Grid) (item: G.Point * Piece) =
    let (itemPt, piece) = item

    neighborPoints itemPt
    |> List.filter (fun pt ->
        match grid.TryGetValue pt with
        | true, Goblin _ ->
            match piece with
            | Elf _ -> true
            | _ -> false
        | true, Elf _ ->
            match piece with
            | Goblin _ -> true
            | _ -> false
        | _ -> false)

let doAttack (grid: Grid) (targets: G.Point seq) =
    let least =
        targets
        |> Seq.map (fun t ->
            match grid.TryGetValue t with
            | true, Goblin hp
            | true, Elf hp -> hp
            | _ -> failwith "doAttach SHOULD NOT BE HERE")
        |> Seq.min

    let opponent =
        targets
        |> Seq.filter (fun t ->
            match grid.TryGetValue t with
            | true, Goblin hp
            | true, Elf hp -> hp = least
            | _ -> failwith "doAttach SHOULD NOT BE HERE")
        |> readOrder
        |> Seq.head

    match grid.TryGetValue opponent with
    | true, Goblin hp ->
        if hp <= 3 then
            grid.[opponent] <- Empty
        else
            grid.[opponent] <- Goblin(hp - 3)
    | true, Elf hp ->
        if hp <= 3 then
            grid.[opponent] <- Empty
        else
            grid.[opponent] <- Elf(hp - 3)
    | _ -> failwith "doAttach SHOULD NOT BE HERE"

let getMoveTargets (grid: Grid) piece =
    grid
    |> Seq.filter (fun (kv: KVP) ->
        match kv.Value with
        | Goblin _ ->
            match piece with
            | Elf _ -> true
            | _ -> false
        | Elf _ ->
            match piece with
            | Goblin _ -> true
            | _ -> false
        | _ -> false)
    |> Seq.map (fun (kv: KVP) -> neighborPoints kv.Key)
    |> Seq.concat
    |> Seq.filter (fun pt ->
        match grid.TryGetValue pt with
        | true, Empty -> true
        | _ -> false)

type DfsState =
    { seen: Set<G.Point>
      path: G.Point list }

let newDfsState () = { seen = Set.empty; path = [] }

let getPaths (grid: Grid) (startPt: G.Point) (endPt: G.Point) =
    let rec walk (pt: G.Point) (state: DfsState) =
        if pt = endPt then
            [ List.rev state.path ]
        else
            neighborPoints pt
            |> List.filter (fun item -> not (state.seen.Contains item))
            |> List.filter (fun item ->
                match grid.TryGetValue item with
                | true, Empty -> true
                | _ -> false)
            |> List.map (fun k ->
                walk
                    k
                    { state with
                        seen = state.seen.Add pt
                        path = k :: state.path })
            |> List.concat

    newDfsState () |> walk startPt

let doMove grid ((pt: G.Point), piece) =
    let keepShortest paths =
        if Seq.isEmpty paths then
            paths
        else
            let shortest =
                Seq.map (fun p -> List.length p) paths |> Seq.min


            Seq.filter (fun p -> List.length p = shortest) paths

    let newPts =
        getMoveTargets grid piece
        |> Seq.map (fun t -> getPaths grid pt t)
        |> Seq.concat
        |> keepShortest
        |> Seq.map (fun p -> List.head p)
        |> readOrder

    if not (Seq.isEmpty newPts) then
        let newPt = Seq.head newPts
        grid.[pt] <- Empty
        grid.[newPt] <- piece

        match getToAttack grid (newPt, piece) with
        | targets when Seq.length targets > 0 -> doAttack grid targets
        | _ -> ()

let doAction grid (item: G.Point * Piece) =
    match getToAttack grid item with
    | targets when Seq.length targets > 0 -> doAttack grid targets
    | _ -> doMove grid item

let combatEnds (grid: Grid) =
    let noGoblins =
        Seq.fold
            (fun acc (kv: KVP) ->
                acc
                && match kv.Value with
                   | Goblin _ -> false
                   | _ -> true)
            true
            grid

    let noElves =
        Seq.fold
            (fun acc (kv: KVP) ->
                acc
                && match kv.Value with
                   | Elf _ -> false
                   | _ -> true)
            true
            grid

    noGoblins || noElves

let round grid =
    let toAct = getToAct grid
    let mutable completed = true

    for item in toAct do
        if combatEnds grid then
            completed <- false

        doAction grid item
    
    completed

let run (input: string) =
    let grid = parse input
    let mutable roundNumber = 0

    while not (combatEnds grid) do
        if round grid then
            roundNumber <- roundNumber + 1

        // printfn $"ROUND {roundNumber}"
        // printGrid grid

        // grid
        // |> Seq.iter (fun (kv: KVP) ->
        //     match kv.Value with
        //     | Goblin _ -> printfn $"({kv.Key.X},{kv.Key.Y}) {kv.Value}"
        //     | Elf _ -> printfn $"({kv.Key.X},{kv.Key.Y}) {kv.Value}"
        //     | _ -> ())

    roundNumber
    * Seq.sumBy
        (fun (kv: KVP) ->
            match kv.Value with
            | Goblin hp -> hp
            | Elf hp -> hp
            | _ -> 0)
        grid
