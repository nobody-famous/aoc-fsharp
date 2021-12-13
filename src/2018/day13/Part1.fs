module Aoc.Year2018.Day13.Part1

module Utils = Aoc.Year2018.Day13.Utils

let parse (input: string) =
    input.Split '\n'
    |> Utils.trimIndent
    |> Array.toList
    |> Utils.buildState

let move (state:Utils.State) (cart:Utils.Cart) =
    let mutable newLoc = cart.Loc
    let mutable newDir = cart.Dir
    let mutable newTurn = cart.NextTurn

    newLoc <- match cart.Dir with
                | Utils.North -> {cart.Loc with Utils.Point.Y = cart.Loc.Y - 1}
                | Utils.South -> {cart.Loc with Utils.Point.Y = cart.Loc.Y + 1}
                | Utils.East -> {cart.Loc with Utils.Point.X = cart.Loc.X + 1}
                | Utils.West -> {cart.Loc with Utils.Point.X = cart.Loc.X - 1}
    newDir <- match state.Grid.[newLoc] with
                | Utils.HorizLine
                | Utils.VertLine -> cart.Dir
                | Utils.Turn(Utils.East,Utils.North) -> match cart.Dir with
                                                        | Utils.South -> Utils.East
                                                        | Utils.West -> Utils.North
                                                        | _ -> failwith $"Invalid dir {cart}"
                | Utils.Turn(Utils.West,Utils.North) -> match cart.Dir with
                                                        | Utils.South -> Utils.West
                                                        | Utils.East -> Utils.North
                                                        | _ -> failwith $"Invalid dir {cart}"
                | Utils.Turn(Utils.East,Utils.South) -> match cart.Dir with
                                                        | Utils.North -> Utils.East
                                                        | Utils.West -> Utils.South
                                                        | _ -> failwith $"Invalid dir {cart}"
                | Utils.Turn(Utils.West,Utils.South) -> match cart.Dir with
                                                        | Utils.North -> Utils.West
                                                        | Utils.East -> Utils.South
                                                        | _ -> failwith $"Invalid dir {cart}"
                | Utils.Intersect -> match cart.NextTurn with
                                        |Utils.Left -> (newTurn <- Utils.Straight
                                                        match cart.Dir with
                                                        |Utils.North -> Utils.West
                                                        |Utils.South -> Utils.East
                                                        |Utils.East -> Utils.North
                                                        |Utils.West -> Utils.South)
                                        |Utils.Right -> (newTurn <- Utils.Left
                                                         match cart.Dir with
                                                        |Utils.North -> Utils.East
                                                        |Utils.South -> Utils.West
                                                        |Utils.East -> Utils.South
                                                        |Utils.West -> Utils.North)
                                        |Utils.Straight -> (newTurn<-Utils.Right
                                                            cart.Dir)
                |_ -> failwith $"Unhandled grid location {newLoc}"
    {cart with Loc = newLoc;Dir = newDir;NextTurn=newTurn}

let tick (state:Utils.State) =
    let mutable crash:option<Utils.Point> = None
    let carts = state.Carts
                |> Seq.toList
                |> List.sortBy (fun entry -> entry.Value.Loc.Y * 100 + entry.Value.Loc.X)
                |> List.map (fun entry -> entry.Value)
                |> List.map (fun cart -> move state cart)

    state.Carts.Clear()
    for cart in carts do
        if state.Carts.ContainsKey cart.Loc
            then (crash <- Some(cart.Loc)
                  state.Carts.Remove(cart.Loc) |> ignore)
            else state.Carts.Add(cart.Loc,cart)

    {state with Crash = crash}

let run (input: string) =
    let mutable state = parse input

    while state.Crash.IsNone do
        state <- tick state

    $"{state.Crash.Value.X},{state.Crash.Value.Y}"