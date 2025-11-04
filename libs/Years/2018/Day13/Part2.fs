module Aoc.Year2018.Day13.Part2

module Utils = Aoc.Year2018.Day13.Utils

let rec runTicks (state: Utils.State) =
    match state.Carts.Count with
    | count when count > 1 -> runTicks (Utils.tick state)
    | _ -> state

let getAnswer (state: Utils.State) =
    let toString (cart: Utils.Cart) = $"{cart.Loc.X},{cart.Loc.Y}"

    state.Carts.Values
    |> Seq.head
    |> toString

let run (input: string list) =
    Utils.parse input |> runTicks |> getAnswer
