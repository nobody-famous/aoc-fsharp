module Aoc.Year2018.Day13.Part1

module G = Aoc.Utils.Geometry

let rec runTicks (state: Utils.State) =
    match state.Crash with
    | None -> runTicks (Utils.tick state)
    | Some c -> c

let getAnswer (crash: G.Point) = $"{crash.X},{crash.Y}"

let run (input: string) =
    Utils.parse input |> runTicks |> getAnswer
