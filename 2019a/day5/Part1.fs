module Aoc.Year2019.Day5.Part1

open Aoc.Year2019.Utils.Intcode

type Io(debug) =
    let mutable lastValue = 0

    member _.LastValue() = lastValue

    member val MachIo =
        { Input = (fun () -> 1)
          Output =
              (fun value ->
                  if debug then printfn $"{value}"
                  lastValue <- value) }

let run exp fileName =
    let io = new Io(false)

    parseInput fileName
    |> MachineWithIo(Some(io.MachIo))
    |> execAll
    |> ignore

    Aoc.Utils.Run.checkResult exp <| io.LastValue()
