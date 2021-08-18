module Aoc.Year2019.Day5.Part2

open Aoc.Year2019.Utils.Intcode

let run exp fileName =
    let io = new Utils.Io(5)

    parseInput fileName
    |> machineIo io.MachIo
    |> execAll
    |> ignore

    Aoc.Utils.Run.checkResult exp <| io.LastValue()
