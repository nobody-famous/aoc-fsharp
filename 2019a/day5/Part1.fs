module Aoc.Year2019.Day5.Part1

open Aoc.Year2019.Utils.Intcode

let run exp fileName =
    let io = new Utils.Io(1)

    parseInput fileName
    |> machineIo io.MachIo
    |> execAll
    |> ignore

    Aoc.Utils.Run.checkResult exp <| io.LastValue()
