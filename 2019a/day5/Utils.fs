module Aoc.Year2019.Day5.Utils

open Aoc.Year2019.Utils.Intcode

type Io(id) =
    let mutable lastValue = 0

    member _.LastValue() = lastValue

    member val MachIo =
        { Input = (fun () -> id)
          Output = (fun value -> lastValue <- value) }
