module Aoc.Year2019.Day2.Utils

open Aoc.Year2019.Intcode

let runMachine mach =
    let rec loop m = if m.Halt then m else loop <| step m

    loop mach
