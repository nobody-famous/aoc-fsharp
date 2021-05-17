module aoc.year2019.day2.utils

open aoc.year2019.intcode

let runMachine mach =
    let rec loop m = if m.halt then m else loop <| step m

    loop mach
