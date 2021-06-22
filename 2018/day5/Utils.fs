module Aoc.Year2018.Day5.Utils

let isUpperMatch lc ch = System.Char.ToUpper lc = ch
let isLowerMatch uc ch = System.Char.ToLower uc = ch

let willTrigger unit1 unit2 =
    match unit1 with
    | lc when unit1 >= 'a' && unit1 <= 'z' -> isUpperMatch lc unit2
    | uc when unit1 >= 'A' && unit1 <= 'Z' -> isLowerMatch uc unit2
    | _ -> false
