module Aoc.Utils.Regex

open System.Text.RegularExpressions

let (|MatchPattern|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None
