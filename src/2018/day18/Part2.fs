module Aoc.Year2018.Day18.Part2

module U = Aoc.Year2018.Day18.Utils

type ValueList = System.Collections.Generic.List<int>

let hasCycle (values: ValueList) =
    let mutable power = 1
    let mutable lam = 1
    let mutable t = values.Count - 1
    let mutable h = values.Count - 2

    while h >= 0 && values.[t] <> values.[h] do
        if power = lam then
            t <- h
            power <- power * 2
            lam <- 0

        h <- h - 1
        lam <- lam + 1

    if h < 0 then
        false
    else
        t <- values.Count - 1
        h <- values.Count - 1 - lam

        let mutable mu = 0

        while h >= 0 && values.[t] <> values.[h] do
            t <- t - 1
            h <- h - 1
            mu <- mu + 1

        if h < 0 then
            false
        else
            // printfn $"MU {mu} LAMBDA {lam} {t} {h}"
            // for ndx in h - lam .. values.Count - 1 do
            //     printfn $"{ndx} {values.[ndx]}"
            true

let run (input: string) =
    let grid = U.parse input

    let values = ValueList()

    let rec loop count curGrid =
        let value = U.getResourceValue curGrid

        if value = 186706 then printfn $"{count} {value}"
        values.Add value

        if hasCycle values then
            curGrid
        else
            loop (count + 1) (U.runMinute curGrid)
    // hasCycle values |> ignore

    // match count with
    // | 500 -> curGrid
    // | _ -> loop (count + 1) (U.runMinute curGrid)

    grid |> loop 0 |> U.getResourceValue
