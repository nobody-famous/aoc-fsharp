module Aoc.Year2019.Day4.Part1

type State = { Low: int []; High: int [] }

let createState (range: int [] []) = { Low = range.[0]; High = range.[1] }

let findFirst state =
    let { Low = low } = state

    let rec loop ndx ch =
        if ndx < low.Length then
            let ch' =
                if ch = None && low.[ndx] < low.[ndx - 1] then
                    Some low.[ndx - 1]
                else
                    ch

            if ch'.IsSome then
                low.[ndx] <- ch'.Value

            loop (ndx + 1) ch'

    loop 1 None
    state

let inc state =
    let { Low = low } = state

    let rec loop ndx =
        if ndx >= 0 then
            low.[ndx] <- low.[ndx] + 1

            if low.[ndx] >= 10 then
                low.[ndx] <- 0
                loop (ndx - 1)

    loop (low.Length - 1)
    findFirst state |> ignore

let lessThan (low: int []) (high: int []) =
    let rec loop ndx =
        if ndx >= low.Length || low.[ndx] < high.[ndx] then
            true
        elif low.[ndx] > high.[ndx] then
            false
        else
            loop (ndx + 1)

    loop 0

let hasPair (num: int []) =
    let mutable found = false

    for ndx in 1 .. num.Length - 1 do
        found <- found || num.[ndx] = num.[ndx - 1]

    found

let countMatches (state: State) =
    let rec loop count =
        inc state

        if lessThan state.Low state.High then
            loop (
                if hasPair state.Low then
                    (count + 1)
                else
                    count
            )
        else
            count

    loop 1

let run exp fileName =
    Parser.parseInput fileName
    |> createState
    |> findFirst
    |> countMatches
    |> Aoc.Utils.Run.checkResult exp
