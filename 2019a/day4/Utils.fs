module Aoc.Year2019.Day4.Utils

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

let countMatches validate (state: State) =
    let rec loop count =
        inc state

        if lessThan state.Low state.High then
            loop (
                if validate state.Low then
                    (count + 1)
                else
                    count
            )
        else
            count

    loop (if validate state.Low then 1 else 0)
