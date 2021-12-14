module Aoc.Year2018.Day1.Part2

open System.Collections.Generic

let findDupe origNums =
    let rec loop nums total (seen: HashSet<int>) =
        match nums with
        | [] -> loop origNums total seen
        | first :: rest ->
            let newTotal = total + first

            if seen.Contains newTotal then
                newTotal
            else
                (seen.Add newTotal |> ignore
                 loop rest newTotal seen)

    loop origNums 0 (HashSet<int>())

let run (input: string) = Utils.parse input |> findDupe
