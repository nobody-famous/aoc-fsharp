module y2018.day1.part2

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

let run (input: string) = utils.parse input |> findDupe
