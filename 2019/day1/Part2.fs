module Aoc.Year2019.Day1.Part2

open System.Threading.Tasks

let totalFuel mass =
    let rec loop mass' amount =
        match mass' with
        | _ when mass' <= 0 -> amount
        | m -> loop <| Utils.fuel m <| amount + m

    loop (Utils.fuel mass) 0

let run fileName =
    Parser.parseInput fileName
    |> Array.map (fun n -> async { return totalFuel n } |> Async.StartAsTask)
    |> Array.fold (fun acc (t: Task<int>) -> acc + t.Result) 0
