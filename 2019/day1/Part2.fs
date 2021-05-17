module aoc.year2019.day1.part2

open System.Threading.Tasks

let totalFuel mass =
    let rec loop mass' amount =
        match mass' with
        | _ when mass' <= 0 -> amount
        | m -> loop <| utils.fuel m <| amount + m

    loop (utils.fuel mass) 0

let run fileName =
    parser.parseInput fileName
    |> Array.map (fun n -> async { return totalFuel n } |> Async.StartAsTask)
    |> Array.fold (fun acc (t: Task<int>) -> acc + t.Result) 0
