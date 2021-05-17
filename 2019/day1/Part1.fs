module aoc.year2019.day1.part1

open System.Threading.Tasks

let run fileName =
    parser.parseInput fileName
    |> Array.map (fun n -> async { return utils.fuel n } |> Async.StartAsTask)
    |> Array.fold (fun acc (t: Task<int>) -> acc + t.Result) 0
