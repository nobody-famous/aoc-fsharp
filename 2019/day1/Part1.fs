module Aoc.Year2019.Day1.Part1

open System.Threading.Tasks

let run fileName =
    Parser.parseInput fileName
    |> Array.map (fun n -> async { return Utils.fuel n } |> Async.StartAsTask)
    |> Array.fold (fun acc (t: Task<int>) -> acc + t.Result) 0
