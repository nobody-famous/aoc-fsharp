module Aoc.Year2018.Day7.Part1

open Aoc.Year2018.Day7.Utils

let getNextNode graph =
    Seq.toList graph.Parents
    |> List.fold
        (fun next (KeyValue (cur, count)) ->
            match count with
            | 0 -> System.Math.Min(int cur, int next) |> char
            | _ -> next)
        'Z'

let getPath (graph: Graph) =
    let rec loop path =
        if graph.Parents.Count = 0 then
            path
        else
            let node = getNextNode graph

            graph.Parents.Remove node |> ignore

            if graph.Tree.ContainsKey node then
                List.iter (fun ch -> graph.Parents.[ch] <- graph.Parents.[ch] - 1) graph.Tree.[node]

            loop (node :: path)

    loop [] |> List.rev

let run exp fileName =
    Parser.parseInput fileName
    |> getPath
    |> System.String.Concat
    |> Aoc.Utils.Run.checkResult exp
