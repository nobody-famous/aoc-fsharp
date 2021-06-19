module Aoc.Year2018.Day2.Part1

open System.Collections.Generic

let hasCounts (str: string) =
    let seen = new Dictionary<char, int>()

    Seq.iter
        (fun ch ->
            if not <| seen.ContainsKey ch then
                seen.Add(ch, 1)
            else
                seen.[ch] <- seen.[ch] + 1)
        str

    seen
    |> Seq.fold (fun (twos, threes) (KeyValue (k, v)) -> (twos || v = 2, threes || v = 3)) (false, false)

let run fileName =
    let (twos, threes) =
        Parser.parseInput fileName
        |> Array.map hasCounts
        |> Array.fold
            (fun (sumTwos, sumThrees) (twos, threes) ->
                (if twos then sumTwos + 1 else sumTwos),
                (if threes then
                     sumThrees + 1
                 else
                     sumThrees))
            (0, 0)

    twos * threes
