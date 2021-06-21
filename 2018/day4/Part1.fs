module Aoc.Year2018.Day4.Part1

open System.Collections.Generic

let highMinuteNdx (minutes: int array) =
    let mutable highNdx = 0

    for ndx in 1 .. Array.length minutes - 1 do
        if minutes.[ndx] > minutes.[highNdx] then
            highNdx <- ndx

    highNdx

let mostMinutesId (minuteMap: Dictionary<int, int array>) =
    minuteMap
    |> Seq.toList
    |> Seq.map (fun (KeyValue (id, minutes)) -> (id, Array.sum minutes))
    |> Seq.fold
        (fun (curId, curHigh) (id, count) ->
            if count > curHigh then
                (id, count)
            else
                (curId, curHigh))
        (0, 0)
    |> fst

let answer id minute = id * minute

let run exp fileName =
    let entries = Parser.parseInput fileName
    let guardId = mostMinutesId entries
    let minute = highMinuteNdx entries.[guardId]

    answer guardId minute
    |> Aoc.Utils.Run.checkResult exp
